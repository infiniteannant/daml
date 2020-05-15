// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.platform.store.dao.events

import akka.NotUsed
import akka.stream.scaladsl.Source
import com.daml.ledger.participant.state.v1.{Offset, TransactionId}
import com.daml.ledger.api.v1.active_contracts_service.GetActiveContractsResponse
import com.daml.ledger.api.v1.transaction_service.{
  GetFlatTransactionResponse,
  GetTransactionResponse,
  GetTransactionTreesResponse,
  GetTransactionsResponse
}
import com.daml.metrics.{Metrics, Timed}
import com.daml.platform.ApiOffset
import com.daml.platform.store.dao.{DbDispatcher, PaginatingAsyncStream}
import com.daml.platform.store.SimpleSqlAsVectorOf.SimpleSqlAsVectorOf

import scala.concurrent.{ExecutionContext, Future}

/**
  * @param dispatcher Executes the queries prepared by this object
  * @param executionContext Runs transformations on data fetched from the database, including DAML-LF value deserialization
  * @param pageSize The number of events to fetch at a time the database when serving streaming calls
  * @see [[PaginatingAsyncStream]]
  */
private[dao] final class TransactionsReader(
    dispatcher: DbDispatcher,
    pageSize: Int,
    metrics: Metrics,
)(implicit executionContext: ExecutionContext) {

  private val dbMetrics = metrics.daml.index.db

  private def offsetFor(response: GetTransactionsResponse): Offset =
    ApiOffset.assertFromString(response.transactions.head.offset)

  private def offsetFor(response: GetTransactionTreesResponse): Offset =
    ApiOffset.assertFromString(response.transactions.head.offset)

  private def deserializeEvent[E](verbose: Boolean)(entry: EventsTable.Entry[Raw[E]]): Future[E] =
    Future(entry.event.applyDeserialization(verbose))

  private def deserializeEntry[E](verbose: Boolean)(
      entry: EventsTable.Entry[Raw[E]],
  ): Future[EventsTable.Entry[E]] =
    deserializeEvent(verbose)(entry).map(event => entry.copy(event = event))

  def getFlatTransactions(
      startExclusive: Offset,
      endInclusive: Offset,
      filter: FilterRelation,
      verbose: Boolean,
  ): Source[(Offset, GetTransactionsResponse), NotUsed] = {
    val events =
      PaginatingAsyncStream(pageSize) { offset =>
        val query =
          EventsTable
            .preparePagedGetFlatTransactions(
              startExclusive = startExclusive,
              endInclusive = endInclusive,
              filter = filter,
              pageSize = pageSize,
              rowOffset = offset,
            )
            .withFetchSize(Some(pageSize))
        val rawEvents =
          dispatcher.executeSql(dbMetrics.getFlatTransactions) { implicit connection =>
            query.asVectorOf(EventsTable.rawFlatEventParser)
          }
        Timed.future(
          future = rawEvents.flatMap(Future.traverse(_)(deserializeEntry(verbose))),
          timer = dbMetrics.getFlatTransactions.translationTimer,
        )

      }

    groupContiguous(events)(by = _.transactionId)
      .flatMapConcat { events =>
        val response = EventsTable.Entry.toGetTransactionsResponse(events)
        Source(response.map(r => offsetFor(r) -> r))
      }
  }

  def lookupFlatTransactionById(
      transactionId: TransactionId,
      requestingParties: Set[Party],
  ): Future[Option[GetFlatTransactionResponse]] = {
    val query = EventsTable.prepareLookupFlatTransactionById(transactionId, requestingParties)
    dispatcher
      .executeSql(
        databaseMetrics = dbMetrics.lookupFlatTransactionById,
        extraLog = Some(s"tx: $transactionId, parties = ${requestingParties.mkString(", ")}"),
      ) { implicit connection =>
        query.asVectorOf(EventsTable.rawFlatEventParser)
      }
      .flatMap(
        rawEvents =>
          Timed.value(
            timer = dbMetrics.lookupFlatTransactionById.translationTimer,
            value = Future.traverse(rawEvents)(deserializeEntry(verbose = true))
        ))
      .map(EventsTable.Entry.toGetFlatTransactionResponse)
  }

  def getTransactionTrees(
      startExclusive: Offset,
      endInclusive: Offset,
      requestingParties: Set[Party],
      verbose: Boolean,
  ): Source[(Offset, GetTransactionTreesResponse), NotUsed] = {
    val events =
      PaginatingAsyncStream(pageSize) { offset =>
        val query =
          EventsTable
            .preparePagedGetTransactionTrees(
              startExclusive = startExclusive,
              endInclusive = endInclusive,
              requestingParties = requestingParties,
              pageSize = pageSize,
              rowOffset = offset,
            )
            .withFetchSize(Some(pageSize))
        val rawEvents =
          dispatcher.executeSql(dbMetrics.getTransactionTrees) { implicit connection =>
            query.asVectorOf(EventsTable.rawTreeEventParser)
          }
        Timed.future(
          future = rawEvents.flatMap(Future.traverse(_)(deserializeEntry(verbose))),
          timer = dbMetrics.getTransactionTrees.translationTimer,
        )
      }

    groupContiguous(events)(by = _.transactionId)
      .flatMapConcat { events =>
        val response = EventsTable.Entry.toGetTransactionTreesResponse(events)
        Source(response.map(r => offsetFor(r) -> r))
      }
  }

  def lookupTransactionTreeById(
      transactionId: TransactionId,
      requestingParties: Set[Party],
  ): Future[Option[GetTransactionResponse]] = {
    val query = EventsTable.prepareLookupTransactionTreeById(transactionId, requestingParties)
    dispatcher
      .executeSql(
        databaseMetrics = dbMetrics.lookupTransactionTreeById,
        extraLog = Some(s"tx: $transactionId, parties = ${requestingParties.mkString(", ")}"),
      ) { implicit connection =>
        query.asVectorOf(EventsTable.rawTreeEventParser)
      }
      .flatMap(
        rawEvents =>
          Timed.value(
            timer = dbMetrics.lookupTransactionTreeById.translationTimer,
            value = Future.traverse(rawEvents)(deserializeEntry(verbose = true))
        ))
      .map(EventsTable.Entry.toGetTransactionResponse)
  }

  def getActiveContracts(
      activeAt: Offset,
      filter: FilterRelation,
      verbose: Boolean,
  ): Source[GetActiveContractsResponse, NotUsed] = {
    val events =
      PaginatingAsyncStream(pageSize) { offset =>
        val query =
          EventsTable
            .preparePagedGetActiveContracts(
              activeAt = activeAt,
              filter = filter,
              pageSize = pageSize,
              rowOffset = offset,
            )
            .withFetchSize(Some(pageSize))
        val rawEvents =
          dispatcher.executeSql(dbMetrics.getActiveContracts) { implicit connection =>
            query.asVectorOf(EventsTable.rawFlatEventParser)
          }
        Timed.future(
          future = rawEvents.flatMap(Future.traverse(_)(deserializeEntry(verbose))),
          timer = dbMetrics.getActiveContracts.translationTimer,
        )
      }

    groupContiguous(events)(by = _.transactionId)
      .flatMapConcat { events =>
        Source(EventsTable.Entry.toGetActiveContractsResponse(events))
      }
  }

}
