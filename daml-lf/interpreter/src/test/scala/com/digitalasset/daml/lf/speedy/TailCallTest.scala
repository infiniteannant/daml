// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.lf.speedy

import com.daml.lf.PureCompiledPackages
import com.daml.lf.data
import com.daml.lf.language.Ast
import com.daml.lf.language.Ast._
import com.daml.lf.speedy.Compiler.FullStackTrace
import com.daml.lf.speedy.SResult._
import com.daml.lf.testing.parser.Implicits._
import com.daml.lf.validation.Validation
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{Matchers, WordSpec}

class TailCallTest extends WordSpec with Matchers with TableDrivenPropertyChecks {

  val pkg: Package =
    p"""
       module F {

         // *Non* tail-recursive definition
         val triangle : (Int64 -> Int64) = \ (x: Int64) ->
           case (EQUAL @Int64 x 0) of
               True -> 0
             | _    -> ADD_INT64 x (F:triangle (SUB_INT64 x 1));

         // Tail-recursive definition, via helper function
         val triangleTR : (Int64 -> Int64) = F:triangleTR_acc 0;

         // Tail-recursive definition using accumulator parameter
         val triangleTR_acc : (Int64 -> Int64 -> Int64) = \ (acc: Int64) (x: Int64) ->
           case (EQUAL @Int64 x 0) of
               True -> acc
             | _    -> F:triangleTR_acc (ADD_INT64 acc x) (SUB_INT64 x 1);

       }
      """

  val small: Int = 5
  val large: Int = 500

  "A *non* tail-recursive definition requires a large env-stack." in {
    val exp = e"F:triangle 100"
    runExprThrows(exp, small) shouldBe true
    runExprThrows(exp, large) shouldBe false
    runExpr(exp, large) shouldBe SValue.SInt64(5050)
  }

  "A tail-recursive definition executes with a small env-stack" in {
    val exp = e"F:triangleTR 100"
    runExprThrows(exp, small) shouldBe false
    runExpr(exp, small) shouldBe SValue.SInt64(5050)
  }

  private def typeAndCompile(pkg: Ast.Package): PureCompiledPackages = {
    val rawPkgs = Map(defaultParserParameters.defaultPackageId -> pkg)
    Validation.checkPackage(rawPkgs, defaultParserParameters.defaultPackageId, pkg)
    data.assertRight(
      PureCompiledPackages(rawPkgs, Compiler.Config.Default.copy(stacktracing = FullStackTrace)))
  }

  val pkgs = typeAndCompile(pkg)

  // Evaluate an expression with a bounded env-stack
  private def runExpr(e: Expr, bound: Int): SValue = {
    val machine = Speedy.Machine.fromPureExpr(pkgs, e, envBound = Some(bound)) // Hard bound on env-stack size
    machine.run() match {
      case SResultFinalValue(v) => v
      case res => throw new RuntimeException(s"runExpr, unexpected result $res")
    }
  }

  // Does evaluation throw an exception?
  private def runExprThrows(e: Expr, bound: Int): Boolean = {
    try {
      runExpr(e, bound)
      false
    } catch {
      case _: Throwable => true
    }
  }

}
