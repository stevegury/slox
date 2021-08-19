package io.stevegury.slox

import org.scalatest._
import flatspec._
import matchers._

class SloxSpec  extends AnyFlatSpec with should.Matchers {
  "Slox" should "evaluate simple expression" in {
      val slox = new Slox()
      slox.run("1 + 2;") shouldEqual 3
  }

  "Slox" should "deal with precedence" in {
      val slox = new Slox()
      slox.run("1 + 2 * 3;") shouldEqual 7
      slox.run("(1 + 2) * 3;") shouldEqual 9
  }

  "Slox" should "declare variables" in {
      val slox = new Slox()
      slox.run("var a = 1; var b = 2; a + b;") shouldEqual 3
  }

  "Slox" should "assign variables" in {
      val slox = new Slox()
      slox.run("var a = 1; var b = 2; a = 11; b = 22; a + b;") shouldEqual 33
  }

  "Slox" should "scope variables" in {
      val slox = new Slox()
      slox.run("var a = 1; { var a = 2; a = 3; } a;") shouldEqual 1
      slox.run("var b = 1; { b = 3; } b;") shouldEqual 3
  }
}
