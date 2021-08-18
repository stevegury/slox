package io.stevegury.slox

import org.scalatest._
import flatspec._
import matchers._

class SloxSpec  extends AnyFlatSpec with should.Matchers {
  "Slox" should "evaluate simple expression" in {
      val slox = new Slox()
      slox.run("1 + 2") shouldEqual 3
  }

  "Slox" should "deal with precedence" in {
      val slox = new Slox()
      slox.run("1 + 2 * 3") shouldEqual 7
      slox.run("(1 + 2) * 3") shouldEqual 9
  }

  "Slox" should "deal with invalid code" in {
      val slox = new Slox()
      slox.run("1 + 2 * (3") shouldEqual "Syntax Error"
  }
}
