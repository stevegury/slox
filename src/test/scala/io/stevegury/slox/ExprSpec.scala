package io.stevegury.slox

import org.scalatest._
import flatspec._
import matchers._

class ExprSpec extends AnyFlatSpec with should.Matchers {
  "An expression" should "be printable" in {
      val tree = Binary(
          Unary(
              Token(MINUS, "-", null, 1),
              Literal(123)
          ),
          Token(STAR, "*", null, 1),
          Grouping(
            Literal(45.67)
          )
      )
      tree.toString() shouldEqual "(* (- 123) (45.67))"
  }
}
