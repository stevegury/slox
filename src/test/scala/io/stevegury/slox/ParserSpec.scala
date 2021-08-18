package io.stevegury.slox

import org.scalatest._
import flatspec._
import matchers._

class ParserSpec extends AnyFlatSpec with should.Matchers {
  "A parser" should "parse simple expression" in {
      val scanner = new Scanner("-1 + 2 / 3")
      val tokens = scanner.scanTockens()
      val parser = new Parser(tokens.toIndexedSeq)
      val ast = parser.parse()
      val expected = Binary(
          Unary(
              Token(MINUS, "-", null, 1),
              Literal(1)
          ),
          Token(PLUS, "+", null, 1),
          Binary(
              Literal(2),
              Token(SLASH, "/", null, 1),
              Literal(3)
          )
      )
      ast shouldEqual expected
  }
}
