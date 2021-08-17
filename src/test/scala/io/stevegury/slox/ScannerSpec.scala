package io.stevegury.slox

import org.scalatest._
import flatspec._
import matchers._
class ScannerSpec extends AnyFlatSpec with should.Matchers {
  "A scanner" should "scan simple tokens" in {
      val scanner = new Scanner("(){},.")
      val tokens = scanner.scanTockens()
      tokens shouldEqual Seq(
        Token(LEFT_PAREN, "(", null, 1),
        Token(RIGHT_PAREN, ")", null, 1),
        Token(LEFT_BRACE, "{", null, 1),
        Token(RIGHT_BRACE, "}", null, 1),
        Token(COMMA, ",", null, 1),
        Token(DOT, ".", null, 1),
        Token(EOF, "", null, 1),
      )
  }

  "A scanner" should "scan double tokens" in {
      val scanner = new Scanner("(!=)={==}<=")
      val tokens = scanner.scanTockens()
      tokens shouldEqual Seq(
        Token(LEFT_PAREN, "(", null, 1),
        Token(BANG_EQUAL, "!=", null, 1),
        Token(RIGHT_PAREN, ")", null, 1),
        Token(EQUAL, "=", null, 1),
        Token(LEFT_BRACE, "{", null, 1),
        Token(EQUAL_EQUAL, "==", null, 1),
        Token(RIGHT_BRACE, "}", null, 1),
        Token(LESS_EQUAL, "<=", null, 1),
        Token(EOF, "", null, 1),
      )
  }

  "A scanner" should "ignore comments" in {
      val scanner = new Scanner("// this is a comment\n+/-")
      val tokens = scanner.scanTockens()
      tokens shouldEqual Seq(
        Token(PLUS, "+", null, 2),
        Token(SLASH, "/", null, 2),
        Token(MINUS, "-", null, 2),
        Token(EOF, "", null, 2),
      )
  }

  "A scanner" should "scan strings" in {
      val scanner = new Scanner("\"my string\"")
      val tokens = scanner.scanTockens()
      tokens shouldEqual Seq(
        Token(STRING, "\"my string\"", "my string", 1),
        Token(EOF, "", null, 1),
      )
  }

  "A scanner" should "scan number literals" in {
      val scanner = new Scanner("3.1415927")
      val tokens = scanner.scanTockens()
      tokens shouldEqual Seq(
        Token(NUMBER, "3.1415927", 3.1415927, 1),
        Token(EOF, "", null, 1),
      )
  }

  "A scanner" should "scan identifiers" in {
      val scanner = new Scanner("toto while for loop")
      val tokens = scanner.scanTockens()
      tokens shouldEqual Seq(
        Token(IDENTIFIER, "toto", null, 1),
        Token(WHILE, "while", null, 1),
        Token(FOR, "for", null, 1),
        Token(IDENTIFIER, "loop", null, 1),
        Token(EOF, "", null, 1),
      )
  }
}
