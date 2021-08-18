package io.stevegury.slox

import scala.io.Source

object Slox {
  private[this] var hasRuntimeError = false
  private[this] val interpreter = new Interpreter()

  def main(args: Array[String]): Unit = {
    println("Welcome to slox!")
    val slox = new Slox()
    if (args.length > 1) {
      println("Usage: slox <script>")
      System.exit(64)
    } else if (args.length == 1) {
      slox.runFile(args(0))
    } else {
      slox.runPrompt()
    }
  }

  def error(line: Int, message: String): Unit = {
    report(line, "", message)
  }

  def error(token: Token, msg: String): Unit = {
    if (token.typ == EOF) {
        report(token.line, " at end", msg)
    } else {
        report(token.line, s" at '${token.lexeme}'", msg)
    }
  }

  def report(line: Int, where: String, message: String): Unit = {
    println(s"[line $line] Error $where: $message")
  }

  def runtimeError(error: RuntimeError): Unit = {
    println(error.getMessage() + s"\n[line ${error.token.line}]")
    hasRuntimeError = true
  }
}

class Slox {
  import Slox._

  var hasError: Boolean = false

  def runPrompt(): Unit = {
    var done = false
    while (!done) {
      print("> ")
      val line = scala.io.StdIn.readLine()
      if (line == null) {
        done = true
      } else {
        run(line)
        hasError = false
      }
    }
  }
  
    def runFile(path: String): Unit = {
      val src = Source.fromFile(path)
      val txt = src.mkString
      println("RUN FILE: \n" + txt)
      run(txt)
      if (hasError) {
        System.exit(65)
      }
      if (hasRuntimeError) {
        System.exit(70)
      }
    }

  def run(prgm: String): Any = {
    print(s"RUNNING: '$prgm'\n")
    
    val scanner = new Scanner(prgm)
    val tokens = scanner.scanTockens()
    println(s"TOKENS: '${tokens.mkString(", ")}'")

    val parser = new Parser(tokens.toIndexedSeq)
    val expr = parser.parse()

    if (hasError || expr == null) {
      return "Syntax Error"
    }
    println(s"AST: $expr")

    val res = interpreter.interpret(expr)
    println(s"result: ${interpreter.stringify(res)}")
    res
  }
}