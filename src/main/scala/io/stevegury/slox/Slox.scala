package io.stevegury.slox

import scala.io.Source

object Slox {
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

  def report(line: Int, where: String, message: String): Unit = {
    println(s"[line $line] Error $where: $message")
  }
}

class Slox {
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
    }

  def run(prgm: String): Unit = {
    print(s"RUNNING: '$prgm'\n")
    val scanner = new Scanner(prgm)
    val tokens = scanner.scanTockens()

    tokens.foreach(println)
  }
}