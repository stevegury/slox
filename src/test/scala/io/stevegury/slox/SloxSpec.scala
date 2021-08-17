package io.stevegury.slox

import org.scalatest.funsuite._

class SloxSpec extends AnyFunSuite {
  test("Basic test") {
      val slox = new Slox()
      slox.run("")
  }
}