package org.spbsu.mkn.scala

import scala.util.Random

object TheGame {

  sealed trait GuessResult

  case class Correct(numTries: Int) extends GuessResult

  case class Incorrect(bulls: Int, cows: Int) extends GuessResult

  class RepeatingDigitsException extends RuntimeException

  class WrongNumberLengthException(expected: Int, got: Int) extends RuntimeException

  def generateNumberString(length: Int): String = {
    var str = ""
    for (i <- '0' to '9') str += i
    for (i <- 'A' to 'Z') str += i
    var len = length
    var ret = ""
    while (len > 0) {
      val randomIndex = Random.nextInt(str.length)
      ret += str(randomIndex)
      str = str.filter(_ != ret.last)
      len -= 1
    }
    ret
  }

  private def isUnique(str: String): Boolean = str.length() == str.toSet.size

  def validate(secret: String, userInput: String, numTries: Int = 1): GuessResult = {
    val length = secret.length
    if (length != userInput.length) throw new WrongNumberLengthException(length, userInput.length)
    if (!isUnique(secret)) throw new RepeatingDigitsException()
    if (secret == userInput) return Correct(numTries)
    var (cows, bulls) = (0, 0)
    for (i <- 0 until length) {
      if (secret(i) == userInput(i)) bulls += 1
      if (secret.count(_ == userInput(i)) != 0) cows += 1
    }
    Incorrect(bulls, cows - bulls)
  }

  def main(args: Array[String]): Unit = {
    print("Enter your name: ")
    val name = "Ainur"
    println(s"Hello, $name!")
    println(generateNumberString(1))
  }
}
