package org.spbsu.mkn.scala

import scala.io.StdIn.{readInt, readLine}
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
    if (!isUnique(userInput) || !isUnique(secret)) throw new RepeatingDigitsException()
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
    val name = readLine()
    println(s"Hello, $name!")
    print("Enter length: ")
    var length = readInt()
    while (length < 0 || length > 26 + 10) {
      println("Length must be in [0,36]")
      print("Enter length: ")
      length = readInt()
    }
    val secret = generateNumberString(length)
    println("Secret key is generated! Try to guess!")
    var turn = 1
    var flag = true
    while (flag) {
      print("Enter your attempt: ")
      val userInput = readLine()
      try {
        validate(secret, userInput, turn) match {
          case x: Correct =>
            print(s"Good job! You have won in ${x.numTries} tries")
            flag = false
          case x: Incorrect =>
            println(s"Try again! Cows: ${x.cows}, bulls: ${x.bulls}")
        }
      } catch {
        case _: WrongNumberLengthException => println(s"Wrong length! Expected $length. Try again!")
        case _: RepeatingDigitsException => println(s"You can not enter string with repeating characters! Try again!")
      }
      turn += 1
    }
  }
}
