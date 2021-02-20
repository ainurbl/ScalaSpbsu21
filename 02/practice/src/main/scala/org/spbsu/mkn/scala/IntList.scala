package org.spbsu.mkn.scala

import org.spbsu.mkn.scala.IntList._

import scala.annotation.tailrec

sealed trait IntList {
  def head: Int

  def tail: IntList = drop(1)

  def drop(n: Int): IntList

  def take(n: Int): IntList

  def map(f: Int => Int): IntList

  def ::(elem: Int): IntList = new ::(elem, this)
}

object IntList {
  def undef: Nothing = throw new UnsupportedOperationException("operation is undefined")

  def fromSeq(seq: Seq[Int]): IntList = seq.foldRight[IntList](IntNil)((elem: Int, xs: IntList) => elem :: xs)

  def sum(intList: IntList): Int = intList match {
    case IntNil => undef
    case a => foldLeft((calculated: Int, init: Int) => calculated + init)(0)(a)
  }

  def size(intList: IntList): Int = foldLeft((calculated: Int, _: Int) => calculated + 1)(0)(intList)

  @tailrec
  def foldLeft(f: (Int, Int) => Int)(init: Int)(intList: IntList): Int = intList match {
    case IntNil => init
    case x :: xs => foldLeft(f)(f(init, x))(xs)
  }
}

case object IntNil extends IntList {
  override def head: Int = undef

  override def drop(n: Int): IntList = if (n == 0) this else undef

  override def take(n: Int): IntList = if (n == 0) this else undef // possible simplification is take = drop, but is it OK?

  override def map(f: Int => Int): IntList = this
}

case class ::(x: Int, xs: IntList) extends IntList {
  override def head: Int = x

  override def drop(n: Int): IntList = if (n == 0) this else xs drop n - 1

  override def take(n: Int): IntList = if (n == 0) IntNil else x :: (xs take n - 1)

  override def map(f: Int => Int): IntList = f(x) :: (xs map f)
}