package org.spbsu.mkn.scala

import org.spbsu.mkn.scala.IntList._

sealed trait IntList {
  def head: Int

  def tail: IntList

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

  def foldLeft(f: (Int, Int) => Int)(init: Int): IntList => Int = {
    case IntNil => init
    case x :: xs => foldLeft(f)(f(init, x))(xs)
  }
}

case object IntNil extends IntList {
  override def head: Int = undef

  override def drop(n: Int): IntList = if (n == 0) this else undef

  override def take(n: Int): IntList = if (n == 0) this else undef // possible simplification is take = drop, but is it OK?

  override def map(f: Int => Int): IntList = this

  override def tail: IntList = undef
}

case class ::(x: Int, xs: IntList) extends IntList {
  override def head: Int = x

  override def drop(n: Int): IntList = if (n == 0) this else xs drop n - 1

  override def take(n: Int): IntList = if (n == 0) IntNil else x :: (xs take n - 1)

  override def map(f: Int => Int): IntList = f(x) :: (xs map f)

  override def tail: IntList = xs
}