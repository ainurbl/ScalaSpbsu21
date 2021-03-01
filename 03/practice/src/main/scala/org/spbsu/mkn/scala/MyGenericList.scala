package org.spbsu.mkn.scala

import org.spbsu.mkn.scala.MyGenericList.undef


sealed trait MyGenericList[+T] {
  def head: T

  def tail: MyGenericList[T]

  def drop(n: Int): MyGenericList[T]

  def take(n: Int): MyGenericList[T]

  def map[W](f: T => W): MyGenericList[W]

  def ::[Y >: T](elem: Y): MyGenericList[Y] = new ::(elem, this)
}

object MyGenericList {
  def undef: Nothing = throw new UnsupportedOperationException("operation is undefined")

  def fromSeq[T](seq: Seq[T]): MyGenericList[T] = seq.foldRight[MyGenericList[T]](MyNil)(_ :: _)

  def size[T](list: MyGenericList[T]): Int =
    foldLeft[T, Int](init = 0)((_, b) => 1 + b)(list)

  def sum[T <: Int](list: MyGenericList[T]): Int = list match {
    case MyNil => undef
    case a => foldLeft[T, Int](0)(_ + _)(a)
  }

  def foldLeft[T, B](init: B)(op: (T, B) => B): MyGenericList[T] => B = {
    case MyNil => init
    case ::(head, tail) => foldLeft(op(head, init))(op)(tail)
  }
}

case object MyNil extends MyGenericList[Nothing] {

  override def head: Nothing = undef

  override def tail: Nothing = undef

  override def drop(n: Int): MyGenericList[Nothing] = if (n != 0) undef else this

  override def take(n: Int): MyGenericList[Nothing] = if (n != 0) undef else this

  override def map[W](f: Nothing => W): MyGenericList[Nothing] = MyNil
}

case class ::[+T](override val head: T, override val tail: MyGenericList[T]) extends MyGenericList[T] {
  override def drop(n: Int): MyGenericList[T] = if (n == 0) this else tail drop n - 1

  override def take(n: Int): MyGenericList[T] = if (n == 0) MyNil else head :: (tail take n - 1)

  override def map[W](f: T => W): MyGenericList[W] = f(head) :: (tail map f)
}