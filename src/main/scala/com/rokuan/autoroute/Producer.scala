package com.rokuan.autoroute

/**
  * Created by Christophe on 21/11/2016.
  */
trait Producer[+T] {
  def isEmpty(): Boolean
}

case class +::[T](val head: T, val tail: Producer[T]) extends Producer[T] {
  override def isEmpty(): Boolean = false
}

case object PNil extends Producer[Nothing] {
  override def isEmpty(): Boolean = true
}

object Producer {
  implicit def producterToList[T](p: Producer[T]): List[T] = p match {
    case PNil => Nil
    case head +:: tail => head :: producterToList(tail)
  }
}