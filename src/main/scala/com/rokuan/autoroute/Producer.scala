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
  implicit def producerToList[T](p: Producer[T]): List[T] = p match {
    case PNil => Nil
    case head +:: tail => head :: producerToList(tail)
  }
  implicit def listToProducer[T](l: List[T]): Producer[T] = l match {
    case Nil => PNil
    case head :: tail => +::(head, listToProducer(tail))
  }
}