package com.rokuan.autoroute.rules

import com.rokuan.autoroute.Producer
import com.rokuan.autoroute.matchers._

import scala.collection.mutable.ListBuffer

/**
  * Created by Christophe on 21/11/2016.
  */
trait Rule[+ProductType <: Any, TerminalType] {
  final def ?[R >: ProductType]: OptionalRule[R, TerminalType] = new OptionalRule[R, TerminalType](this)
  final def +[R >: ProductType] : NonEmptyList[R, TerminalType] = new NonEmptyList[R, TerminalType](this)
  final def *[R >: ProductType]: PossibleEmptyList[R, TerminalType] = new PossibleEmptyList[R, TerminalType](this)
  def ~[L](other: Rule[L, TerminalType]) : NonTerminalState[TerminalType] = new NonTerminalState[TerminalType](List(this, other))
  def produce(l: Producer[TerminalType]): Option[(ProductType, Producer[TerminalType])]
}

class NonEmptyList[T, K](val underlying: Rule[T, K]) extends Rule[List[T], K] {
  import Rule.internalProduct

  override def produce(l: Producer[K]): Option[(List[T], Producer[K])] = {
    underlying.produce(l).map { case (result, tail) =>
      internalProduct(underlying, (new ListBuffer[T]() += result), tail)
    }.getOrElse(None)
  }

  def apply[R](matcher: List[T] => R) = new BasicTransformer(this, matcher)
}

class PossibleEmptyList[T, K](val underlying: Rule[T, K]) extends Rule[List[T], K] {
  import Rule.internalProduct

  override def produce(l: Producer[K]): Option[(List[T], Producer[K])] = {
    internalProduct(underlying, new ListBuffer[T](), l)
  }

  def apply[R](matcher: List[T] => R) = new BasicTransformer(this, matcher)
}

class OptionalRule[T, K](val underlying: Rule[T, K]) extends Rule[Option[T], K] {
  override def produce(l: Producer[K]): Option[(Option[T], Producer[K])] = {
    underlying.produce(l).map { case (v, tail) => Some(Some(v), tail) }
      .getOrElse(Some(None, l))
  }

  def apply[R](matcher: Option[T] => R) = new BasicTransformer(this, matcher)
}

class NonTerminalState[T](val rules: List[Rule[_, T]]) extends Rule[List[Any], T] {
  override def ~[L](other: Rule[L, T]): NonTerminalState[T] =
    new NonTerminalState[T](rules :+ other)

  override def produce(l: Producer[T]): Option[(List[Any], Producer[T])] = {
    def productFold(values: ListBuffer[Any], rs: List[Rule[_, T]], p: Producer[T]): Option[(List[Any], Producer[T])] = {
      rs match {
        case Nil => Some(values.toList, p)
        case head :: tail =>
          head.produce(p).map {
            case (result, producer) => productFold(values += result, tail, producer)
          }.getOrElse(None)
      }
    }

    productFold(new ListBuffer[Any](), rules, l)
  }

  def apply[R](m: List[Any] => R) = new BasicTransformer(this, m)
}

trait TerminalState[T] extends Rule[T, T] {
  import com.rokuan.autoroute.+::

  override def produce(l: Producer[T]): Option[(T, Producer[T])] = l match {
    case head +:: tail if valueMatches(head) => Some(head, tail)
    case _ => None
  }

  def valueMatches(t: T): Boolean

  def apply[R](m: T => R) = new BasicTransformer(this, m)
}

class SimpleTerminalState[T](val v: T) extends TerminalState[T] {
  override def valueMatches(t: T): Boolean = v == t
}

object TerminalState {
  implicit def tokenToTerminal[T](t: T) = new SimpleTerminalState[T](t)
}

object Rule {
  def opt[T, K](rule: Rule[T, K]) = new OptionalRule[T, K](rule)
  def list[T, K](rule: Rule[T, K]) = new NonEmptyList[T, K](rule)
  def token[T](t: T) = TerminalState.tokenToTerminal(t)

  def internalProduct[T, K](underlying: Rule[T, K], acc: ListBuffer[T], l: Producer[K]): Option[(List[T], Producer[K])] = {
    if(l.isEmpty){
      Some(acc.toList, l)
    } else {
      underlying.produce(l).map { case (result, tail) =>
        internalProduct(underlying, acc += result, tail)
      }.getOrElse(Some(acc.toList, l))
    }
  }
}