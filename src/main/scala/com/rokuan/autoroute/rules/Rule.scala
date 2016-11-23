package com.rokuan.autoroute.rules

import com.rokuan.autoroute.Producer
import com.rokuan.autoroute.matchers.{ListTransformer, OptionalTransformer, SimpleTransformer}

import scala.collection.mutable.ListBuffer

/**
  * Created by Christophe on 21/11/2016.
  */
trait Rule[ProductType <: Any, TerminalType] {
  final def ? : OptionalRule[ProductType, TerminalType] = new OptionalRule[ProductType, TerminalType](this)
  final def + : NonEmptyList[ProductType, TerminalType] = new NonEmptyList[ProductType, TerminalType](this)
  final def * : PossibleEmptyList[ProductType, TerminalType] = new PossibleEmptyList[ProductType, TerminalType](this)
  final def ~(other: Rule[_, TerminalType]) : NonTerminalState[TerminalType] = new NonTerminalState[TerminalType](this :: List(other))
  def product(l: Producer[TerminalType]): Option[(ProductType, Producer[TerminalType])]
}

class NonEmptyList[T, K](val underlying: Rule[T, K]) extends Rule[List[T], K] {
  import Rule.internalProduct

  override def product(l: Producer[K]): Option[(List[T], Producer[K])] = {
    underlying.product(l).map { case (result, tail) =>
      internalProduct(underlying, (new ListBuffer[T]() += result), tail)
    }.getOrElse(None)
  }
}

class PossibleEmptyList[T, K](val underlying: Rule[T, K]) extends Rule[List[T], K] {
  import Rule.internalProduct

  override def product(l: Producer[K]): Option[(List[T], Producer[K])] = {
    internalProduct(underlying, new ListBuffer[T](), l)
  }
}

class OptionalRule[T, K](val underlying: Rule[T, K]) extends Rule[Option[T], K] {
  override def product(l: Producer[K]): Option[(Option[T], Producer[K])] = {
    underlying.product(l).map { case (v, tail) => Some(Some(v), tail) }
      .getOrElse(Some(None, l))
  }

  def apply[R](matcher: Option[T] => R) = new OptionalTransformer(this, matcher)
}

class NonTerminalState[T](val rules: List[Rule[_, T]]) extends Rule[List[_], T] {
  override def product(l: Producer[T]): Option[(List[_], Producer[T])] = {
    def productFold(values: ListBuffer[Any], rs: List[Rule[_, T]], p: Producer[T]): Option[(List[_], Producer[T])] = {
        rs match {
          case Nil => Some(values.toList, p)
          case head :: tail =>
            head.product(p).map {
              case (result, producer) => productFold(values += result, tail, producer)
            }.getOrElse(None)
        }
    }

    productFold(new ListBuffer[Any](), rules, l)
  }

  def apply(matcher: List[Any] => T) = new ListTransformer(this, matcher)
}

class TerminalState[T](val v: T) extends Rule[T, T] {
  import com.rokuan.autoroute.+::

  override def product(l: Producer[T]): Option[(T, Producer[T])] = l match {
    case head +:: tail if v == head => Some(v, tail)
    case _ => None
  }
}

object TerminalState {
  implicit def tokenToTerminal[T](t: T) = new TerminalState[T](t)
}

object Rule {
  def token[T](t: T) = TerminalState.tokenToTerminal(t)

  def internalProduct[T, K](underlying: Rule[T, K], acc: ListBuffer[T], l: Producer[K]): Option[(List[T], Producer[K])] = {
    if(l.isEmpty){
      Some(acc.toList, l)
    } else {
      underlying.product(l).map { case (result, tail) =>
        internalProduct(underlying, acc += result, tail)
      }.getOrElse(Some(acc.toList, l))
    }
  }
}