package com.rokuan.autoroute.rules

import com.rokuan.autoroute.Producer
import com.rokuan.autoroute.automaton.TokenProduct
import com.rokuan.autoroute.matchers._

import scala.collection.mutable.ListBuffer

/**
  * Created by Christophe on 21/11/2016.
  */
trait Rule[+ProductType <: Any, TerminalType] {
  final def ?[R >: ProductType]: OptionalRule[R, TerminalType] = new OptionalRule[R, TerminalType](this)
  final def +[R >: ProductType] : NonEmptyListRule[R, TerminalType] = new NonEmptyListRule[R, TerminalType](this)
  final def *[R >: ProductType]: ListRule[R, TerminalType] = new ListRule[R, TerminalType](this)
  def ~[L](other: Rule[L, TerminalType]) : NonTerminalRule[TerminalType] = new NonTerminalRule[TerminalType](List(this, other))
}

class NonEmptyListRule[T, K](val underlying: Rule[T, K]) extends Rule[List[T], K] {
  def apply[R](matcher: List[T] => R) = new NonEmptyListConverter[T, K, R](this, matcher)
}

class ListRule[T, K](val underlying: Rule[T, K]) extends Rule[List[T], K] {
  def apply[R](matcher: List[T] => R) = new ListConverter[T, K, R](this, matcher)
}

class OptionalRule[T, K](val underlying: Rule[T, K]) extends Rule[Option[T], K] {
  def apply[R](matcher: Option[T] => R) = new BasicTransformer(this, matcher)
}

class NonTerminalRule[T](val rules: List[Rule[_, T]]) extends Rule[List[Any], T] {
  override def ~[L](other: Rule[L, T]): NonTerminalRule[T] =
    new NonTerminalRule[T](rules :+ other)
  def apply[R](m: List[Any] => R) = new BasicTransformer(this, m)
}

trait TerminalState[T] extends Rule[TokenProduct[T], T] {
  def getElement(): T
  def valueMatches(t: TokenProduct[T]): Boolean
  def apply[R](m: TokenProduct[T] => R) = new TokenConverter[T, R](this, m)
}

class SimpleTerminalState[T](val v: T) extends TerminalState[T] {
  override def valueMatches(t: TokenProduct[T]): Boolean = t.isOfType(v)
  override def getElement(): T = v

  override def equals(obj: scala.Any): Boolean = obj match {
    case s: SimpleTerminalState[T] if s.v == v => true
    case _ => false
  }

  override def hashCode(): Int = v.hashCode()
}

object TerminalState {
  implicit def tokenToTerminal[T](t: T) = new SimpleTerminalState[T](t)
}

object Rule {
  def opt[T, K](rule: Rule[T, K]) = new OptionalRule[T, K](rule)
  def list[T, K](rule: Rule[T, K]) = new NonEmptyListRule[T, K](rule)
  def token[T](t: T) = TerminalState.tokenToTerminal(t)
}