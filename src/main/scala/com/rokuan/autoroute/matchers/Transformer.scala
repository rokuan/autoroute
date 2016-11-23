package com.rokuan.autoroute.matchers

import com.rokuan.autoroute.rules.{NonTerminalState, OptionalRule, Rule, TerminalState}

/**
  * Created by Christophe on 23/11/2016.
  */
trait Transformer[T, K, R] {
  val rule: Rule[T, K]
  def transform: T => R
}

class SimpleTransformer[T](val rule: TerminalState[T], v: T) extends Transformer[T, T, T] {
  override def transform: T => T = { _ => v }
}
class OptionalTransformer[T, K, R](val rule: OptionalRule[T, K], val m: Option[T] => R) extends Transformer[Option[T], K, R] {
  override def transform: Option[T] => R = m
}
class ListTransformer[K, R](val rule: NonTerminalState[K], m: List[Any] => R) extends Transformer[List[Any], K, R] {
  val r: Rule[List[Any], K] = rule
  override def transform: List[Any] => R = m
}
