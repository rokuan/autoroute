package com.rokuan.autoroute.matchers

import com.rokuan.autoroute.rules.{NonTerminalState, OptionalRule, Rule, TerminalState}

/**
  * Created by Christophe on 23/11/2016.
  */
trait Transformer[T, K, R] {
  val rule: Rule[T, K]
  def transform: T => R
  def |[S](transformer: Transformer[S, K, R]) = rule ~ (transformer.rule)
}

class IdentityTransformer[R](val rule: TerminalState[R], val v: R) extends Transformer[R, R, R] {
  override def transform: (R) => R = { _ => v }
}

class SimpleTransformer[K, R](val rule: TerminalState[K], val m: K => R) extends Transformer[K, K, R] {
  override def transform: K => R = m
}

class OptionalTransformer[T, K, R](val rule: OptionalRule[T, K], val m: Option[T] => R) extends Transformer[Option[T], K, R] {
  override def transform: Option[T] => R = m
}

class ListTransformer[K, R](val rule: NonTerminalState[K], m: List[Any] => R) extends Transformer[List[Any], K, R] {
  val r: Rule[List[Any], K] = rule
  override def transform: List[Any] => R = m
}

class MultipleTransformer[T, K, R](val rules: List[Rule[T, K]])