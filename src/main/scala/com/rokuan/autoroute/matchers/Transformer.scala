package com.rokuan.autoroute.matchers

import com.rokuan.autoroute.Producer
import com.rokuan.autoroute.rules._


/**
  * Created by Christophe on 23/11/2016.
  */
trait Transformer[T <: Any, K, R] extends Rule[R, K]{
  def |[S <: Any](transformer: AbstractTransformer[S, K, R]): Transformer[Any, K, R]
}

trait AbstractTransformer[T <: Any, K, R] extends Transformer[T, K, R] {
  val rule: Rule[T, K]
  def transform: T => R

  override def produce(p: Producer[K]): Option[(R, Producer[K])] =
    rule.produce(p).map {
      case (result, left) => Some(transform(result), left)
    }.getOrElse(None)

  override def |[S <: Any](transformer: AbstractTransformer[S, K, R]): Transformer[Any, K, R] = new MultipleTransformer[K, R](this :: transformer :: Nil)
}

class IdentityTransformer[R](val rule: TerminalState[R], val v: R) extends AbstractTransformer[R, R, R] {
  override def transform: (R) => R = { _ => v }
}

class SimpleTransformer[K, R](val rule: TerminalState[K], val m: K => R) extends AbstractTransformer[K, K, R] {
  override def transform: K => R = m
}

class OptionalTransformer[T, K, R](val rule: OptionalRule[T, K], val m: Option[T] => R) extends AbstractTransformer[Option[T], K, R] {
  override def transform: Option[T] => R = m
}

class ListTransformer[K, R](val rule: NonTerminalState[K], m: List[Any] => R) extends AbstractTransformer[List[Any], K, R] {
  val r: Rule[List[Any], K] = rule
  override def transform: List[Any] => R = m
}

class MultipleTransformer[K, R](val transformers: List[Transformer[_ <: Any, K, R]]) extends Transformer[Any, K, R] {
  override def produce(p: Producer[K]): Option[(R, Producer[K])] = {
    def internalProduct(ts: List[Transformer[_ <: Any, K, R]], p: Producer[K]): Option[(R, Producer[K])] = {
      ts match {
        case head :: tail =>
          head.produce(p).map {
            case (result, left) => Some(result, left)
          }.getOrElse(internalProduct(tail, p))
        case Nil => None
      }
    }
    internalProduct(transformers, p)
  }

  override def |[S <: Any](transformer: AbstractTransformer[S, K, R]): Transformer[Any, K, R] =
    new MultipleTransformer[K, R](transformers :+ transformer)
}