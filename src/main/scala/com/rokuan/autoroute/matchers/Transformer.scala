package com.rokuan.autoroute.matchers

import com.rokuan.autoroute.Producer
import com.rokuan.autoroute.rules._


/**
  * Created by Christophe on 23/11/2016.
  */
trait Transformer[T <: Any, K, +R] extends Rule[R, K]{
  def |[R1 >: R, S <: Any](transformer: BasicTransformer[S, K, R1]): Transformer[Any, K, R1]
}

class BasicTransformer[T, K, R](val rule: Rule[T, K], val transform: T => R) extends Transformer[T, K, R] {
  override def produce(p: Producer[K]): Option[(R, Producer[K])] =
    rule.produce(p).map {
      case (result, left) => Some(transform(result), left)
    }.getOrElse(None)

  //override def |[S <: Any](transformer: BasicTransformer[S, K, R]): Transformer[Any, K, R] = new MultipleTransformer[K, R](this :: transformer :: Nil)
  override def |[R1 >: R, S <: Any](transformer: BasicTransformer[S, K, R1]): Transformer[Any, K, R1] =
    new MultipleTransformer[K, R1](List(this, transformer))

  def apply[S](matcher: R => S) = new BasicTransformer(this, matcher)
}

class MultipleTransformer[K, R <: Any](val transformers: List[Transformer[_ <: Any, K, R]]) extends Transformer[Any, K, R] {
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

  override def |[R1 >: R, S <: Any](transformer: BasicTransformer[S, K, R1]): Transformer[Any, K, R1] =
    new MultipleTransformer[K, R1](transformers :+ transformer)

  def apply[S](matcher: R => S) = new BasicTransformer(this, matcher)
}