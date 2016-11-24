package com.rokuan.autoroute.matchers

import com.rokuan.autoroute.Producer

/**
  * Created by Christophe on 23/11/2016.
  */
case class Route[T, K, R](val transformers: List[Transformer[T, K, R]]) {
  def this(transformer: Transformer[T, K, R]) = this(List(transformer))

  def apply(p: Producer[K]): R = {
    def internalApply(tr: List[Transformer[T, K, R]], p: Producer[K]): R = {
      tr match {
        case head :: tail =>
          head.rule.product(p).collect {
            case (result, left) if left.isEmpty() => head.transform(result)
          }.getOrElse(internalApply(tail, p))
        case Nil => throw new Exception("No branch matches the text")
      }
    }
    internalApply(transformers, p)
  }
}

object Route {
  implicit def transformerToRoute[T, K, R](transformer: Transformer[T, K, R]) = new Route(List(transformer))
  implicit def transformerListToRoute[T, K, R](transformers: List[Transformer[T, K, R]]) = new Route(transformers)
  //def route[T, K, R](transformers: List[Transformer[T, K, R]]) = new Route(transformers)
}
