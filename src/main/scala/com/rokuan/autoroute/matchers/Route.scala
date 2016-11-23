package com.rokuan.autoroute.matchers

import com.rokuan.autoroute.Producer

/**
  * Created by Christophe on 23/11/2016.
  */
class Route[T, K, R](val transformers: List[Transformer[T, K, R]]) {
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
