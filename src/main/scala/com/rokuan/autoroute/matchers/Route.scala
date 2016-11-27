package com.rokuan.autoroute.matchers

import com.rokuan.autoroute.Producer

case class Route[T, K, R](val transformer: Transformer[T, K, R]) {
  def apply(p: Producer[K]): R =
    transformer.produce(p).collect {
      case (result, left) if left.isEmpty() => result
    }.getOrElse(throw new Exception("No matching found or not all the input has been consumed"))
}
