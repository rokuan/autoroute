package com.rokuan.autoroute.rules

import com.rokuan.autoroute.Producer

/**
  * Created by Christophe on 21/11/2016.
  */
class Grammar[T, K](val rules: List[Rule[T, K]]) {
  def run(p: Producer[K]): T = {
    def internalProduct(rs: List[Rule[T, K]]): T = {
      rs.head.product(p).collect {
        case (result, left) if left.isEmpty() => result
      }.getOrElse(internalProduct(rs.tail))
    }
    internalProduct(rules)
  }
}
