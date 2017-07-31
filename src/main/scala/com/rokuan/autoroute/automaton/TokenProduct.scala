package com.rokuan.autoroute.automaton

/**
  * Created by Christophe on 29/07/2017.
  */
trait TokenProduct[TokenType] {
  def getTypes(): Set[TokenType]
  def isOfType(t: TokenType): Boolean
}
