package com.rokuan.autoroute.automaton

/**
  * Created by Christophe on 29/07/2017.
  */
trait TokenSource[TokenType] {
  def query(q: String, types: List[TokenType]): TokenProduct[TokenType]
  def hasNext(t: TokenType): Boolean = hasNext(List(t))
  def hasNext(types: List[TokenType]): Boolean
  def next(types: List[TokenType]): TokenProduct[TokenType]
}
