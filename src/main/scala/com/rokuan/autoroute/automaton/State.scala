package com.rokuan.autoroute.automaton

import com.rokuan.autoroute.matchers.{BasicTransformer, MultipleTransformer}
import com.rokuan.autoroute.rules.TerminalState

import scala.collection.mutable

/**
  * Created by Christophe on 29/07/2017.
  */
class State[TokenType] {
  protected val nextStates = new mutable.HashMap[TerminalState[TokenType], Set[State[TokenType]]]()

  def getNext(t: TokenType): Set[State[TokenType]] = nextStates.get(t).getOrElse(Set())
  def getNext(): Set[State[TokenType]] = nextStates.values.foldLeft(Set[State[TokenType]]())(_ ++ _)

  def addNext(transition: TerminalState[TokenType], next: State[TokenType]) = {
    nextStates.put(transition, nextStates.get(transition).map { _ + next }.getOrElse(Set(next)))
  }
}