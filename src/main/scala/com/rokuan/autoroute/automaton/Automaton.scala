package com.rokuan.autoroute.automaton

import java.util

import com.rokuan.autoroute.matchers.TokenConverter
import com.rokuan.autoroute.rules.{SimpleTerminalState, TerminalState}

/**
  * Created by Christophe on 15/07/2017.
  */
class Automaton[TokenType](protected val source: TokenSource[TokenType]) {
  protected val states = new util.HashMap[TokenType, util.HashSet[State]]()

  def produce(): Unit = {

  }
}

class TerminalAutomaton[TokenType, ReturnType](protected val source: TokenSource, protected val converter: TokenConverter[TokenType, ReturnType]) {
  protected val initialState = new State[TokenType]()
  protected val finalState = new State[TokenType]()
  protected val tokenTransition = converter.r

  def getFirstTransition(): TerminalState[TokenType] = tokenTransition
  def getFirst(): State[TokenType] = initialState
  def getLasts(): Set[State[TokenType]] = Set(finalState)

  protected def buildAutomaton(): Unit = {
    initialState.addNext(tokenTransition, finalState)
  }
}

