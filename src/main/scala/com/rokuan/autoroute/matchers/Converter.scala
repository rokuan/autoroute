package com.rokuan.autoroute.matchers

import com.rokuan.autoroute.automaton.{TokenProduct, TokenSource}
import com.rokuan.autoroute.rules._

/**
  * Created by Christophe on 30/07/2017.
  */
trait Converter[ElementType <: Any, TokenType, +ReturnType] {

}

class TokenConverter[TokenType, ReturnType](val r: TerminalState[TokenType], val convert: TokenProduct[TokenType] => ReturnType)
  extends Converter[TokenProduct[TokenType], TokenType, ReturnType] {
  def getToken(): TokenType = r.getElement()
  def matches(v: TokenProduct[TokenType]) = r.valueMatches(v)
}

class OptionalConverter[EntryType, TokenType, +ReturnType](val r: OptionalRule[EntryType, TokenType], val convert: Option[EntryType] => ReturnType)
  extends Converter[EntryType, TokenType, ReturnType] {

}

class ListConverter[EntryType, TokenType, +ReturnType](val r: ListRule[EntryType, TokenType], val convert: List[EntryType] => ReturnType)
  extends Converter[EntryType, TokenType, ReturnType] {

}

class NonEmptyListConverter[EntryType, TokenType, +ReturnType](val r: NonEmptyListRule[EntryType, TokenType], val convert: List[EntryType] => ReturnType)
  extends Converter[EntryType, TokenType, ReturnType] {

}

class NonTerminalConverter[EntryType, TokenType, ReturnType](val r: NonTerminalRule[TokenType], val convert: List[Any] => ReturnType)
  extends Converter[EntryType, TokenType, ReturnType] {

}

class OrConverter[TokenType, ReturnType](val converters: List[Converter[_ <: Any, TokenType, ReturnType]])
extends Converter[Any, TokenType, ReturnType] {

}
