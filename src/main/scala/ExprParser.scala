package edu.luc.cs.laufer.cs371.expressions

import scala.util.parsing.combinator.JavaTokenParsers
import Expr.*

trait ExprParser[Result] extends JavaTokenParsers:

  given [A, B](using CanEqual[A, A], CanEqual[B, B]): CanEqual[A ~ B, A ~ B] = CanEqual.derived

  def repl: Parser[Result] = rep(statement) ^^ onBlock

  // Safety fix: Put specific keywords (conditional/loop) before generic ones (assignment/expr)
  def statement: Parser[Result] = 
    conditional | loop | assignment | block | expr <~ ";"

  def assignment: Parser[Result] = 
    ident ~ ("=" ~> expr) <~ ";" ^^ onAssign

  def conditional: Parser[Result] = 
    "if" ~> "(" ~> expr ~ (")" ~> block) ~ opt("else" ~> block) ^^ onConditional

  def loop: Parser[Result] = 
    "while" ~> "(" ~> expr ~ (")" ~> block) ^^ onLoop

  def block: Parser[Result] = 
    "{" ~> rep(statement) <~ "}" ^^ onBlock

  // --- UPGRADED TO rep() ---
  def expr: Parser[Result] = term ~ rep(("+" | "-") ~ term) ^^ onExpr
  def term: Parser[Result] = factor ~ rep(("*" | "/" | "%") ~ factor) ^^ onTerm

  def factor: Parser[Result] = 
    ident ^^ onIdent
    | wholeNumber ^^ onNumber
    | "+" ~> factor ^^ onPlusFactor
    | "-" ~> factor ^^ onMinusFactor
    | "(" ~> expr <~ ")" ^^ onParenExpr

  // --- UPDATED SIGNATURES TO HANDLE LISTS ---
  def onExpr: Result ~ List[String ~ Result] => Result
  def onTerm: Result ~ List[String ~ Result] => Result
  
  def onNumber: String => Result
  def onPlusFactor: Result => Result
  def onMinusFactor: Result => Result
  def onParenExpr: Result => Result
  def onIdent: String => Result
  def onAssign: String ~ Result => Result
  def onConditional: Result ~ Result ~ Option[Result] => Result
  def onLoop: Result ~ Result => Result
  def onBlock: List[Result] => Result

end ExprParser

