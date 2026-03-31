package edu.luc.cs.laufer.cs371.expressions

import Expr.*

object ASTBuilder extends ExprParser[Expr]:

  override def onExpr: Expr ~ List[String ~ Expr] => Expr =
    case l ~ reps => reps.foldLeft(l) {
      case (acc, "+" ~ r) => Plus(acc, r)
      case (acc, "-" ~ r) => Minus(acc, r)
      case (acc, _)       => acc // Silences the exhaustivity warning
    }

  override def onTerm: Expr ~ List[String ~ Expr] => Expr =
    case l ~ reps => reps.foldLeft(l) {
      case (acc, "*" ~ r) => Times(acc, r)
      case (acc, "/" ~ r) => Div(acc, r)
      case (acc, "%" ~ r) => Mod(acc, r)
      case (acc, _)       => acc // Silences the exhaustivity warning
    }

  override def onNumber = Constant.apply compose (_.toInt)
  override def onPlusFactor = identity
  override def onMinusFactor = UMinus.apply
  override def onParenExpr = identity

  override def onIdent: String => Expr = Variable.apply

  override def onAssign: String ~ Expr => Expr =
    case name ~ expr => Assign(Variable(name), expr)

  override def onConditional: Expr ~ Expr ~ Option[Expr] => Expr =
    case guard ~ thenBranch ~ Some(elseBranch) => Cond(guard, thenBranch, elseBranch)
    case guard ~ thenBranch ~ None             => Cond(guard, thenBranch, Block())

  override def onLoop: Expr ~ Expr => Expr =
    case guard ~ body => Loop(guard, body)

  override def onBlock: List[Expr] => Expr =
    case stmts => Block(stmts*) // Updated to Scala 3 vararg syntax

end ASTBuilder