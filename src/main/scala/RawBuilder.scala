package edu.luc.cs.laufer.cs371.expressions

object RawBuilder extends ExprParser[Any]:
  
  // --- Existing Methods ---
  override def onExpr = identity
  override def onTerm = identity
  override def onNumber = identity
  override def onPlusFactor = identity
  override def onMinusFactor = identity
  override def onParenExpr = identity

  // --- NEW: Imperative Methods ---
  override def onIdent = identity
  override def onAssign = identity
  override def onConditional = identity
  override def onLoop = identity
  override def onBlock = identity

end RawBuilder
