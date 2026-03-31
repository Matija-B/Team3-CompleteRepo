package edu.luc.cs.laufer.cs371.expressions

/** An initial algebra of arithmetic expressions. */
enum Expr derives CanEqual:
  case Constant(value: Int)
  case UMinus(expr: Expr)
  case Plus(left: Expr, right: Expr)
  case Minus(left: Expr, right: Expr)
  case Times(left: Expr, right: Expr)
  case Div(left: Expr, right: Expr)
  case Mod(left: Expr, right: Expr)

  
  // Variables (e.g., x, y, myVar)
  case Variable(name: String)
  
  // Assignments (e.g., x = 5;)
  case Assign(lhs: Expr, rhs: Expr)
  
  // Conditionals (e.g., if (1) { x = 2; } else { x = 3; })
  case Cond(guard: Expr, thenBranch: Expr, elseBranch: Expr)
  
  // Loops (e.g., while (y) { r = r + x; })
  case Loop(guard: Expr, body: Expr)
  
  // Blocks (e.g., { x = 5; y = 7; })
  // We use `Expr*` (varargs) here so that it prints exactly like your assignment 
  // example: Block(Assign(...), Assign(...)) instead of having a List inside it.
  case Block(statements: Expr*)
