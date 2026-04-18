package edu.luc.cs.laufer.cs371.expressions

import util.Try
import Expr.*
import org.json4s.JsonAST.JValue
import org.json4s.JsonDSL.*

object behaviors:

  // --- STEP 1: The Domain of Semantic Values & Mutable Store ---
  sealed trait Value
  case class Num(value: Int) extends Value

  // This is the "memory" map where we will save variables (e.g., x -> Num(3))
  val memory = scala.collection.mutable.Map[String, Num]()

  // --- 1. EVALUATION (Fully Functional Interpreter) ---
  private def evaluateR(e: Expr): Num = e match
    // Math operations updated to unwrap (.value) and wrap (Num(...))
    case Constant(c) => Num(c)
    case UMinus(r)   => Num(-evaluateR(r).value)
    case Plus(l, r)  => Num(evaluateR(l).value + evaluateR(r).value)
    case Minus(l, r) => Num(evaluateR(l).value - evaluateR(r).value)
    case Times(l, r) => Num(evaluateR(l).value * evaluateR(r).value)
    case Div(l, r)   => Num(evaluateR(l).value / evaluateR(r).value)
    case Mod(l, r)   => Num(evaluateR(l).value % evaluateR(r).value)

    // Variables and Assignments
    case Variable(x) => 
      memory.getOrElse(x, throw new NoSuchFieldException(x))
      
    case Assign(Variable(x), r) =>
      val res = evaluateR(r)
      memory(x) = res
      Num(0) // Assignments evaluate to "void" (0)

    // --- Phase 3b: Control Flow ---
    // Blocks: Evaluate all statements, return the last result. Empty blocks return 0.
    case Block(stmts*) =>
      if stmts.isEmpty then Num(0)
      else stmts.foldLeft(Num(0)) { (_, stmt) => evaluateR(stmt) }

    // Loops: While guard is true (non-zero), run body. Returns void (0).
    case Loop(guard, body) =>
      while evaluateR(guard).value != 0 do
        evaluateR(body)
      Num(0)

    // Conditionals: If guard is true (non-zero) run then, else run else.
    case Cond(guard, thenB, elseB) =>
      if evaluateR(guard).value != 0 then 
        evaluateR(thenB)
      else 
        evaluateR(elseB)

  // Top-level evaluator returning Try[Value]
  def evaluate(e: Expr): Try[Value] = Try(evaluateR(e))

  // --- 2. AST METRICS ---
  def size(e: Expr): Int = e match
    case Constant(c)   => 1
    case Variable(_)   => 1
    case UMinus(r)     => 1 + size(r)
    case Plus(l, r)    => 1 + size(l) + size(r)
    case Minus(l, r)   => 1 + size(l) + size(r)
    case Times(l, r)   => 1 + size(l) + size(r)
    case Div(l, r)     => 1 + size(l) + size(r)
    case Mod(l, r)     => 1 + size(l) + size(r)
    case Assign(l, r)  => 1 + size(l) + size(r)
    case Cond(g, t, e) => 1 + size(g) + size(t) + size(e)
    case Loop(g, b)    => 1 + size(g) + size(b)
    case Block(s*)     => 1 + s.map(size).sum

  def height(e: Expr): Int = e match
    case Constant(c)   => 1
    case Variable(_)   => 1
    case UMinus(r)     => 1 + height(r)
    case Plus(l, r)    => 1 + math.max(height(l), height(r))
    case Minus(l, r)   => 1 + math.max(height(l), height(r))
    case Times(l, r)   => 1 + math.max(height(l), height(r))
    case Div(l, r)     => 1 + math.max(height(l), height(r))
    case Mod(l, r)     => 1 + math.max(height(l), height(r))
    case Assign(l, r)  => 1 + math.max(height(l), height(r))
    case Cond(g, t, e) => 1 + math.max(height(g), math.max(height(t), height(e)))
    case Loop(g, b)    => 1 + math.max(height(g), height(b))
    case Block(s*)     => 1 + s.map(height).maxOption.getOrElse(0)

  // --- 3. JSON CONVERTER (Phase 4) ---
  def toJson(e: Expr): JValue = e match
    case Constant(c)   => c
    case Variable(v)   => "Variable" -> v
    case UMinus(r)     => "UMinus" -> toJson(r)
    case Plus(l, r)    => "Plus" -> Seq(toJson(l), toJson(r))
    case Minus(l, r)   => "Minus" -> Seq(toJson(l), toJson(r))
    case Times(l, r)   => "Times" -> Seq(toJson(l), toJson(r))
    case Div(l, r)     => "Div" -> Seq(toJson(l), toJson(r))
    case Mod(l, r)     => "Mod" -> Seq(toJson(l), toJson(r))
    case Assign(l, r)  => "Assign" -> Seq(toJson(l), toJson(r))
    case Loop(g, b)    => "Loop" -> Seq(toJson(g), toJson(b))
    case Cond(g, t, el)=> "Cond" -> Seq(toJson(g), toJson(t), toJson(el))
    case Block(s*)     => "Block" -> s.map(toJson)

  // --- 4. THE UNPARSER (Phase 3) ---
  def unparse(e: Expr, indent: Int = 0): String =
    val in = "  " * indent // Generates the correct number of spaces
    e match
      case Constant(c) => c.toString
      case Variable(v) => v
      case UMinus(r)   => s"-${unparse(r, 0)}"
      case Plus(l, r)  => s"(${unparse(l, 0)} + ${unparse(r, 0)})"
      case Minus(l, r) => s"(${unparse(l, 0)} - ${unparse(r, 0)})"
      case Times(l, r) => s"(${unparse(l, 0)} * ${unparse(r, 0)})"
      case Div(l, r)   => s"(${unparse(l, 0)} / ${unparse(r, 0)})"
      case Mod(l, r)   => s"(${unparse(l, 0)} % ${unparse(r, 0)})"

      case Assign(l, r) => s"${in}${unparse(l, 0)} = ${unparse(r, 0)};"

      case Block(stmts*) =>
        val stmtsStr = stmts.map(s => unparse(s, indent + 1)).mkString("\n")
        if (stmts.isEmpty) s"${in}{\n${in}}" else s"${in}{\n$stmtsStr\n${in}}"

      case Loop(guard, body) =>
        s"${in}while (${unparse(guard, 0)}) ${unparseBlockBody(body, indent)}"

      case Cond(guard, thenB, elseB) =>
        val thenStr = unparseBlockBody(thenB, indent)
        val elseStr = elseB match
          case Block() => "" // No else branch
          case b       => s" else ${unparseBlockBody(b, indent)}"
        s"${in}if (${unparse(guard, 0)}) $thenStr$elseStr"

  // Helper function to format the curly braces exactly like the assignment requires
  private def unparseBlockBody(e: Expr, indent: Int): String = e match
    case Block(stmts*) =>
      val stmtsStr = stmts.map(s => unparse(s, indent + 1)).mkString("\n")
      if (stmts.isEmpty) s"{\n${"  " * indent}}"
      else s"{\n$stmtsStr\n${"  " * indent}}"
    case _ => 
      s"{\n${unparse(e, indent + 1)}\n${"  " * indent}}"

end behaviors