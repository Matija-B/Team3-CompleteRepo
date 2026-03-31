package edu.luc.cs.laufer.cs371.expressions

object Calculator:

  def processExpr(input: String): Unit =
    println("You entered: " + input)
    // 1. CHANGED: ASTBuilder.expr -> ASTBuilder.repl
    val result = ASTBuilder.parseAll(ASTBuilder.repl, input)
    if result.isEmpty then
      println("This expression could not be parsed")
    else
      import org.json4s.native.JsonMethods.{pretty, render}
      import behaviors.*
      // 2. CHANGED: RawBuilder.expr -> RawBuilder.repl
      val raw = RawBuilder.parseAll(RawBuilder.repl, input).get
      println("The untyped parse tree is: " + raw)
      val expr = result.get
      println("The resulting expression is: " + expr)
      
      // 3. NEW: Show off the Phase 3 pretty-printer!
      println("The unparsed code is:")
      println(unparse(expr))
      
      println("The corresponding JSON structure is:")
      println(pretty(render(toJson(expr))))
      println("It has size " + size(expr) + " and height " + height(expr))
      
      // Note: evaluate might throw an error for un-implemented imperative constructs, 
      // but that is expected based on the assignment instructions.
      println("It evaluates to " + evaluate(expr))

  def main(args: Array[String]): Unit =
    if args.length > 0 then
      processExpr(args mkString " ")
    else
      print("Enter MiniJS statement: ") // Updated prompt
      scala.io.Source.stdin.getLines() foreach:
        line =>
          processExpr(line)
          print("Enter MiniJS statement: ")

end Calculator