package edu.luc.cs.laufer.cs371.expressions

import scala.io.StdIn
import util.{ Success, Failure }
import org.scalatest.funsuite.AnyFunSuite

import behaviors.*
import TestFixtures.*

object Main:
  def main(args: Array[String]): Unit =
    println("Welcome to the MiniJS REPL! Type 'quit' or 'exit' to stop.")
    
    var keepRunning = true
    while (keepRunning) do
      print("minijs> ")
      val input = StdIn.readLine()
      
      if input == null || input.trim == "quit" || input.trim == "exit" then
        keepRunning = false
      else if input.trim.nonEmpty then
        println(s"You entered: $input")
        
        // Pass the user input into the new top-level `repl` parser
        ASTBuilder.parseAll(ASTBuilder.repl, input) match
          case ASTBuilder.Success(parsedAST, _) =>
            println("The parsed statements are:")
            println(parsedAST) // Prints the raw AST (e.g., Block(Assign(...)))
            
            println("The unparsed statements are:")
            println(unparse(parsedAST)) // Prints the pretty JSON-like formatting
            
          case failure =>
            // If the user types invalid MiniJS code, the parser will catch it here
            println(s"Syntax Error: $failure")

end Main

// --- Existing Tests (Untouched) ---
class Test extends AnyFunSuite:
  test("evaluate(p)") { assert(evaluate(complex1).get == -1) }
  test("size(p)") { assert(size(complex1) == 9) }
  test("height(p)") { assert(height(complex1) == 4) }
  test("evaluate(q)") { assert(evaluate(complex2).get == 0) }
  test("size(q)") { assert(size(complex2) == 10) }
  test("height(q)") { assert(height(complex2) == 5) }
  test("evaluate(bad)") { assert(evaluate(bad).isFailure) }
end Test