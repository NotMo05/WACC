package wacc.repl
import wacc.front_end._
import parsley.Success
import parsley.Failure

object SimpleREPL {
  def repl(args: Array[String]): Unit = {
    println("Welcome to the Simple REPL. Type :q to quit.")
    var running = true
    while (running) {
      print("> ")
      val input = readMultiLineInput()
      if (input == ":q") {
        running = false
      } else {
        parser.stmtParse(input) match
          case Success(result) => 

            println(s"Parsed: $result")
            try {
              // val evalResult = evaluate(result)
              // println(s"Result: $evalResult")
            } catch {
              case e: Exception => println(s"Evaluation Error: ${e.getMessage}")
            }
          case Failure(err) =>
            println(s"Parse Error: $err")

      }
    }
  }

  // Read multi-line input
  def readMultiLineInput(): String = {
    val input = new StringBuilder()
    var line = scala.io.StdIn.readLine()
    input.append(line)

    // Continue reading until the input is complete
    while (!isInputComplete(input.toString())) {
      print(". ") // Indicate continuation
      line = scala.io.StdIn.readLine()
      input.append("\n").append(line)
    }

    input.toString()
  }

    // Check if the input is complete using Parsley
  def isInputComplete(input: String): Boolean = {
    // Attempt to parse the input
    parser.stmtParse(input) match {
      case Success(_) => true // Input is complete
      case Failure(_) => false // Input is incomplete
    }
  }
}