package wacc.repl
import wacc.front_end._

object SimpleREPL {
  def repl(): Unit = {
    println("Welcome to the Simple REPL. Type :q to quit.")
    var running = true
    while (running) {
      print("> ")
      val input = scala.io.StdIn.readLine()
      if (input == ":q") {
        running = false
      } else {
        println(parser.stmtParse(input))



        try {
          val result = evaluate(input)
          println(s"Result: $result")
        } catch {
          case e: Exception => println(s"Error: ${e.getMessage}")
        }
      }
    }
  }

  def evaluate(input: String): Any = {
    // Replace this with your actual evaluation logic
    input.toInt * 2 // Example: Doubles the input number
  }
}