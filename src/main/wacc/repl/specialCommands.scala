package scala.repl
import scala.util.{Try, Success, Failure}
import wacc.interpreter.Interpreter
import org.jline.reader.LineReader
import scala.jdk.CollectionConverters._
def handleSpecialCommand(input: String, interpreter: Interpreter, reader: (LineReader)): Boolean = {
  input match {
    case ":q" | ":quit" =>
      println("Exiting REPL. Goodbye!")
      false

    case ":clear" | ":cls" =>
      print("\u001b[H\u001b[2J") // ANSI escape code to clear the screen
      true

    case ":help" =>
      println("""
        Available commands:
        :q, :quit       - Quit the REPL
        :clear, :cls    - Clear the screen
        :help           - Show this help message
        :history        - Show command history
        :reset          - Reset the interpreter state
        :vars           - List defined variables
        :funcs          - List defined functions
      """)
      true

    case ":history" =>
      reader.getHistory.forEach(println) // Show command history
      true

    case ":reset" =>
      interpreter.reset()
      println("Interpreter state has been reset.")
      true
      
    case ":vars" =>
      interpreter.printLocals()
      true

    case ":funcs" =>
      interpreter.printFunctions()
      true

    case _ =>
      false
  }
}