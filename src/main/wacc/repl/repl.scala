package wacc.repl
import wacc.front_end._
import parsley.Success
import parsley.Failure
import org.jline.reader.{LineReader, LineReaderBuilder, EndOfFileException, UserInterruptException}
import org.jline.terminal.TerminalBuilder
import scala.collection.mutable
import wacc.front_end.semantic.validStmtArgs
import wacc.interpreter.Interpreter

object SimpleREPL {
  def repl(args: Array[String]): Unit = {
    val terminal = TerminalBuilder.builder().system(true).build()
    val reader = LineReaderBuilder.builder()
      .terminal(terminal)
      .build()
    val interpreter = Interpreter()

    // Enable history
    val historyFile = java.nio.file.Paths.get(".repl_history")
    reader.setVariable(LineReader.HISTORY_FILE, historyFile)

    println("Welcome to the Simple REPL. Type :q to quit.")
    var running = true
    val topRenamingScope = mutable.Map.empty[String, QualifiedName]
    while (running) {
      try {
        val input = reader.readLine("> ")
        if (input == ":q") {
          running = false
        } else {
          parser.stmtParse(input) match
            case Success(astStmt) =>
              val renamedStmt = renameStmt(astStmt, topRenamingScope, Map.empty[String, QualifiedName])
              val typeCheckedStmt = validStmtArgs(renamedStmt)

              try {
                val retVal = interpreter.stmtHandler(typeCheckedStmt)
              } catch {
                case e: Exception => println(s"Evaluation Error: ${e.getMessage}")
              }
            case Failure(err) =>
              println(s"Parse Error: $err")
        }
      } catch {
        case _: EndOfFileException => running = false // Handle Ctrl+D
        case _: UserInterruptException => running = false // Handle Ctrl+C
      }
    }
    reader.getHistory.save()
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