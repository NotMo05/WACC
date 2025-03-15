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
        val input = readMultiLineInput(reader)
        if (input == ":q") {
          running = false
        } else {
          // Try parsing as a statement
          parser.importParse(input) match
            case Success(Import(filePath)) => {


            }
            case Failure(msg) => {
            parser.stmtParse(input) match {
              case Success(astStmt) =>
                // Process the statement
                val renamedStmt = renameStmt(astStmt, topRenamingScope, Map.empty[String, QualifiedName])
                val typeCheckedStmt = validStmtArgs(renamedStmt)

                try {
                  interpreter.stmtHandler(typeCheckedStmt)
                } catch {
                  case e: Exception => println(s"Evaluation Error: ${e.getMessage}")
                }

              case Failure(stmtErr) =>
                // If statement parsing fails, try parsing as an expression
                parser.exprParse(input) match {
                  case Success(expr: Expr) =>
                    // Process the expression
                    val renamedStmt = renameStmt(Println(expr), topRenamingScope, Map.empty[String, QualifiedName])
                    val typeCheckedStmt = validStmtArgs(renamedStmt)
                    try {
                      val retVal = interpreter.stmtHandler(typeCheckedStmt)
                    } catch {
                      case e: Exception => println(s"Evaluation Error: ${e.getMessage}")
                    }

                  case Failure(exprErr) =>
                    // If both parsing attempts fail, print an error message
                    println(s"Parse Error (Statement): $stmtErr")
                }
            }
          }
        }
      } catch {
        case _: EndOfFileException => running = false // Handle Ctrl+D
        case _: UserInterruptException => running = false // Handle Ctrl+C
      }
    }
    reader.getHistory.save()
  }


  // Read multi-line input
  def readMultiLineInput(reader: LineReader): String = {
    val input = new StringBuilder()
    var line = reader.readLine("\n> ")
    input.append(line)

    // Continue reading until the input is complete
    while (!isInputComplete(input.toString())) {
      line = reader.readLine(". ") // Use ". " as the continuation prompt
      input.append("\n").append(line)
    }

    input.toString()
  }

  // Check if the input is complete
  def isInputComplete(input: String): Boolean = {
    // Check for balanced braces and keywords
    val stack = scala.collection.mutable.Stack[Char]()
    var inString = false
    var inComment = false

    for (char <- input) {
      if (char == '"' && !inComment) inString = !inString
      if (char == '/' && !inString) inComment = !inComment

      if (!inString && !inComment) {
        char match {
          case '[' | '(' => stack.push(char)
          case ']' => !(stack.isEmpty || stack.pop() != '[')
          case ')' => !(stack.isEmpty || stack.pop() != '(')
          case _ => // Ignore other characters
        }
      }
    }

    // Check for incomplete `while` loops
    if (input.contains("while")) {
      val whileIndex = input.indexOf("while")
      val doIndex = input.indexOf("do")
      val doneIndex = input.indexOf("done")

      // Ensure `while` is followed by `do` (on the same or next line)
      if (doIndex == -1 || doIndex < whileIndex) return false

      // Ensure `done` is present
      if (doneIndex == -1) return false
    }

    // Check for incomplete `if` statements
    if (input.contains("if")) {
      val ifIndex = input.indexOf("if")
      val thenIndex = input.indexOf("then")
      val fiIndex = input.indexOf("fi")

      // Ensure `if` is followed by `then` (on the same or next line)
      if (thenIndex == -1 || thenIndex < ifIndex) return false

      // Ensure `fi` is present
      if (fiIndex == -1) return false
    }

    stack.isEmpty // Input is complete if all braces are balanced
  }
}