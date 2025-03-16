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
            case Success(imp: Import) => {
              interpreter.mutableFuncTable.addAll(
                interpreter.addFuncsToMutableFuncTable(importHandleForRepl(imp))
              )
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

    var indentLevel = updateIndentation(line, 0)

    // Continue reading until the input is complete
    while (!isInputComplete(input.toString())) {
      line = reader.readLine(". " + "  " * indentLevel) // Use ". " as the continuation prompt
      input.append("\n").append(line)
      indentLevel = updateIndentation(line, indentLevel)
    }

    input.toString()
  }

  // Check if the input is complete
  def isInputComplete(input: String): Boolean = {
    val lines = input.split("\n")
    val stack = scala.collection.mutable.Stack[String]() // Stack to track block keywords
    var isComplete = true // Flag to track if the input is complete

    for (line <- lines) {
      val trimmedLine = line.trim

      // Skip empty lines
      if (trimmedLine.nonEmpty) {
        // Check for block keywords
        if (trimmedLine.startsWith("while")) {
          stack.push("while")
        } else if (trimmedLine.startsWith("if")) {
          stack.push("if")
        } else if (trimmedLine.startsWith("begin")) {
          stack.push("begin")
        }

        // Check for block terminators

        // Check for block terminators
        trimmedLine match
          case line if line.endsWith("done") =>
            if (stack.isEmpty || stack.pop() != "while") isComplete = false
          case line if line.endsWith("fi") =>
            if (stack.isEmpty || stack.pop() != "if") isComplete = false
          case line if line.endsWith("end") =>
            if (stack.isEmpty || stack.pop() != "begin") isComplete = false
          case _ => // Ignore other lines
      }
    }

    // Input is complete if all blocks are closed
    isComplete && stack.isEmpty
  }

  def updateIndentation(line: String, currentIndent: Int): Int = {
    val trimmedLine = line.trim

    if (trimmedLine.startsWith("while") || trimmedLine.startsWith("if") || trimmedLine.startsWith("begin")) {
      currentIndent + 1 // Increase indentation for blocks
    } else if (trimmedLine.endsWith("done") || trimmedLine.endsWith("fi") || trimmedLine.endsWith("end")) {
      currentIndent - 1 // Decrease indentation for block terminators
    } else {
      currentIndent // No change in indentation
    }
  }
}