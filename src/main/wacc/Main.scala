package wacc.front_end
import scala.io.Source
import parsley.{Success, Failure}
import java.io.File
import wacc.back_end.IR.generateIR
import wacc.back_end.AssemblyWriter.generateAsmFile
import wacc.interpreter.Interpreter
import wacc.repl.SimpleREPL
final val SUCCESS = 0
final val SYNTAX_ERR = 100
final val SEMANTIC_ERR = 200

def main(args: Array[String]): Unit = {
  if args.isEmpty then
    println("No argument was provided"); return

  if args(0) == "interpreter" then {
    processFileArg(args, 1).foreach { fileContent => //file path should be second argument
      val fileName = args(1)
      val prog = genAST(fileContent, fileName)
      try {
        val interpreter = new Interpreter(prog)
        interpreter.execute(prog)
        sys.exit(SUCCESS)
      } catch {
        case e: RuntimeException => println(e.getMessage()); sys.exit(1)
      }
    }
    return
  } else if args(0) == "repl" then {
    SimpleREPL.repl(args.drop(1))
    sys.exit(SUCCESS)
  }

  processFileArg(args, 0).foreach { fileContent =>
    val fileName = args(0)
    val prog = genAST(fileContent, fileName)
    compileAST(prog, fileName)
  }
}

def processFileArg(args: Array[String], index: Int): Option[String] = {
  if (args.length > index) {
    val fileName = args(index)
    val file = new File(fileName)

    if (!file.exists() || !file.isFile) {
      println(s"Error: File '$fileName' does not exist or is not a regular file.")
      None
    } else {
      Some(Source.fromFile(file).mkString)
    }
  } else {
    println(s"Error: No argument at index $index.")
    None
  }
}

def genAST(fileContent: String, fileName: String): Prog = {
  parser.parse(fileContent) match {
    case Success(ast) =>
      val (prog, renamingErrors) = rename(ast)
      val (newProg, typeErrors) =  semantic.analyse(prog)
      if (renamingErrors.nonEmpty || typeErrors.nonEmpty) {
        println("Semantic Errors:")
        renamingErrors.foreach(println)
        typeErrors.foreach(println)
        sys.exit(SEMANTIC_ERR)
      } else {
        newProg
      }

    case Failure(msg) =>
      println("Syntax Error")
      println(msg)
      sys.exit(SYNTAX_ERR)
  }
}

def compileAST(prog: Prog, fileName: String) = {
  val IR = generateIR(prog)
  generateAsmFile(IR, fileName)
  sys.exit(SUCCESS)
}