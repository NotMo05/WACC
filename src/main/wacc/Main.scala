package wacc

import scala.io.Source
import parsley.{Success, Failure}
import java.io.File

def main(args: Array[String]): Unit = {
  println("hello WACC!")

  val fileName = args(0)
  val file = new File(fileName)
  val fileContent = Array(Source.fromFile(file).mkString)
  fileContent.headOption match {
    // Parse and conduct syntax analysis on the program string
    case Some(progString) => parser.parse(progString) match {
      case Success(ast) => {
        // Conduct semantic analysis on the AST produced by syntax analysis
        // ast.main.foreach(println(_))
        val (newProg, errors) = rename(ast)
        if !(errors.isEmpty && semantic.analyse(newProg).isEmpty) then {
          println("Semantic Errors:")
          // errors.foreach(println(_))
          semantic.analyse(newProg).foreach(println(_))
          newProg.main.foreach(println(_))
          // newProg.funcs.foreach(_.stmts.foreach(println(_)))
          // println(newProg)
          sys.exit(200)
        } else {
          print("No Error")
          sys.exit(0)
        }
      }
      case Failure(msg) => {
        print("Syntax Error")
        print(msg)
        sys.exit(100)}
    }
      case None => println("please enter an expression")
    }
  }
