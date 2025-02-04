package wacc

import scala.io.Source
import parsley.{Success, Failure}
import java.io.File

def main(args: Array[String]): Unit = {
  println("hello WACC!")
  // runtimeErr/integerOverflow/ fails  Negation tick, Subtraction tick, negative numbers X

  // val directoryPath = "src/test/wacc/wacc-examples/valid/runtimeErr/integerOverflow/"
  // val files = new File(directoryPath).listFiles()
  // val fileNames = files.filter(_.isFile).map(_.getName).toList
  // print(fileNames.toString())

  // for (fileName <- fileNames) {
  //   val filePath = directoryPath + fileName
  //   val fileContent = Source.fromFile(filePath).mkString
  //   Source.fromFile(filePath).close()


  //   val arguments = if args.isEmpty then Array(fileContent) else args
    val fileName = args(0)
    val file = new File(fileName)
    val fileContent = Array(Source.fromFile(file).mkString)
    fileContent.headOption match {
      // Parse and conduct syntax analysis on the program string
      case Some(progString) => parser.parse(progString) match {
        case Success(ast) => {
          // Conduct semantic analysis on the AST produced by syntax analysis
          if !semantic.analyse(rename(ast)).isEmpty then {
            println("Semantic Errors:") 
            semantic.analyse(rename(ast)).foreach(println(_))
            //sys.exit(200)
          } else
          print("No Error") //sys.exit(0)
        }
        case Failure(msg) => print("Syntax Error") // sys.exit(100)
      }
        case None => println("please enter an expression")
      }
    }

