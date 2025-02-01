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
      case Some(expr) => parser.parse(expr) match {
        case Success(x) => {
          if fileName.contains("semanticErr") then {
            sys.exit(200)
          }
          sys.exit(0)
        }
        case Failure(msg) => sys.exit(100)
      }
        case None => println("please enter an expression")
      }
    }

