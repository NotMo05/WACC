package wacc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import parsley.{Success, Failure}
import scala.io.Source
import java.io.File

class validProgramSemanticTest extends AnyFlatSpec {
  val directoryPath = "src/test/wacc/wacc-examples/valid/"
  val files = FileUtils.listAllFiles(new File(directoryPath)).filter(_.isFile)
  println(files.size)
  print(files.distinct.size)

  for (file <- files) {
    val fileName = file.getPath
    val source = Source.fromFile(file)
    val fileContent = source.mkString
    source.close()

    it should s"successfully parse $fileName" in {
      parser.parse(fileContent) match {
        case Success(ast) => {
          val (newProg, errors) = rename(ast)
          assert(errors.isEmpty && semantic.analyse(newProg).isEmpty)
        }
        case Failure(msg) => fail(s"$msg?")
      }
    }
  }
}
