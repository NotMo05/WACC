package wacc.front_end

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import parsley.{Success, Failure}
import scala.io.Source
import java.io.File

class validProgramSyntaxTest extends AnyFlatSpec {
  val directoryPath = "src/test/wacc/wacc-examples/valid/"
  val files = FileUtils.listAllFiles(new File(directoryPath)).filter(_.isFile)

  for (file <- files) {
    val fileName = file.getPath
    val source = Source.fromFile(file)
    val fileContent = source.mkString
    source.close()

    it should s"successfully parse $fileName" in {
      parser.parse(fileContent) match {
        case Success(_) =>
        case Failure(msg) => fail(s"$msg")
      }
    }
  }
}