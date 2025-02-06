package wacc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import parsley.{Parsley, Success, Failure}
import scala.io.Source
import java.io.File

class validProgramSyntaxTest extends AnyFlatSpec {
  val directoryPath = "wacc-examples/valid/"
  val files = listAllFiles(new File(directoryPath)).filter(_.isFile)

  for (file <- files) {
    val fileName = file.getPath
    val source = Source.fromFile(file)
    val fileContent = source.mkString
    source.close()

    it should s"successfully parse $fileName" in {
      parser.parse(fileContent) match {
        case Success(_) =>
        case Failure(msg) => fail(s"$msg?")
      }
    }
  }


  def listAllFiles(dir: File): List[File] = {
    val files = dir.listFiles()
    if (files != null) {
      files.toList.flatMap { file =>
        if (file.isDirectory) listAllFiles(file)
        else List(file)
      }
    } else {
      List.empty
    }
  }
}