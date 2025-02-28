package wacc.front_end
import wacc.back_end

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import parsley.{Success, Failure}
import scala.io.Source
import java.io.File
import java.nio.file.{Files, Paths}
import wacc.back_end.IR
import wacc.back_end.AssemblyWriter
import wacc.back_end.AssemblyWriter.generateAsmFile
import scala.sys.process._
import wacc.back_end.IR.generateIR

class produceAssembly extends AnyFlatSpec {

  val binDir: String = "src/test/wacc/back_end_tests/bin"
  val assDir: String = "assemblyFiles"

  def findOutputValues(filePath: String): List[String] = {
    val source = Source.fromFile(filePath)
    try {
      val lines = source.getLines()
        .filter(_.startsWith("#")) // Find all lines starting with "#"
        .map(_.stripPrefix("#").trim) // Remove "#" and trim whitespace
        .map(line => "  " + line)
        .toList

      // Find index of "Output:" marker
      val outputIndex = lines.indexWhere(_.contains("Output:"))

      if (outputIndex >= 0) {
        lines.drop(outputIndex + 1) // Take all lines after "Output:"
          .filter(!_.contains("Program:")) // Filter out lines containing "Program:"
          .filter(_.nonEmpty) // Remove empty lines
          .map(_.replaceAll("\\s", "")) // Remove all whitespace characters
      } else {
        List() // Return empty list if marker not found
      }
    } finally {
      source.close()
    }
  }

    /** Function to dynamically register tests for a given folder **/
  def runTest(directoryPath: String, isPending: Boolean): Unit = {
    val files = FileUtils.listAllFiles(new File(directoryPath)).filter(_.isFile)

    for (file <- files if file.getPath.endsWith(".wacc")) {
      val fileName = file.getPath

      it should s"successfully produce assembly for $fileName" in {
        if isPending then pending
        val source = Source.fromFile(file)
        val fileContent = try source.mkString finally source.close()

        parser.parse(fileContent) match {
          case Success(ast) =>
            val (newProg, errors) = rename(ast)
            assert(errors.isEmpty && semantic.analyse(newProg).isEmpty)

            val IR = generateIR(newProg)
            generateAsmFile(IR, fileName, s"$assDir/")

            val baseName = new File(fileName).getName.replace(".wacc", "")
            val assemblyFilepath = s"$assDir/$baseName.s"

            if (!new File(binDir).exists()) {
              new File(binDir).mkdirs()
            }

            val compilation = Process(s"gcc -z noexecstack -o $binDir/$baseName $assemblyFilepath").!
            println(s"\nCompilation exit code: $compilation")

            val exitCode = Process(s"./$binDir/$baseName").!
            println(s"Program exitCode: $exitCode")

            val expectedResult = findOutputValues(file.getPath)
            println(s"Comments are: $expectedResult")

            if (expectedResult.isEmpty) {
              println("Not expecting any exit codes or returns")
              assert(exitCode.toString == "0")
            } else if (expectedResult.head == "Exit:") {
              val expected = expectedResult(1)
              println(s"Expecting exit code of $expected")
              assert(expected == exitCode.toString)
            }

          case Failure(msg) => fail(s"Parsing failed: $msg")
        }
      }
    }
  }


  val expressionPath = "src/test/wacc/wacc-examples/valid/expressions"
  val basicPath = "src/test/wacc/wacc-examples/valid/basic"

  runTest(expressionPath, true)
  runTest(basicPath, false)
}