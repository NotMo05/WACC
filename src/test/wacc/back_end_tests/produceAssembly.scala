package wacc.front_end
import wacc.back_end

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import parsley.{Success, Failure}
import scala.io.Source
import java.nio.file.{Files, Paths}
import java.io.File
import wacc.back_end.IR
import wacc.back_end.AssemblyWriter
import wacc.back_end.AssemblyWriter.generateAsmFile
import scala.sys.process._
import wacc.back_end.IR.generateIR
import org.scalatest.BeforeAndAfter

class produceAssembly extends AnyFlatSpec with BeforeAndAfter {

  // Temp Directories for Assembly and Binary Files
  val assemblyDir: String = "src/test/wacc/back_end_tests/assemblyFiles"
  val binDir: String = "src/test/wacc/back_end_tests/binFiles"

  /** Function to delete a directory and its contents **/
  def deleteDirectory(directory: File): Boolean = {
    if (directory.exists()) {
      val files = directory.listFiles()
      if (files != null) {
        files.foreach { file =>
          if (file.isDirectory) {
            deleteDirectory(file)
          } else {
            file.delete()
          }
        }
      }
      directory.delete()
    } else {
      false
    }
  }

  /** Function to find output values from a file **/
  def findOutputValues(filePath: String): List[String] = {
    val source = Source.fromFile(filePath)
    try {
      val lines = source.getLines()
        .filter(_.startsWith("#"))          // Find all lines starting with "#"
        .map(_.stripPrefix("#").trim)       // Remove "#" and trim whitespace
        .map(line => "  " + line)
        .toList

      // Find index of "Output:" marker
      val outputIndex = lines.indexWhere(_.contains("Output:"))

      if (outputIndex >= 0) {
        lines.drop(outputIndex + 1)         // Take all lines after "Output:"
          .filter(!_.contains("Program:"))  // Filter out lines containing "Program:"
          .filter(_.nonEmpty)               // Remove empty lines
          .map(_.replaceAll("\\s", ""))     // Remove all whitespace characters
      } else {
        List() // Return empty list if no ouput is listed
      }
    } finally {
      source.close()
    }
  }

  /** Function to dynamically register tests for a given folder **/
  def runTest(directoryPath: String, isPending: Boolean, persist: Boolean): Unit = {
    try {
      val path = Paths.get(assemblyDir)
      if (!Files.exists(path)) {
        Files.createDirectory(path)
      }
      val files = FileUtils.listAllFiles(new File(directoryPath)).filter(_.isFile)

      for (file <- files if file.getPath.endsWith(".wacc")) {
        val fileName = file.getPath

        it should s"successfully produce assembly for $fileName" in {
          if isPending then pending
          val source = Source.fromFile(file)
          val fileContent = try source.mkString finally source.close()

          parser.parse(fileContent) match {
            case Success(ast) =>
              val (prog, renamingErrors) = rename(ast)
              val (newProg, typeErrors) =  semantic.analyse(prog)
              assert(renamingErrors.isEmpty && typeErrors.isEmpty)

              val IR = generateIR(newProg)
              generateAsmFile(IR, fileName, s"$assemblyDir/")

              val baseName = new File(fileName).getName.replace(".wacc", "")
              val assemblyFilepath = s"$assemblyDir/$baseName.s"

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
    } finally {
      // Clean up resources if needed
    }
  }

  val advancedPath = "src/test/wacc/wacc-examples/valid/advanced"
  val arrayPath = "src/test/wacc/wacc-examples/valid/array"
  val basicPath = "src/test/wacc/wacc-examples/valid/basic"
  val expressionPath = "src/test/wacc/wacc-examples/valid/expressions"
  val functionPath = "src/test/wacc/wacc-examples/valid/function"
  val ifPath = "src/test/wacc/wacc-examples/valid/if"
  val IOPath = "src/test/wacc/wacc-examples/valid/IO"
  val pairsPath = "src/test/wacc/wacc-examples/valid/pairs"
  val runtimeErrPath = "src/test/wacc/wacc-examples/valid/runtimeErr"
  val scopePath = "src/test/wacc/wacc-examples/valid/scope"
  val variablesPath = "src/test/wacc/wacc-examples/valid/variables"
  val whilePath = "src/test/wacc/wacc-examples/valid/while"

  // runTest(advancedPath, false, true)
  // runTest(arrayPath, false, true)
  runTest(basicPath, false, true)
  runTest(expressionPath, false, true)
  // runTest(functionPath, false, true)
  runTest(ifPath, false, true)
  // runTest(IOPath, false, true)
  // runTest(pairsPath, false, true)
  runTest(runtimeErrPath, false, true)
  // runTest(scopePath, false, true)
  // runTest(variablesPath, false, true)
  // runTest(whilePath, false, true)
}