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

class produceAssembly extends AnyFlatSpec {

  val binDir: String = "src/test/wacc/back_end_tests/bin"
  val assDir: String = "src/test/wacc/back_end_tests/assemblyFiles"

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
    try {
      val path = Paths.get(assDir)
      if (!Files.exists(path)) {
        Files.createDirectory(path)
      }
      val files = FileUtils.listAllFiles(new File(directoryPath)).filter(_.isFile)

      for (file <- files if file.getPath.endsWith(".wacc")) {
        // Your existing test code
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
    } finally {
      // Delete the assembly files directory after tests complete
      try {
        deleteDirectory(new File(assDir))
        println(s"Cleaned up assembly files directory: $assDir")
        
        // Also clean up binary directory
        deleteDirectory(new File(binDir))
        println(s"Cleaned up binary files directory: $binDir")
      } catch {
        case e: Exception => 
          println(s"Warning: Failed to delete directories: ${e.getMessage}")
      }
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

  runTest(advancedPath, true)
  runTest(arrayPath, true)
  runTest(basicPath, true)
  runTest(expressionPath, true)
  runTest(functionPath, true)
  runTest(ifPath, true)
  runTest(IOPath, true)
  runTest(pairsPath, true)
  runTest(runtimeErrPath, true)
  runTest(scopePath, true)
  runTest(variablesPath, true)
  runTest(whilePath, true)
}