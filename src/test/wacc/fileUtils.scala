package wacc

import java.io.File

object FileUtils {
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

  def formatString(input: String): String = {
  val indentStep = "  "  // Define the indentation step
  var indentLevel = 0     // Track the current indentation level

  input.foldLeft("") { (formattedString, char) =>
    char match {
      case '(' =>
        indentLevel += 1
        formattedString + "(\n" + " " * (indentLevel * indentStep.length)
      case ')' =>
        indentLevel -= 1
        formattedString + "\n" + " " * (indentLevel * indentStep.length) + ")"
      case ',' =>
        formattedString + ",\n" + " " * (indentLevel * indentStep.length)
      case ' ' =>
        formattedString
      case _ =>
        formattedString + char
    }
  }
}
}
