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
}
