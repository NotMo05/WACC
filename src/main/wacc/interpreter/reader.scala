package wacc.interpreter
import java.io.{BufferedReader, InputStreamReader}


object TerminalReader {
  private val reader = new BufferedReader(new InputStreamReader(System.in))

  // Buffer to store the current character
  private var currentChar: Option[Char] = None

  // Read a single character from the terminal
private def readChar(): Option[Char] = {
    val char = reader.read()
    if (char == -1) {
      None // End of stream
    } else if (char == '\n' || char == '\r') {
      None
    } else {
      Some(char.toChar)
    }
  }

  // Get the current character without moving forward
  def peekChar(): Option[Char] = {
    if (currentChar.isEmpty) {
      currentChar = readChar()
    }
    currentChar
  }

  // Move forward and read the next character
  def nextChar(): Option[Char] = {
    val char = peekChar()
    currentChar = None // Clear the buffer to read the next character
    char
  }

  // Read an entire integer from the terminal
  def readInt(): Option[BigInt] = {
    var result: BigInt = 0
    var isNegative = false

    // Skip non-digit characters
    while (peekChar().exists(c => !c.isDigit && c != '-')) {
      nextChar()
    }

    // Handle negative numbers
    if (peekChar().contains('-')) {
      isNegative = true
      nextChar()
    }

    // Read digits
    while (peekChar().exists(_.isDigit)) {
      result = result * 10 + (nextChar().get - '0')
    }

    if (isNegative) Some(-result) else Some(result)
  }

  // Read a non-integer character
  def readNonIntChar(): Option[Char] = {
    while (peekChar().exists(_.isDigit)) {
      nextChar()
    }
    nextChar()
  }
}