package wacc


enum Token {
  case Equals // Should Times and Divide be more general? // Plus Times Divides are purely BinaryOps
  case WType(typename: String) //case Int, Bool, Char, String, Pair, maybe Enum so only allowed ones
  case WNull
  enum Bool {
    case True
    case False
  }
}