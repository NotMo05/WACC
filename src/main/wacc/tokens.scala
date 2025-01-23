package wacc

enum Token {
  case Equals // Should Times and Divide be more general? // Plus Times Divides are purely BinaryOps
  case LParen, RParen, LBrack, RBrack // Parentheses and Brackets
  case Semi, Comma // Seperators

  case WType(typename: String) //case Int, Bool, Char, String, Pair, maybe Enum so only allowed ones
  case WNull
  case CommentChar // #

  enum Bool {
    case True
    case False
  }

  // Unary Operators ( Also Minus here )
  enum UnaryOps {
    case Bang
    case Len
    case Ord
    case Char
    case Negation
    case Minus
  }

  // Binary Ops
  enum BinaryOps {
    case Plus
    case Subtract
    case Times
    case Divide
    case Modulo
    case Less
    case LessEquals
    case Greater
    case GreaterEquals
    case NotEquals
    case DEquals
    case And
    case Or 
  }

  // Keywords but not really ( Stmts )	
  case Skip
  case Read
  case Free, Return, Exit
  case Print, Println 
  case Begin, End
  case If, Then, Else, Fi
  case While, Do, Done
  case Newpair, Fst, Snd
}