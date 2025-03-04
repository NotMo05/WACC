package wacc.back_end

import wacc.front_end._
import RegName._
import DataWidth._
import ExprGen._
import wacc.back_end.Stack.typeToSize
import scala.collection.mutable
import scala.collection.mutable.{Builder}
import wacc.front_end.semantic.getExprType


object IR {
  val STACK_ALIGN = -16
  var localLabelCounter = 0
  var strCounter: Int = 0

  val strMap = mutable.Map[String, Int]()

  case class StringInfo(val string: String, val len: Int, val strCount: Int) {
    override def toString(): String = s"$strCount$string$len"
  }
  
    def foldConsts(prog: Prog): Prog = {
      // Process main program statements
      val foldedMain = prog.main.map(foldConstStmtHelper)
      
      // Process functions
      val foldedFuncs = prog.funcs.map(func => 
        Func(func.t, func.identifier, func.params, func.stmts.map(foldConstStmtHelper))
      )
      
      Prog(foldedFuncs, foldedMain)
    }

  def foldConstStmtHelper(stmt: Stmt): Stmt = {
    stmt match {
      case Skip => Skip
      case Read(lValue) => Read(lValue)
      case Free(expr) => Free(foldConstExprHelper(expr))
      case Return(expr) => Return(foldConstExprHelper(expr))
      case Exit(expr) => Exit(foldConstExprHelper(expr))
      case Print(expr) => Print(foldConstExprHelper(expr))
      case Println(expr) => Println(foldConstExprHelper(expr))
      
      case WhileDo(condition, stmts) => {
        val foldedCond = foldConstExprHelper(condition)
        foldedCond match {
          case BoolLiteral(false) => Skip  // Unreachable code elimination
          case _ => WhileDo(foldedCond, stmts.map(foldConstStmtHelper))
        }
      }
      
      case IfElse(condition, thenStmts, elseStmts) => {
        val foldedCond = foldConstExprHelper(condition)
        foldedCond match {
          case BoolLiteral(true) => Scope(thenStmts.map(foldConstStmtHelper))
          case BoolLiteral(false) => Scope(elseStmts.map(foldConstStmtHelper))
          case _ => IfElse(foldedCond, 
                          thenStmts.map(foldConstStmtHelper), 
                          elseStmts.map(foldConstStmtHelper))
        }
      }
      
      case Assgn(t, identifier, rValue) => {
        rValue match {
          case expr: Expr => Assgn(t, identifier, foldConstExprHelper(expr))
          case Call(funcName, args) => 
            Assgn(t, identifier, Call(funcName, args.map(foldConstExprHelper)))
          case other => Assgn(t, identifier, other)
        }
      }
      
      case ReAssgn(lValue, rValue) => {
        rValue match {
          case expr: Expr => ReAssgn(lValue, foldConstExprHelper(expr))
          case Call(funcName, args) => 
            ReAssgn(lValue, Call(funcName, args.map(foldConstExprHelper)))
          case other => ReAssgn(lValue, other)
        }
      }
      
      case Scope(stmts) => Scope(stmts.map(foldConstStmtHelper))
    }
  }

  def foldConstExprHelper(expr: Expr): Expr = {
    expr match {
      // Base cases - literals remain unchanged
      case lit: IntLiteral => lit
      case lit: BoolLiteral => lit
      case lit: StringLiteral => lit
      case lit: CharLiteral => lit
      case id: Ident => id
      case NullLiteral => NullLiteral
      
      // Array elements - fold the index expressions
      case ArrayElem(arrayName, index) => 
        ArrayElem(arrayName, index.map(foldConstExprHelper))
      
      // Unary operations
      case Neg(x) => {
        val foldedX = foldConstExprHelper(x)
        foldedX match {
          case IntLiteral(i) => IntLiteral(-i)
          case _ => Neg(foldedX)
        }
      }
      
      case Not(x) => {
        val foldedX = foldConstExprHelper(x)
        foldedX match {
          case BoolLiteral(b) => BoolLiteral(!b)
          case _ => Not(foldedX)
        }
      }
      
      case Len(x) => {
        val foldedX = foldConstExprHelper(x)
        foldedX match {
          case StringLiteral(s) => IntLiteral(s.length)
          case _ => Len(foldedX)
        }
      }
      
      // Character conversions
      case Chr(x) => {
        val foldedX = foldConstExprHelper(x)
        foldedX match {
          case IntLiteral(i) if i >= 0 && i <= 127 => CharLiteral(i.toChar)
          case _ => Chr(foldedX)
        }
      }
      
      case Ord(x) => {
        val foldedX = foldConstExprHelper(x)
        foldedX match {
          case CharLiteral(c) => IntLiteral(c.toInt)
          case _ => Ord(foldedX)
        }
      }
      
      // Binary arithmetic operations
      case Mul(l, r) => {
        val (foldedL, foldedR) = (foldConstExprHelper(l), foldConstExprHelper(r))
        (foldedL, foldedR) match {
          case (IntLiteral(a), IntLiteral(b)) => IntLiteral(a * b)
          case _ => Mul(foldedL, foldedR)
        }
      }
      
      case Div(l, r) => {
        val (foldedL, foldedR) = (foldConstExprHelper(l), foldConstExprHelper(r))
        (foldedL, foldedR) match {
          case (IntLiteral(a), IntLiteral(b)) if b != 0 => IntLiteral(a / b)
          case _ => Div(foldedL, foldedR) // Let runtime handle division by zero
        }
      }
      
      case Mod(l, r) => {
        val (foldedL, foldedR) = (foldConstExprHelper(l), foldConstExprHelper(r))
        (foldedL, foldedR) match {
          case (IntLiteral(a), IntLiteral(b)) if b != 0 => IntLiteral(a % b)
          case _ => Mod(foldedL, foldedR)
        }
      }
      
      case Add(l, r) => {
        val (foldedL, foldedR) = (foldConstExprHelper(l), foldConstExprHelper(r))
        (foldedL, foldedR) match {
          case (IntLiteral(a), IntLiteral(b)) => IntLiteral(a + b)
          case _ => Add(foldedL, foldedR)
        }
      }
      
      case Sub(l, r) => {
        val (foldedL, foldedR) = (foldConstExprHelper(l), foldConstExprHelper(r))
        (foldedL, foldedR) match {
          case (IntLiteral(a), IntLiteral(b)) => IntLiteral(a - b)
          case _ => Sub(foldedL, foldedR)
        }
      }
      
      // Comparison operations
      case Less(l, r) => {
        val (foldedL, foldedR) = (foldConstExprHelper(l), foldConstExprHelper(r))
        (foldedL, foldedR) match {
          case (IntLiteral(a), IntLiteral(b)) => BoolLiteral(a < b)
          case _ => Less(foldedL, foldedR)
        }
      }
      
      case LessE(l, r) => {
        val (foldedL, foldedR) = (foldConstExprHelper(l), foldConstExprHelper(r))
        (foldedL, foldedR) match {
          case (IntLiteral(a), IntLiteral(b)) => BoolLiteral(a <= b)
          case _ => LessE(foldedL, foldedR)
        }
      }
      
      case Greater(l, r) => {
        val (foldedL, foldedR) = (foldConstExprHelper(l), foldConstExprHelper(r))
        (foldedL, foldedR) match {
          case (IntLiteral(a), IntLiteral(b)) => BoolLiteral(a > b)
          case _ => Greater(foldedL, foldedR)
        }
      }
      
      case GreaterE(l, r) => {
        val (foldedL, foldedR) = (foldConstExprHelper(l), foldConstExprHelper(r))
        (foldedL, foldedR) match {
          case (IntLiteral(a), IntLiteral(b)) => BoolLiteral(a >= b)
          case _ => GreaterE(foldedL, foldedR)
        }
      }
      
      // Equality operations
      case Eq(l, r) => {
        val (foldedL, foldedR) = (foldConstExprHelper(l), foldConstExprHelper(r))
        (foldedL, foldedR) match {
          case (IntLiteral(a), IntLiteral(b)) => BoolLiteral(a == b)
          case (BoolLiteral(a), BoolLiteral(b)) => BoolLiteral(a == b)
          case (CharLiteral(a), CharLiteral(b)) => BoolLiteral(a == b)
          case (StringLiteral(a), StringLiteral(b)) => BoolLiteral(a == b)
          case _ => Eq(foldedL, foldedR)
        }
      }
      
      case NotEq(l, r) => {
        val (foldedL, foldedR) = (foldConstExprHelper(l), foldConstExprHelper(r))
        (foldedL, foldedR) match {
          case (IntLiteral(a), IntLiteral(b)) => BoolLiteral(a != b)
          case (BoolLiteral(a), BoolLiteral(b)) => BoolLiteral(a != b)
          case (CharLiteral(a), CharLiteral(b)) => BoolLiteral(a != b)
          case (StringLiteral(a), StringLiteral(b)) => BoolLiteral(a != b)
          case _ => NotEq(foldedL, foldedR)
        }
      }
      
      // Logical operations with short-circuit optimization
      case And(l, r) => {
        val foldedL = foldConstExprHelper(l)
        foldedL match {
          case BoolLiteral(false) => BoolLiteral(false) // Short-circuit
          case _ => {
            val foldedR = foldConstExprHelper(r)
            (foldedL, foldedR) match {
              case (BoolLiteral(true), right) => right
              case (left, BoolLiteral(true)) => left
              case (BoolLiteral(a), BoolLiteral(b)) => BoolLiteral(a && b)
              case _ => And(foldedL, foldedR)
            }
          }
        }
      }
      
      case Or(l, r) => {
        val foldedL = foldConstExprHelper(l)
        foldedL match {
          case BoolLiteral(true) => BoolLiteral(true) // Short-circuit
          case _ => {
            val foldedR = foldConstExprHelper(r)
            (foldedL, foldedR) match {
              case (BoolLiteral(false), right) => right
              case (left, BoolLiteral(false)) => left
              case (BoolLiteral(a), BoolLiteral(b)) => BoolLiteral(a || b)
              case _ => Or(foldedL, foldedR)
            }
          }
        }
      }
    }
  }
    
  // Generates intermediate representation (IR) for the given program
  def generateIR(prog: Prog): (List[StringInfo], List[FuncLabelDef]) = {
    val optimizedProg = foldConsts(prog)
    val sections = generateROData(optimizedProg.main) :: optimizedProg.funcs.map(func => generateROData(func.stmts))
    val funcLabelDefs = funcGenerate(optimizedProg.main) :: optimizedProg.funcs.map(func => funcGenerate(func.stmts, func.identifier.identifier, func.params))
    val errorList = List("_errBadChar","_errNull","_errOutOfMem","_errOutOfBounds","_errOverflow","_errDivZero")
    val allFuncLabelDefs: List[FuncLabelDef] = funcLabelDefs ++ errorList.map(generateErrorLabels(_))

    return(sections.flatten, allFuncLabelDefs)
  }

  // Generates a function label definition for error handling based on the provided name
  def generateErrorLabels(name: String): FuncLabelDef = {
    val asmBuilder = List.newBuilder[Instr]
    asmBuilder += AND(Reg(Rsp, QWord), Imm(STACK_ALIGN))
    asmBuilder += LEA(Reg(Rdi, QWord), StringAddr(s"${name}"))
    printString(Reg(Rdi, QWord), formatMap(StringType), asmBuilder)
    asmBuilder += MOV(Reg(Rdi, Byte), Imm(-1))
    asmBuilder += AND(Reg(Rsp, QWord), Imm(STACK_ALIGN))
    asmBuilder += CALL(Label("exit@plt"))
    FuncLabelDef(name, asmBuilder)
  }

  // Moves a qualified name to a destination register with an optional data width (default is QWord)
  def movQnToReg(destReg: RegName, qn: QualifiedName, dataWidth: DataWidth = QWord) =
   MOV(Reg(destReg, dataWidth), OffsetAddr(dataWidth, Reg(Rbp, QWord), Stack.getOffset(qn)))

  // Moves a register or immediate value to memory at the specified qualified name with an optional data width.
  def movRegOrImmToMem(srcRegOrImm: RegName | Imm | Reg, qn: QualifiedName, dataWidth: DataWidth = QWord) = {
    srcRegOrImm match
      case reg: RegName => MOV(OffsetAddr(dataWidth, Reg(Rbp, QWord), Stack.getOffset(qn)), Reg(reg, dataWidth))
      case imm: Imm => MOV(OffsetAddr(dataWidth, Reg(Rbp, QWord), Stack.getOffset(qn)), imm)
      case reg: Reg => MOV(OffsetAddr(dataWidth, Reg(Rbp, QWord), Stack.getOffset(qn)), reg)
  }

  // Generates read-only data section from a list of statements
  def generateROData(stmts: List[Stmt]): List[StringInfo] = {
    return stmts.foldLeft(List.empty[StringInfo])((list: List[StringInfo], stmt: Stmt) => 
      (stmt match
        case Return(expr) => ROExprHelper(expr)
        case Print(expr) => ROExprHelper(expr)
        case Println(expr) => ROExprHelper(expr) 
        case WhileDo(condition, stmts) => ROExprHelper(condition) ++ generateROData(stmts)
        case IfElse(condition, thenStmts, elseStmts) => ROExprHelper(condition) ++ generateROData(thenStmts) ++ generateROData(elseStmts)
        case Scope(stmts) => generateROData(stmts)
        case Assgn(t, identifier, rValue) => RORValueHelper(rValue)
        case ReAssgn(lValue, rValue) => RORValueHelper(rValue)
        case _ => Nil
      ) ++ list)
  }

  // Helpers for generating read-only data section

  // Helper for RValues
  def RORValueHelper(rvalue: RValue): List[StringInfo] = {
    return rvalue match
      case Call(funcName: QualifiedFunc, args: List[Expr]) => args.map(i => ROExprHelper(i)).flatten
      case Fst(lValue: LValue) => ROLValueHelper(lValue)
      case Snd(lValue: LValue) => ROLValueHelper(lValue)
      case NewPair(fst: Expr, snd: Expr) => ROExprHelper(fst) ++ ROExprHelper(snd)
      case ArrayLiter(elems) => elems.map(i => ROExprHelper(i)).flatten
      case expr: Expr => ROExprHelper(expr)
      case _ => Nil
  }

  // Helper for LValues
  def ROLValueHelper(lvalue: LValue): List[StringInfo] = {
    return lvalue match
      case Fst(lValue: LValue) => ROLValueHelper(lValue)
      case Snd(lValue: LValue) => ROLValueHelper(lValue)
      case _ => Nil
}

  // Helper for expressions
  def ROExprHelper(expr: Expr): List[StringInfo] = {
    expr match
      case StringLiteral(string) => {
        strMap(string) = strCounter
        strCounter += 1
        List(StringInfo(string, string.length(), strCounter - 1))
      }
      case op: Operator => {
        op match 
          case Not(x) => ROExprHelper(x)
          case Eq(l, r) => ROExprHelper(l) ++ ROExprHelper(r)
          case NotEq(l, r) => ROExprHelper(l) ++ ROExprHelper(r) 
          case And(l, r) => ROExprHelper(l) ++ ROExprHelper(r) 
          case Or(l, r) => ROExprHelper(l) ++ ROExprHelper(r)
          case _ => Nil
      }
      case _ => Nil
    }   

  // Generates a function label definition from a list of statements and parameters.
  def funcGenerate(stmts: List[Stmt], funcName: String = "", params: List[Param] = Nil): FuncLabelDef = {
    val sortedParams = params.sortBy(p => p.t.toString())

    val assignedParams = if !params.isEmpty then {
      sortedParams.map( p =>
        (p: @unchecked) match
          case Param(t, qn: QualifiedName) => {
            Assgn(t, qn, IntLiteral(0))
          }
      )
    } else Nil
    
    val asmBuilder = List.newBuilder[Instr]
    Stack.initialise(assignedParams ++ stmts)
    println(Stack.frames.last.identTable)
    asmBuilder += PUSH(Reg(Rbp, QWord))
    asmBuilder += MOV(Reg(Rbp, QWord), Reg(Rsp, QWord))
    asmBuilder += SUB(Reg(Rsp, QWord), Imm(-Stack.frames.last.currentDepth))
    stmts.map(stmtGen(_, asmBuilder))
    
    val funcLabel = if (funcName == "") {
      asmBuilder += MOV(Reg(Rax, QWord), Imm(0))
      "main"
    } else s"wacc_$funcName"

    asmBuilder += ADD(Reg(Rsp, QWord), Imm(-Stack.frames.last.currentDepth)) 
    asmBuilder ++= popRbp
    asmBuilder += RET 
    
    return FuncLabelDef(funcLabel, asmBuilder)
  }

  // Generates assembly instructions for a given statement
  def stmtGen(stmt: Stmt, asmBuilder: Builder[Instr, List[Instr]]): Unit =
    (stmt: @unchecked) match {
      case Skip => ()
      case Assgn(t, qn: QualifiedName, rValue) => assgnGen(qn, rValue, asmBuilder, t)
      case ReAssgn(lValue, rValue) => reassgnGen(lValue, rValue, asmBuilder)
      case Exit(expr) => exitGen(expr, asmBuilder)
      case Return(expr) => returnGen(expr, asmBuilder)
      case Read(lValue) => readGen(lValue, asmBuilder)
      case Free(expr) => freeGen(expr, asmBuilder)
      case Scope(stmts) => scopeGen(stmts, asmBuilder)
      case Print(expr) => {
        printGen(expr, asmBuilder)
        asmBuilder ++= popRbp
      }
      case Println(expr) => {
        printGen(expr, asmBuilder)
        asmBuilder.addAll(
          List(
            SUB(Reg(Rsp, QWord), Imm(8)),
            MOV(OffsetAddr(MemOpModifier.QWordPtr, Reg(Rsp, QWord)), Imm(0)),
            MOV(Reg(Rdi, QWord), Reg(Rsp, QWord)),
            CALL(Label("puts@plt")),
            MOV(Reg(Rdi, QWord), Imm(0)),
            CALL(Label("fflush@plt")),
            ADD(Reg(Rsp, QWord), Imm(8))
          )
        )
        asmBuilder ++= popRbp
      }
      case WhileDo(condition, stmts) => whileGen(condition, stmts, asmBuilder)
      case IfElse(condition, thenStmts, elseStmts) => ifElseGen(condition, thenStmts, elseStmts, asmBuilder)
    }

  // Generates assembly instructions for reassigning a value to a variable
  def reassgnGen(lValue: LValue, rValue: RValue, asmBuilder: Builder[Instr, List[Instr]]) = {
    (lValue: @unchecked) match
      case qn: QualifiedName => assgnGen(qn, rValue, asmBuilder, qn.t)
      case ArrayElem(qn: QualifiedName, index) => {
        val (dataWidth, pointer) = repeatAccessArray(qn, index, asmBuilder)
        asmBuilder += MOV(pointer, rValueGen(rValue, asmBuilder, qn.t))
      }

      case Fst(lValue2) => {
        fstSndAddress(lValue2, asmBuilder)
        asmBuilder += MOV(Reg(R9, QWord), Reg(R10, QWord))
        val regOrImm: (Reg | Imm) =
        rValueGen(rValue, asmBuilder) match
          case Reg(num, dataWidth) => Reg(num, QWord)
          case Imm(value) => Imm(value)
        asmBuilder += MOV(OffsetAddr(MemOpModifier.QWordPtr, Reg(R9, QWord)), regOrImm) 
      }

      case Snd(lValue2) => {
        fstSndAddress(lValue2, asmBuilder, 8)
        asmBuilder += MOV(Reg(R9, QWord), Reg(R10, QWord))
        val regOrImm: (Reg | Imm) =
        rValueGen(rValue, asmBuilder) match
          case Reg(num, dataWidth) => Reg(num, QWord)
          case Imm(value) => Imm(value)
        asmBuilder += MOV(OffsetAddr(MemOpModifier.QWordPtr, Reg(R9, QWord)), regOrImm)
      }
  }

  // Accesses an element at a specific index in an array
  def arrayIndexAccess(
    pointerAddress: MemAddr, 
    index: Expr, t: Type, 
    asmBuilder: Builder[Instr, List[Instr]]
  ): (Int, MemAddr) = {
    asmBuilder += MOV(Reg(R9, QWord), pointerAddress)
    val dataWidth = typeToSize(t)
    asmBuilder += PUSH(Reg(R9, QWord))
    val reg = exprGen(index, asmBuilder)
    asmBuilder += POP(Reg(R9, QWord))
    asmBuilder += MOV(Reg(R8, DWord), reg)

    asmBuilder.addAll(
      List(
        CMP(Reg(R8, DWord), Imm(0)),
        JCond(Cond.L, Label("_errOutOfBounds")),
        CMP(Reg(R8, DWord), OffsetAddr(MemOpModifier.DWordPtr, Reg(R9, QWord), -4)),
        JCond(Cond.GE, Label("_errOutOfBounds"))
      )
    )
    (dataWidth, RegScale(dataWidth, Reg(R9, QWord), typeToSize(t), Reg(R8, QWord)))
  }

  // Generates the intermediate representation (IR) for an r-value expression
  def rValueGen(
    rValue: RValue, 
    asmBuilder: Builder[Instr, List[Instr]], 
    t: Type = Undefined
  ): (Reg | Imm) = {
    (rValue: @unchecked) match
      case expr: Expr => exprGen(expr, asmBuilder)
      case Fst(lValue) => fstSndAddress(lValue, asmBuilder)
      case Snd(lValue) => fstSndAddress(lValue, asmBuilder, 8)
      case ArrayLiter(elems) => mallocArrayLiter(elems, t.asInstanceOf[ArrayType], asmBuilder)
      case NewPair(fst, snd) => mallocNewPair(fst, snd, asmBuilder)
      case Call(qn: QualifiedFunc, args) => callGen(qn, args, asmBuilder)
  }

  // Generates assembly instructions for an assignment operation
  def assgnGen(
    qn: QualifiedName, 
    rValue: RValue, 
    asmBuilder: Builder[Instr, List[Instr]], 
    t: Type = Undefined
  ) = {
    val dataWidth = Stack.typeToSize(t)
    asmBuilder += ((rValue: @unchecked) match
      case expr: Expr => {
        exprGen(expr, asmBuilder) match
          case Reg(num, dataWidth) => {
            print(MOV(OffsetAddr(dataWidth, Reg(Rbp, QWord), Stack.getOffset(qn)), Reg(num, dataWidth)))
            MOV(OffsetAddr(dataWidth, Reg(Rbp, QWord), Stack.getOffset(qn)), Reg(num, dataWidth))
          }

          case Imm(value) => MOV(OffsetAddr(dataWidth, Reg(Rbp, QWord), Stack.getOffset(qn)), Imm(value))
      }

      case Fst(lValue) => fstSndAddress(lValue, asmBuilder); movRegOrImmToMem(R10, qn, dataWidth)
      case Snd(lValue) => fstSndAddress(lValue, asmBuilder, 8); movRegOrImmToMem(R10, qn, dataWidth)

      case ArrayLiter(elems) => {
        t match
          case StringType => mallocArrayLiter(elems, ArrayType(CharType, 1), asmBuilder); movRegOrImmToMem(Rax, qn, dataWidth)
          case _ => mallocArrayLiter(elems, t.asInstanceOf[ArrayType], asmBuilder); movRegOrImmToMem(Rax, qn, dataWidth)
      }

      case NewPair(fst, snd) => mallocNewPair(fst, snd, asmBuilder); movRegOrImmToMem(Rax, qn, dataWidth)
      case Call(qnFunc: QualifiedFunc, args) => callGen(qnFunc, args, asmBuilder); movRegOrImmToMem(Rax, qn, dataWidth))
  }  

  // Allocates memory for an array literal and generates corresponding assembly instructions.
  def mallocArrayLiter(
    elems: List[Expr], 
    arrayType: ArrayType, 
    asmBuilder: Builder[Instr, List[Instr]]
  ) = {
    val arrayTypeSize = if arrayType.d != 1 then typeToSize(arrayType) else typeToSize(arrayType.t)
    asmBuilder += MOV(Reg(Rdi, DWord), Imm(arrayTypeSize*(elems.size) + 4))
    malloc(asmBuilder)

    asmBuilder.addAll(
      List(
        MOV(OffsetAddr(MemOpModifier.DWordPtr, Reg(Rax, QWord)), Imm(elems.size)),
        ADD(Reg(Rax, QWord), Imm(4))
      )
    )
    
    elems.indices.map(i => {
      asmBuilder += MOV(OffsetAddr(arrayTypeSize, Reg(Rax, QWord), i*arrayTypeSize), exprGen(elems(i), asmBuilder))
    })
    Reg(Rax, QWord)
  }

  // Allocates memory for a new pair and initializes it with the given expressions.
  def mallocNewPair(fst: Expr, snd: Expr, asmBuilder: Builder[Instr, List[Instr]]) = {
    val (fstSize, sndSize) = (8, 8)
    asmBuilder += MOV(Reg(Rdi, DWord), Imm(fstSize+sndSize))
    malloc(asmBuilder)
    asmBuilder += MOV(OffsetAddr(fstSize, Reg(Rax, QWord)), exprGen(fst, asmBuilder))
    asmBuilder += MOV(OffsetAddr(sndSize, Reg(Rax, QWord), sndSize), exprGen(snd, asmBuilder))
    Reg(Rax, QWord)
  }

  // Generates assembly instructions for array element access
  def repeatAccessArray(
    qn: QualifiedName, 
    index: List[Expr], 
    asmBuilder: Builder[Instr, List[Instr]]
  ): (Int, MemAddr) = {
    val ArrayType(t, d) = qn.t.asInstanceOf[ArrayType]
    var dimensionAccess = 0
    var pointer: MemAddr = OffsetAddr(MemOpModifier.QWordPtr, Reg(Rbp, QWord), Stack.getOffset(qn))
    
    if (index.size > 1) {
      for (i <- 0 until index.size - 1) {
        pointer = arrayIndexAccess(pointer, index(i), ArrayType(t, d - dimensionAccess), asmBuilder)._2
        dimensionAccess += 1
      }
    }

    val finalType = if (d - dimensionAccess == 1) t else ArrayType(t, d - dimensionAccess)
    arrayIndexAccess(pointer, index.last, finalType, asmBuilder)
  }

  // Generates assembly instructions for a function call
  def readFunc(dataWidth: Int,asmBuilder: Builder[Instr, List[Instr]]) = {
    val formatMode = if dataWidth == 1 then "c" else "d"
    asmBuilder.addAll(pushRbp)
    asmBuilder.addAll(
      List(
        AND(Reg(Rsp, QWord), Imm(STACK_ALIGN)),
        SUB(Reg(Rsp, QWord), Imm(-STACK_ALIGN)),
        MOV(OffsetAddr(dataWidth, Reg(Rsp, QWord)), Reg(Rdi, dataWidth)),
        LEA(Reg(Rsi, QWord), OffsetAddr(MemOpModifier.QWordPtr, Reg(Rsp, QWord))),
        LEA(Reg(Rdi, QWord), StringAddr(formatMode)),
        MOV(Reg(Rax, Byte), Imm(0)),
        CALL(Label("scanf@plt")),
        MOV(Reg(Rax, dataWidth), OffsetAddr(dataWidth, Reg(Rsp, QWord))),
        ADD(Reg(Rsp, QWord), Imm(-STACK_ALIGN)),
      )
    )
    asmBuilder.addAll(popRbp)
  }

  // Generates assembly instructions for read call (for inputs)
  def readGen(lValue: LValue, asmBuilder: Builder[Instr, List[Instr]]) = {
    (lValue: @unchecked) match
      case qn: QualifiedName => {
        val dataWidth = typeToSize(qn.t)
        asmBuilder += movQnToReg(Rdi, qn, dataWidth) 
        readFunc(dataWidth, asmBuilder)
        asmBuilder += movRegOrImmToMem(Rax, qn, dataWidth) 
      }

      case ArrayElem(qn: QualifiedName, index) => {
        val arrayBaseType = qn.t.asInstanceOf[ArrayType].t
        val arrayPointer = OffsetAddr(MemOpModifier.QWordPtr, Reg(Rbp, QWord), Stack.getOffset(qn))
        val (dataWidth, pointer) =  arrayIndexAccess(arrayPointer, index.last, arrayBaseType, asmBuilder)
        asmBuilder += MOV(Reg(Rdi, dataWidth), pointer)
        readFunc(dataWidth, asmBuilder)
        asmBuilder += MOV(pointer, Reg(Rax, dataWidth))
      }

      case Fst(lValue) => readPair(lValue, asmBuilder, 0)
      case Snd(lValue) => readPair(lValue, asmBuilder , 8)
    }

  // Generates assembly instructions for accessing a pair element
  def fstSndAddress(
    lValue: LValue, 
    asmBuilder: Builder[Instr, List[Instr]], 
    fstOrSnd: Int = 0
  ): Reg = {
    val storeInstrs = List(
      CMP(Reg(10, QWord), Imm(0)),
      JCond(Cond.E, Label("_errNull")),
      MOV(Reg(10, QWord), OffsetAddr(MemOpModifier.QWordPtr, Reg(R10, QWord), fstOrSnd))
    )
    (lValue: @unchecked) match
      case qn: QualifiedName => {
        asmBuilder += movQnToReg(R10, qn)
        asmBuilder.addAll(storeInstrs)
      }
      case ArrayElem(qn: QualifiedName, index) => {
        val (dataWidth, pointer) = repeatAccessArray(qn, index, asmBuilder)
        asmBuilder += MOV(Reg(10, dataWidth), pointer)
        asmBuilder.addAll(storeInstrs)
      }
      case Fst(lValue) => {
        fstSndAddress(lValue, asmBuilder)
        asmBuilder.addAll(storeInstrs)
      }
      case Snd(lValue) => {
        fstSndAddress(lValue, asmBuilder, 8)
        asmBuilder.addAll(storeInstrs)
      }
      Reg(R10, QWord)
    }

  // Generates assembly instructions for reading a pair
  def readPair(lValue: LValue, asmBuilder: Builder[Instr, List[Instr]], fstOrSnd: Int) = {
    (lValue: @unchecked) match
      case qn: QualifiedName => {
        val dataWidth = typeToSize(qn.t)
        fstSndAddress(lValue, asmBuilder, fstOrSnd)
        asmBuilder += MOV(Reg(Rdi, dataWidth), OffsetAddr(dataWidth, Reg(R10, QWord)))
        readFunc(dataWidth, asmBuilder)
        asmBuilder += MOV(OffsetAddr(dataWidth, Reg(R10, QWord)), Reg(Rax, dataWidth))
      }
      case ArrayElem(qn: QualifiedName, index) => {
        val dataWidth = typeToSize(qn.t.asInstanceOf[ArrayType].t)
        fstSndAddress(lValue, asmBuilder, fstOrSnd)
        asmBuilder += MOV(Reg(Rdi, dataWidth), OffsetAddr(dataWidth, Reg(R10, QWord)))
        readFunc(dataWidth, asmBuilder)
        asmBuilder += MOV(OffsetAddr(dataWidth, Reg(R10, QWord)), Reg(Rax, dataWidth))
      }
  }

  // Generates assembly instructions for program exit
  def exitGen(expr: Expr, asmBuilder: Builder[Instr, List[Instr]]) = {
    asmBuilder += MOV(Reg(Rdi, DWord), exprGen(expr, asmBuilder))
    asmBuilder ++= pushRbp
    asmBuilder.addAll(
      List(
        AND(Reg(Rsp, QWord), Imm(STACK_ALIGN)),
        CALL(Label("exit@plt"))
      )
    )
    asmBuilder ++= popRbp
  }

  // Generates assembly instructions for free call
  def freeGen(expr: Expr, asmBuilder:Builder[Instr, List[Instr]]) = {
    (expr: @unchecked) match
      case qn: QualifiedName if qn.t.isInstanceOf[ArrayType] => {
        asmBuilder.addAll(
          List(
            movQnToReg(Rdi, qn),
            SUB(Reg(Rdi, QWord), Imm(4))
          )
        )
        asmBuilder ++= pushRbp
        asmBuilder += AND(Reg(Rsp, QWord), Imm(-16))
        asmBuilder += CALL(Label("free@plt"))
        asmBuilder ++= popRbp
        asmBuilder += MOV(Reg(Rax, QWord), Imm(0))
      }
      case qn: QualifiedName if qn.t.isInstanceOf[PairType] => {
        asmBuilder += movQnToReg(Rdi, qn)
        asmBuilder ++= pushRbp
        asmBuilder.addAll(
          List(
            AND(Reg(Rsp, QWord), Imm(-16)),
            CMP(Reg(Rdi, QWord), Imm(0)),
            JCond(Cond.E, Label("_errNull")),
            CALL(Label("free@plt")),
            MOV(Reg(Rax, QWord), Imm(0))
          )
        )
        asmBuilder ++= popRbp
      }
  }

  def scopeGen(stmts: List[Stmt], asmBuilder: Builder[Instr, List[Instr]]) = {
    val prevFrame = Stack.frames.last
    Stack.frames += StackFrame(stmts, prevFrame.identTable, prevFrame.absoluteDepth())
    asmBuilder += SUB(Reg(Rsp, QWord), Imm(-Stack.frames.last.currentDepth))
    stmts.map(stmtGen(_, asmBuilder))
    asmBuilder += ADD(Reg(Rsp, QWord), Imm(-Stack.frames.last.currentDepth))
    Stack.frames.dropRightInPlace(1)
  }

  // Generates assembly instructions for return call
  def returnGen(expr: Expr, asmBuilder: Builder[Instr, List[Instr]]) = {
    val reg = exprGenRegister(expr, asmBuilder)
    asmBuilder += MOV(Reg(Rax, reg.dataWidth), reg)
    asmBuilder += ADD(Reg(Rsp, QWord), Imm(-Stack.frames.last.currentDepth))
    asmBuilder ++= popRbp
    asmBuilder += RET
  }

  def callGen(qn: QualifiedFunc, args: List[Expr], asmBuilder: Builder[Instr, List[Instr]]) = {
    asmBuilder += PUSH(Reg(Rbp, QWord))
    asmBuilder += MOV(Reg(Rbp, QWord), Reg(Rsp, QWord))
    val spaceNeeded = qn.paramTypes.foldLeft(0)(_ + typeToSize(_))
    asmBuilder += SUB(Reg(Rsp, QWord), Imm(spaceNeeded))
    var currentDepth = 0
    val sortedArgs = args.sortBy(getExprType(_).toString())
    val sortedParams = qn.paramTypes.sortBy(_.toString())
    for ((paramType, i) <- sortedParams.zipWithIndex) {
      println(qn.paramTypes.sortBy(_.toString()))
      val dataWidth = typeToSize(paramType)
      currentDepth -= dataWidth
      asmBuilder += MOV(OffsetAddr(dataWidth, Reg(Rbp, QWord), currentDepth), exprGen(sortedArgs(i), asmBuilder))
    }
    asmBuilder += CALL(Label(s"wacc_${qn.funcName}"))
    asmBuilder += ADD(Reg(Rsp, QWord), Imm(spaceNeeded))
    asmBuilder ++= popRbp
    Reg(Rax, typeToSize(qn.t))
  }

  def printGen(expr: Expr, asmBuilder: Builder[Instr, List[Instr]]) = {
    val t = getExprType(expr).get
    if (t == BoolType) {
      val n = localLabelCounter
      localLabelCounter += 2
      asmBuilder += CMP(exprGenRegister(expr, asmBuilder), Imm(0))
      asmBuilder += JCond(Cond.NE, WhileIfLabel(n))
      asmBuilder += LEA(Reg(R10, QWord), StringAddr("false")) // FALSE
      asmBuilder += JCond(Cond.AL, WhileIfLabel(n+1))
      asmBuilder += WhileIfLabel(n)
      asmBuilder += LEA(Reg(R10, QWord), StringAddr("true")) // TRUE
      asmBuilder += WhileIfLabel(n+1)
      asmBuilder ++= pushRbp
      printString(Reg(R10, QWord), formatMap(StringType), asmBuilder) 
    } else {
    val formatMode = formatMap(t)
    val reg = exprGenRegister(expr, asmBuilder) 
    val dataWidth = reg.dataWidth
    asmBuilder ++= pushRbp
    if (t == StringType || t == ArrayType(CharType, 1)) 
    then printString(reg, formatMode, asmBuilder) 
    else printNonString(reg, dataWidth, formatMode, asmBuilder)
    }
  }



  val (pushRbp) = 
    List(
      PUSH(Reg(Rbp, QWord)),
      MOV(Reg(Rbp, QWord), Reg(Rsp, QWord))
    )

  val popRbp = 
    List(
      MOV(Reg(Rsp, QWord), Reg(Rbp, QWord)),
      POP(Reg(Rbp, QWord)),
    )

  // The below are for C style printing ie
  // printf("%d", num) to print an int in C
  def formatMap(t: Type): Int = {
    val p = 0x007025   // p for pointers
    val s = 0x732A2E25 // s for strings
    val c = 0x006325   // c for chars
    val d = 0x006425   // d for ints
    (t: @unchecked) match
      case PairType(t1, t2) => p
      case ArrayType(CharType, 1) => s
      case ArrayType(t, d) => p
      case IntType => d
      case StringType => s
      case BoolType => ???
      case CharType => c
  }

  // Generates assembly for while loops
  def whileGen(cond: Expr, loopStmts: List[Stmt], asmBuilder: Builder[Instr, List[Instr]]) = {
    val n = localLabelCounter
    // Added 2 since n and n+1 labels are reserved for this while's blocks (conditional jumps)
    localLabelCounter += 2 

    asmBuilder += JCond(Cond.AL, WhileIfLabel(n))
    asmBuilder += WhileIfLabel(n + 1)
    scopeGen(loopStmts, asmBuilder)

    asmBuilder += WhileIfLabel(n)
    exprGenRegister(cond, asmBuilder)
    asmBuilder += CMP(Reg(R10, Byte), Imm(1))
    asmBuilder += JCond(Cond.E, WhileIfLabel(n + 1))
  }

  // Generates assembly for if else statements
  def ifElseGen(cond: Expr, thenStmts: List[Stmt], elseStmts: List[Stmt], asmBuilder: Builder[Instr, List[Instr]]) = {
    val n = localLabelCounter
    // Added 2 since n and n+1 are reserved for this if-else blocks (conditional jumps)
    localLabelCounter += 2 

    exprGenRegister(cond, asmBuilder)
    asmBuilder += CMP(Reg(R10, Byte), Imm(1))
    asmBuilder += JCond(Cond.E, WhileIfLabel(n))
    scopeGen(elseStmts, asmBuilder) //else
    asmBuilder += JCond(Cond.AL, WhileIfLabel(n+1))
    asmBuilder += WhileIfLabel(n) 
    scopeGen(thenStmts, asmBuilder) //if condition is true
    asmBuilder += WhileIfLabel(n+1)
  }

  // Generates assembly for non-string prints
  def printNonString(reg: Reg, dataWidth: DataWidth, formatMode: Int, asmBuilder: Builder[Instr, List[Instr]]) = {
    asmBuilder.addAll(
      List(
        MOV(Reg(Rsi, dataWidth), reg),
        SUB(Reg(Rsp, QWord), Imm(8)),
        MOV(OffsetAddr(MemOpModifier.QWordPtr, Reg(Rsp, QWord)), Imm(formatMode)),
        MOV(Reg(Rdi, QWord), Reg(Rsp, QWord)),
        MOV(Reg(Rax, Byte), Imm(0)),
        AND(Reg(Rsp, QWord), Imm(STACK_ALIGN)),
        CALL(Label("printf@plt")),
        MOV(Reg(Rdi, QWord), Imm(0)),
        CALL(Label("fflush@plt")),
        ADD(Reg(Rsp, QWord), Imm(8))
      )
    )
  }

  // Generates assembly for string prints
  def printString(reg: Reg, formatMode: Int, asmBuilder: Builder[Instr, List[Instr]]) = {
    asmBuilder.addAll(
      List(
        MOV(Reg(Rdx, QWord), reg),
        MOV(Reg(Rsi, DWord), OffsetAddr(MemOpModifier.DWordPtr, Reg(Rdx, QWord), -4)),
        SUB(Reg(Rsp, QWord), Imm(8)),
        MOV(OffsetAddr(MemOpModifier.QWordPtr, Reg(Rsp, QWord)), Imm(formatMode)),
        MOV(Reg(Rdi, QWord), Reg(Rsp, QWord)),
        MOV(Reg(Rax, Byte), Imm(0)),
        AND(Reg(Rsp, QWord), Imm(STACK_ALIGN)),
        CALL(Label("printf@plt")),
        MOV(Reg(Rdi, QWord), Imm(0)),
        CALL(Label("fflush@plt")),
        ADD(Reg(Rsp, QWord), Imm(8))
      )
    )
  }

  // Generates assembly for malloc call
  def malloc(asmBuilder: Builder[Instr, List[Instr]]) = {
    asmBuilder ++= pushRbp
    asmBuilder.addAll(
      List(
        AND(Reg(Rsp, QWord), Imm(STACK_ALIGN)),
        CALL(Label("malloc@plt")),
        CMP(Reg(Rax, QWord), Imm(0)),
        JCond(Cond.E, Label("_errOutOfMem"))
      )
    )
    asmBuilder ++= popRbp
  }

}

