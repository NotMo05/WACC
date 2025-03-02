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
    
  def generateIR(prog: Prog): (List[StringInfo], List[FuncLabelDef]) = {
    val sections = generateROData(prog.main) :: prog.funcs.map(func => generateROData(func.stmts))
    val funcLabelDefs = funcGenerate(prog.main) :: prog.funcs.map(func => funcGenerate(func.stmts, func.identifier.identifier, func.params))
    val errorList = List("_errBadChar","_errNull","_errOutOfMem","_errOutOfBounds","_errOverflow","_errDivZero")
    val allFuncLabelDefs: List[FuncLabelDef] = funcLabelDefs ++ errorList.map(generateErrorLabels(_))

    // each section is .rodata (read only)
    // what follows is .text
    return(sections.flatten, allFuncLabelDefs)
  }

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

  // Data width defaults to QWord for moving pointers in registers
  def movQnToReg(destReg: RegName, qn: QualifiedName, dataWidth: DataWidth = QWord) =
   MOV(Reg(destReg, dataWidth), OffsetAddr(dataWidth, Reg(Rbp, QWord), Stack.getOffset(qn)))
  def movRegOrImmToMem(srcRegOrImm: RegName | Imm | Reg, qn: QualifiedName, dataWidth: DataWidth = QWord) = {
    srcRegOrImm match
      case reg: RegName => MOV(OffsetAddr(dataWidth, Reg(Rbp, QWord), Stack.getOffset(qn)), Reg(reg, dataWidth))
      case imm: Imm => MOV(OffsetAddr(dataWidth, Reg(Rbp, QWord), Stack.getOffset(qn)), imm)
      case reg: Reg => MOV(OffsetAddr(dataWidth, Reg(Rbp, QWord), Stack.getOffset(qn)), reg)
  }

  def generateROData(stmts: List[Stmt]): List[StringInfo] = { // CHANGED TO LIST OF SECTIONS FOR NOW
    // This will generate the boilerplate labels at the beginning of assembly file that contain string data
    // Including raw string, string length and the label number 
                // Remind me to change stringGen in ExprGen
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

  def ROLValueHelper(lvalue: LValue): List[StringInfo] = {
    return lvalue match
      case Fst(lValue: LValue) => ROLValueHelper(lValue)
      case Snd(lValue: LValue) => ROLValueHelper(lValue)
      case _ => Nil
}

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

  def funcGenerate(stmts: List[Stmt], funcName: String = "", params: List[Param] = Nil): FuncLabelDef = {
    val assignedParams = if !params.isEmpty then {
      params.reverse.map( p =>
        p match
          case Param(t, qn: QualifiedName) => {
            Assgn(t, qn, IntLiteral(0))
          }
      )
    } else Nil
    val asmBuilder = List.newBuilder[Instr]
    Stack.initialise(assignedParams ++ stmts)
    asmBuilder += PUSH(Reg(Rbp, QWord))
    asmBuilder += MOV(Reg(Rbp, QWord), Reg(Rsp, QWord))
    asmBuilder += SUB(Reg(Rsp, QWord), Imm(-Stack.frames.last.currentDepth))
    println(funcName)
    stmts.map(stmtGen(_, asmBuilder))
    println("WE FINSIHED REC")
    
    val funcLabel = if (funcName == "") {
      asmBuilder += MOV(Reg(Rax, QWord), Imm(0))
      "main"
    } else s"wacc_$funcName"

    asmBuilder += ADD(Reg(Rsp, QWord), Imm(-Stack.frames.last.currentDepth)) 
    asmBuilder += POP(Reg(Rbp, QWord)) 
    asmBuilder += RET 
    
    return FuncLabelDef(funcLabel, asmBuilder)
  }

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

  def reassgnGen(lValue: LValue, rValue: RValue, asmBuilder: Builder[Instr, List[Instr]]) = {
    (lValue: @unchecked) match
        case qn: QualifiedName => assgnGen(qn, rValue, asmBuilder, qn.t)
        case ArrayElem(qn: QualifiedName, index) => {
          val (dataWidth, pointer) = repeatAccessArray(qn, index, asmBuilder)
          print(dataWidth)
          asmBuilder += MOV(pointer, rValueGen(rValue, asmBuilder, qn.t))
          asmBuilder += CDQ
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

  def arrayIndexAccess(
    pointerAddress: MemAddr, 
    index: Expr, t: Type, 
    asmBuilder: Builder[Instr, List[Instr]]): 
    (Int, MemAddr) = 
  {
    asmBuilder += MOV(Reg(R9, QWord), pointerAddress)
    val dataWidth = typeToSize(t)
    asmBuilder.addAll(
      List(
        MOV(Reg(R8, DWord), exprGen(index, asmBuilder)),
        CMP(Reg(R8, DWord), Imm(0)),
        JCond(Cond.L, Label("_errOutOfBounds")),
        CMP(Reg(R8, DWord), OffsetAddr(MemOpModifier.DWordPtr, Reg(R9, QWord), -4)),
        JCond(Cond.GE, Label("_errOutOfBounds"))
      )
    )
    (dataWidth, RegScale(dataWidth, Reg(R9, QWord), typeToSize(t), Reg(R8, QWord)))
  }

  def rValueGen(rValue: RValue, asmBuilder: Builder[Instr, List[Instr]], t: Type = Undefined): (Reg | Imm) = {
    (rValue: @unchecked) match
      case expr: Expr => exprGen(expr, asmBuilder)
      case Fst(lValue) => fstSndAddress(lValue, asmBuilder)
      case Snd(lValue) => fstSndAddress(lValue, asmBuilder, 8)
      case ArrayLiter(elems) => mallocArrayLiter(elems, t.asInstanceOf[ArrayType], asmBuilder)
      case NewPair(fst, snd) => mallocNewPair(fst, snd, asmBuilder)
      case Call(qn: QualifiedFunc, args) => callGen(qn, args, asmBuilder)
  }

  def assgnGen(qn: QualifiedName, rValue: RValue, asmBuilder: Builder[Instr, List[Instr]], t: Type = Undefined) = {
    val dataWidth = Stack.typeToSize(t)
    print(t)
    asmBuilder += ((rValue: @unchecked) match
      case expr: Expr => movRegOrImmToMem(exprGen(expr, asmBuilder), qn, dataWidth)
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

  def mallocArrayLiter(elems: List[Expr], arrayType: ArrayType, asmBuilder: Builder[Instr, List[Instr]]) = {

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

  def mallocNewPair(fst: Expr, snd: Expr, asmBuilder: Builder[Instr, List[Instr]]) = {
    val (fstSize, sndSize) = (8, 8)
    asmBuilder += MOV(Reg(Rdi, DWord), Imm(fstSize+sndSize))
    malloc(asmBuilder)
    asmBuilder += MOV(OffsetAddr(fstSize, Reg(Rax, QWord)), exprGen(fst, asmBuilder))
    asmBuilder += MOV(OffsetAddr(sndSize, Reg(Rax, QWord), sndSize), exprGen(snd, asmBuilder))
    Reg(Rax, QWord)
  }


  def repeatAccessArray(qn: QualifiedName, index: List[Expr], asmBuilder: Builder[Instr, List[Instr]]): (Int, MemAddr) = {
    val ArrayType(t,d) = qn.t.asInstanceOf[ArrayType]
    var dimensionAccess = 0
    var pointer: MemAddr = OffsetAddr(MemOpModifier.QWordPtr, Reg(Rbp, QWord), Stack.getOffset(qn))
    print(index.size)
    for (i <- 1 to index.size - 1) {
      pointer = arrayIndexAccess(pointer, index(i), ArrayType(t, d - dimensionAccess), asmBuilder)._2
      dimensionAccess -= 1
    }
    val finalType = 
    if d - dimensionAccess == 0 then t
    else ArrayType(t, d - dimensionAccess)
    arrayIndexAccess(pointer, index.last, finalType, asmBuilder)
  }

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

  def readGen(lValue: LValue, asmBuilder: Builder[Instr, List[Instr]]) = {
    (lValue: @unchecked) match
      case qn: QualifiedName => {
        val dataWidth = typeToSize(qn.t)
        print(dataWidth, "AAAAAAA")
        asmBuilder += movQnToReg(Rdi, qn, dataWidth) 
        print(movQnToReg(Rdi, qn, dataWidth))
        print("ABOUT TO READ")
        readFunc(dataWidth, asmBuilder)
        print("READ")
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

  def fstSndAddress(lValue: LValue, asmBuilder: Builder[Instr, List[Instr]], fstOrSnd: Int = 0): Reg = {
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

  def returnGen(expr: Expr, asmBuilder: Builder[Instr, List[Instr]]) = {
    val reg = exprGenRegister(expr, asmBuilder)
    asmBuilder += MOV(Reg(Rax, reg.dataWidth), reg)
    asmBuilder += ADD(Reg(Rsp, QWord), Imm(-Stack.frames.last.currentDepth))
    asmBuilder += POP(Reg(Rbp, QWord))
    asmBuilder += RET
  }

  def callGen(qn: QualifiedFunc, args: List[Expr], asmBuilder: Builder[Instr, List[Instr]]) = {
    asmBuilder += PUSH(Reg(Rbp, QWord))
    asmBuilder += MOV(Reg(Rbp, QWord), Reg(Rsp, QWord))
    val spaceNeeded = qn.paramTypes.foldLeft(0)(_ + typeToSize(_))
    asmBuilder += SUB(Reg(Rsp, QWord), Imm(spaceNeeded))
    var currentDepth = 0
    for ((paramType, i) <- qn.paramTypes.zipWithIndex) {
      val dataWidth = typeToSize(paramType)
      currentDepth -= dataWidth
      asmBuilder += MOV(OffsetAddr(dataWidth, Reg(Rbp, QWord), currentDepth), exprGen(args(i), asmBuilder))
    }
    asmBuilder += CALL(Label(s"wacc_${qn.funcName}"))
    asmBuilder += ADD(Reg(Rsp, QWord), Imm(spaceNeeded))
    asmBuilder += POP(Reg(Rbp, QWord))
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

  def formatMap(t: Type): Int = {
    // The below are for C style printing ie
    // printf("%d", num) to print an int in C
    val p = 0x007025 // p for pointers
    val s = 0x732A2E25 // s for strings
    val c = 0x006325 // c for chars
    val d = 0x006425 // d for ints
    (t: @unchecked) match
      case PairType(t1, t2) => p
      case ArrayType(CharType, 1) => s
      case ArrayType(t, d) => p
      case IntType => d
      case StringType => s
      case BoolType => ???
      case CharType => c
  }

  def whileGen(cond: Expr, loopStmts: List[Stmt], asmBuilder: Builder[Instr, List[Instr]]) = {
    val n = localLabelCounter
    localLabelCounter += 2 // because n and n+1 are reserved for this while

    asmBuilder += JCond(Cond.AL, WhileIfLabel(n))
    asmBuilder += WhileIfLabel(n + 1)
    scopeGen(loopStmts, asmBuilder)

    asmBuilder += WhileIfLabel(n)
    exprGenRegister(cond, asmBuilder)
    asmBuilder += CMP(Reg(R10, Byte), Imm(1))
    asmBuilder += JCond(Cond.E, WhileIfLabel(n + 1))
  }

  def ifElseGen(cond: Expr, thenStmts: List[Stmt], elseStmts: List[Stmt], asmBuilder: Builder[Instr, List[Instr]]) = {
    val n = localLabelCounter
    localLabelCounter += 2 // because n, and n+2, are reserved for this ifelse block

    exprGenRegister(cond, asmBuilder)
    asmBuilder += CMP(Reg(R10, Byte), Imm(1))
    asmBuilder += JCond(Cond.E, WhileIfLabel(n))
    scopeGen(elseStmts, asmBuilder) //else
    asmBuilder += JCond(Cond.AL, WhileIfLabel(n+1))
    asmBuilder += WhileIfLabel(n) 
    scopeGen(thenStmts, asmBuilder) //if condition is true
    asmBuilder += WhileIfLabel(n+1)
  }

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

