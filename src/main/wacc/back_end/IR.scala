package wacc.back_end

import wacc.front_end._
import RegName._
import DataWidth._
import ExprGen._
// At the start of each assembly file
// .intel_syntax noprefix
// .globl main
object IR {

  def generateIR(prog: Prog): (List[Section], List[LabelDef]) = {
    ???
  }
  val sections = List.newBuilder[Section] // each section is .rodata (read only)
                                          // what follows is .text
  val labelDefs = List.newBuilder[LabelDef] // each function may contain labels/jumping points (ie for loops)

  def generateSection(prog: Prog) = {
    // This will generate the boilerplate at the beginning of the asm file and
    // the string+len of string stuff to go in that section
  }

  def generate(prog: Prog) = prog match {
    case Prog(_, main) =>
      mainGenerate(main)
  }

  def mainGenerate(stmts: List[Stmt]) = {

  }

  def stmtGen(stmt: Stmt): List[Instr] = stmt match {
    case Skip => Nil
    case Read(lValue) => ???
    case Free(expr) => ???
    case Return(expr) => ???
    case Exit(expr) =>
      List(
        PUSH(Reg(Rbp, DWord)),
        PUSH(Reg(Rbx, DWord)),
        MOV(Reg(Rbp, DWord), Reg(Rsp, DWord))) ++
        exprGen(expr) ++
        List(MOV(Reg(Rdi, DWord), Reg(R10)))
        // CALL(Label())

    case Print(expr) => ???
    case Println(expr) => ???
    case WhileDo(condition, stmts) => ???
    case IfElse(condition, thenStmts, elseStmts) => ???
    case Assgn(t, identifier: QualifiedName, rValue) => assgnGen(t, identifier, rValue)
    case ReAssgn(lValue, rValue) => ???
    case Scope(stmts) => ???
  }

  def assgnGen(t: Type, identifier: QualifiedName, rValue: RValue) : List[Instr] = {
    rValue match
      case expr: Expr => ???
      case Call(ident, args) => ???
      case ArrayLiter(elems) => ???
      case NewPair(fst, snd) => ??? 
      case Fst(lValue) => ???
      case Snd(lValue) => ???
  }



}

// Prog(
//   List(
//     Func(
//       IntType,
//       QualifiedFunc(
//         t=IntType,
//         funcName=fibonacci,
//         paramNum=1,
//         paramTypes=List(
//           IntType
//         )
//       ),
//       List(
//         Param(
//           IntType,
//           QualifiedName(
//             name=n,
//             num=0,
//             t=IntType
//           )
//         )
//       ),
//       List(
//         IfElse(
//           LessE(
//             QualifiedName(
//               name=n,
//               num=0,
//               t=IntType
//             ),
//             IntLiteral(
//               1
//             )
//           ),
//           List(
//             Return(
//               QualifiedName(
//                 name=n,
//                 num=0,
//                 t=IntType
//               )
//             )
//           ),
//           List(
//             Skip
//           )
//         ),
//         Assgn(
//           IntType,
//           QualifiedName(
//             name=f1,
//             num=0,
//             t=IntType
//           ),
//           Call(
//             QualifiedFunc(
//               t=IntType,
//               funcName=fibonacci,
//               paramNum=1,
//               paramTypes=List(
//                 IntType
//               )
//             ),
//             List(
//               Sub(
//                 QualifiedName(
//                   name=n,
//                   num=0,
//                   t=IntType
//                 ),
//                 IntLiteral(
//                   1
//                 )
//               )
//             )
//           )
//         ),
//         Assgn(
//           IntType,
//           QualifiedName(
//             name=f2,
//             num=0,
//             t=IntType
//           ),
//           Call(
//             QualifiedFunc(
//               t=IntType,
//               funcName=fibonacci,
//               paramNum=1,
//               paramTypes=List(
//                 IntType
//               )
//             ),
//             List(
//               Sub(
//                 QualifiedName(
//                   name=n,
//                   num=0,
//                   t=IntType
//                 ),
//                 IntLiteral(
//                   2
//                 )
//               )
//             )
//           )
//         ),
//         Return(
//           Add(
//             QualifiedName(
//               name=f1,
//               num=0,
//               t=IntType
//             ),
//             QualifiedName(
//               name=f2,
//               num=0,
//               t=IntType
//             )
//           )
//         )
//       )
//     )
//   ),
//   List(
//     Println(
//       StringLiteral(
//         Thisprogramcalculatesthenthfibonaccinumberrecursively.
//       )
//     ),
//     Print(
//       StringLiteral(
//         Pleaseentern(
//           shouldnotbetoolarge
//         ):
//       )
//     ),
//     Assgn(
//       IntType,
//       QualifiedName(
//         name=n,
//         num=1,
//         t=IntType
//       ),
//       IntLiteral(
//         0
//       )
//     ),
//     Read(
//       QualifiedName(
//         name=n,
//         num=1,
//         t=IntType
//       )
//     ),
//     Print(
//       StringLiteral(
//         Theinputnis
//       )
//     ),
//     Println(
//       QualifiedName(
//         name=n,
//         num=1,
//         t=IntType
//       )
//     ),
//     Print(
//       StringLiteral(
//         Thenthfibonaccinumberis
//       )
//     ),
//     Assgn(
//       IntType,
//       QualifiedName(
//         name=result,
//         num=0,
//         t=IntType
//       ),
//       Call(
//         QualifiedFunc(
//           t=IntType,
//           funcName=fibonacci,
//           paramNum=1,
//           paramTypes=List(
//             IntType
//           )
//         ),
//         List(
//           QualifiedName(
//             name=n,
//             num=1,
//             t=IntType
//           )
//         )
//       )
//     ),
//     Println(
//       QualifiedName(
//         name=result,
//         num=0,
//         t=IntType
//       )
//     )
//   )
// )