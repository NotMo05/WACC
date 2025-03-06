package wacc.interpreter
import wacc.front_end.Prog

object Interpreter {


  def execute(prog: Prog): Unit = {
    prog.main.map(_ => println("test"))
  }



}