package wacc.back_end

import scala.collection.mutable.{Builder}


// Todo: rename func label def to label def
case class FuncLabelDef(
  name: String,
  instructions: Builder[Instr, List[Instr]]
  )
