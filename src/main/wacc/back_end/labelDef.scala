package wacc.back_end

import scala.collection.mutable.{Builder}



case class FuncLabelDef(
  name: String,
  instructions: Builder[Instr, List[Instr]]
  )
