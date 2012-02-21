package ppl.dsl.deliszt

import ppl.dsl.deliszt.datastruct.scala._
import ppl.dsl.deliszt.MetaInteger._

trait LanguageImplOps {
  this: DeLisztExp =>


}

trait LanguageImplOpsStandard extends LanguageImplOps {
  this: DeLisztCompiler with DeLisztExp =>
}
