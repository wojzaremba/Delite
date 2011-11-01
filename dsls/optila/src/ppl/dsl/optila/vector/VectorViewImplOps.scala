package ppl.dsl.optila.vector

import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{BaseExp, Base}
import ppl.delite.framework.datastructures._
import ppl.dsl.optila.{VectorView}
import ppl.dsl.optila.{OptiLALift, OptiLACompiler, OptiLA}

trait VectorViewImplOps { this: OptiLA with DeliteArrayOps =>
  def vectorview_update_impl[A:Manifest](v: Rep[VectorView[A]], a: Rep[DeliteArray[A]]): Rep[Unit]
}

trait VectorViewImplOpsStandard extends VectorViewImplOps {
  this: OptiLACompiler with OptiLALift with DeliteArrayOpsExp =>

  def vectorview_update_impl[A:Manifest](v: Rep[VectorView[A]], a: Rep[DeliteArray[A]]) = {
    var i = unit(0)
    while (i < a.length) {
      v(i) = a(i)    
      i += 1
    }    
  }
}