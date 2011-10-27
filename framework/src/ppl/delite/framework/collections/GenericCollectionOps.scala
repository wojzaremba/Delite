package ppl.delite.framework.collections



import java.io.PrintWriter
import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.datastruct.scala.DeliteCollection
import scala.virtualization.lms.common._
import ppl.delite.framework.ops.DeliteOpsExp



trait GenericCollectionOps extends Variables {
  
  /** Definition of the CanBuild evidence */
  trait CanBuild[-Coll, -S, +Target] {
    def alloc(source: Rep[Coll]): Rep[Target]
    def emptyAlloc(source: Rep[Coll]): Rep[Target]
    def emitterFactory(source: Rep[Coll]): EmitterFactory
    def noPrealloc: Boolean
  }
  
}


trait GenericCollectionGen {
}


trait EmitterFactory {
  def needsCombine: Boolean
  def needsPostProcess: Boolean
  def needsPostProcess2: Boolean
  
  def scala: ScalaEmitter
}


/** Collection construction emitter. Given that the collection can be emitted for a given
 *  platform, uses the evidence to emit the code.
 */
trait ScalaEmitter {
  def emitBufferDefs(kernelname: String, basename: String, elemtype: String)(implicit stream: PrintWriter)
  def emitInitSubActivation(basename: String, activname: String, chunkIdxVar: String, numChunksVar: String)(implicit stream: PrintWriter)
  def emitAddToBuffer(basename: String, activname: String, elemname: String)(implicit stream: PrintWriter)
  def emitAddToDataStructure(prefixSym: String, basename: String, elemname: String)(implicit stream: PrintWriter)
  def emitPostCombine(basename: String, activname: String, lhsname: String)(implicit stream: PrintWriter)
  def emitPostProcInit(basename: String, activname: String)(implicit stream: PrintWriter)
  def emitPostProcess(basename: String, activname: String)(implicit stream: PrintWriter)
  def emitPostCombine2(basename: String, activname: String, lhsname: String)(implicit stream: PrintWriter)
  def emitPostProcInit2(basename: String, activname: String)(implicit stream: PrintWriter)
  def emitPostProcess2(basename: String, activname: String)(implicit stream: PrintWriter)
  def emitDataDeclaration(basename: String, activname: String, dataname: String)(implicit stream: PrintWriter)
  def emitInitializeDataStructure(basename: String, activname: String, collectionname: String, dataname: String)(implicit stream: PrintWriter)
}

