package epfl.mdarrays.staged

import _root_.scala.virtualization.lms.internal.GenericNestedCodegen
import _root_.scala.virtualization.lms.common._
import _root_.scala.virtualization.lms.internal._
import epfl.mdarrays.library.scala.MDArray
import java.io.PrintWriter

import ppl.delite.framework.ops.{DeliteOpsExp}
import ppl.delite.framework.codegen.delite.primitives.{DeliteScalaGenAllOverrides, DeliteScalaGenIfThenElse, DeliteScalaGenVariables}

trait BaseGenMDArray extends GenericNestedCodegen {

  val IR: MDArrayBaseExp
  import IR._

}


trait TypedGenMDArray extends BaseGenMDArray {

  val IR: TY.IR.type
  val TY: MDArrayTypingBubbleUp
  import IR.{Exp, Sym, Def}
  import TY.TypingConstraint

  def emitChecks(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) =
    for (constraint <- TY.getRuntimeChecks(sym))
      emitConstraint(constraint, "RuntimeCheck ")

  def emitConstraint(expr: TypingConstraint, ctrType: String)(implicit stream: PrintWriter) = {
    stream.println("// " + ctrType + ": " + expr.toString)
  }

  def emitShapeValue(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter): Unit = {
    stream.println("/* " + sym + " <= " + rhs + " */")
    stream.println("/*   ... " + TY.getTypingString(sym) + " */")
    /* Enable the following lines to get a trace of the execution:

        var newRhs = rhs.toString.replace('"','\'')
        if (newRhs.indexOf('\n') != -1)
          newRhs = newRhs.substring(0, newRhs.indexOf('\n'))
        stream.println("println(\"" + sym + " <= " + newRhs + "      " + TY.getTypingString(sym) +"\")")
        stream.println("println(\"" + sym + " <= " + newRhs + "      " + TY.getTypingString(sym) +"\")")
     */
  }
}

trait ScalaGenMDArray extends ScalaGenEffect with DeliteScalaGenIfThenElse with DeliteScalaGenVariables with TypedGenMDArray {

  // We cannot import eveything, else scala-virtualized will virtualize us :)
  import IR.{KnownAtCompileTime, KnownAtRuntime}
  import IR.{ToArray, ToList, ToValue, FromArray, FromList, FromValue, FromMDArrayList}
  import IR.{ToDim, ToShape, Sel, Cat, Reshape}
  import IR.{InfixOp, UnaryOp, Values, Where}
  import IR.{GenArrayWith, ModArrayWith, FoldArrayWith, WithNode}
  import IR.{ScalarOperatorApplication, ScalarOperatorWrapper }
  import IR.{ToString}
  import IR.{ReadMDArray, WriteMDArray}
  import IR.{StartTimer, StopTimer}
  import IR.IfThenElse
  import IR.PrintLn

  // Delite
  import IR.{DeliteWhile, DeliteIfThenElse, ReadVar, NewVar, Assign, Variable, deliteInputs, deliteResult}

  // Generic IR
  import IR.{Sym, Def, TP, Exp, Const, findDefinition, syms}
  // Typing
  import TY.{getShapeLength, getValueLength, getShapeValue, getValueValue}

  // Generate unique identifiers
  var currentIndex = 0
  def getNewIndex = { currentIndex += 1; currentIndex}

  // Mark values generated outside
  var emittedOutsideSyms: List[Sym[_]] = Nil
  var emittedInKernel = false

  // This function stores the action of the innermost with loop
  var withLoopAction: (String, String, String)=>Unit = (a, b, c)=> { sys.error("No with loop action set!") }

  def strip[A](m: Manifest[A]) : String = {
    if (m.erasure == classOf[MDArray[Any]])
      remap(m.typeArguments.head)
    else
      sys.error("Stripping MDArray on a Symbol that is not an MDArray!")
  }

  override def remap[A](m: Manifest[A]) : String = {
    if (m.erasure == classOf[MDArray[Any]])
      "(Array[Int], Array[" + remap(m.typeArguments.head) + "])"
    else
      super.remap(m).replace("java.lang.String", "String")
      /*
        for some reason java.lang.String is not parsed:
          /mnt/local-data/Work/Workspace/Scala/Delite/sac-test/generatedCache/scala/src/kernels/x5.scala:5: error: ']' expected but '.' found.
          val (x4_shape: Array[Int], x4_value: Array[java.lang.String]) = x4
       */
  }

  def quoteShape(e: Exp[Any]): String = e match {
    case sym: Sym[_] =>
      if (sym.Type.erasure == classOf[MDArray[Any]])
        if (!emittedOutsideSyms.contains(sym))
          super.quote(e) + "_shape"
        else
          super.quote(e) + "._1"
      else
        "Array()"
    case _ =>
      super.quote(e)
  }

  def quoteValue(e: Exp[Any]): String = e match {
    case sym: Sym[_] =>
      if (sym.Type.erasure == classOf[MDArray[Any]])
        if (!emittedOutsideSyms.contains(sym))
          super.quote(e) + "_value"
        else
          super.quote(e) + "._2"
      else
        "Array(" + super.quote(e) + ")"
    case _ =>
      super.quote(e)
  }

  def emitShapeDecl(sym: Sym[_], debug: Boolean = false)(implicit stream: PrintWriter) = {
    stream.print("val " + quoteShape(sym) + ": Array[Int] = ")
  }

  def emitValDecl(sym: Sym[_])(implicit stream: PrintWriter) = {
    stream.print("val " + quoteValue(sym) + ": Array[" + strip(sym.Type) + "] = ")
  }

  def emitSymDecl(sym: Sym[_], debug: Boolean = false)(implicit stream: PrintWriter) = {
    stream.print("val " + quote(sym) + " = ")
  }

  def emitOperationPrologue(sym: Sym[Any], exp: Exp[Any], index: Int)(implicit stream: PrintWriter) = {
    emitShapeDecl(sym)
    stream.println(quoteShape(exp))
    emitValDecl(sym)
    stream.println("new Array[" + strip(sym.Type) + "](prod(" + quoteShape(sym) + "))")
    stream.println("var i_" + index + ": Int = 0")
    stream.println("while (i_" + index + " < " + quoteValue(sym) + ".length) {")
  }

  def emitOperationEpilogue(sym: Sym[Any], exp: Exp[Any], index: Int)(implicit stream: PrintWriter) = {
    stream.println("i_" + index + " += 1")
    stream.println("} // while (i_" + index + " < ...)")
  }

  // This makes it easy to get the elements we need
  def findAndCast[T](e: Any): Option[T] = e match {
    case s: Sym[_] => findAndCast[T](findDefinition(s).get.rhs)
    case d: Def[_] => Some(d.asInstanceOf[T]) // hopefully it matches
    case _ => None
  }

  def emitGenArray[A](sym: Sym[_], wl: List[Exp[MDArray[A]]], shp: Exp[MDArray[Int]])(implicit stream: PrintWriter) = {

    val shpSym: Sym[_] = shp.asInstanceOf[Sym[_]]

    def emitGenArrayAction(iv: String, resultShape: String, resultValue: String): Unit = {
      stream.println("if (result == null) {")
      stream.println("// create the array and shape")
      stream.println("result = new Array[" + strip(sym.Type) + "](prod(" + quoteValue(shpSym) + ") * " + resultValue + ".length)")
      stream.println("rshape = " + resultShape)
      stream.println("}")
      stream.println("// copy new content")
      stream.println("val mainIndex: Int = flattenGenArray(" + quoteValue(shpSym) + ", rshape, " + iv + ")")
      val index = getNewIndex
      //stream.println("println(\"IV=\"+"+iv+".mkString(\" \"))")
      //stream.println("println(\"Shape=\"+"+quoteValue(shpSym)+".mkString(\" \"))")
      //stream.println("println(\"Copying \"+"+resultValue+".mkString(\" \")+\" to offset: \"+mainIndex)")
      stream.println("var innerIndex_" + index + ": Int = 0")
      stream.println("while (innerIndex_" + index + " < " + resultValue + ".length) {")
      stream.println("result(mainIndex + innerIndex_" + index + ") = " + resultValue + "(innerIndex_" + index + ")")
      stream.println("innerIndex_" + index + " += 1")
      stream.println("} // while (innerIndex_" + index + " ...")
    }

    stream.println("{")
    stream.println("var result: Array[" + strip(sym.Type) + "] = null")
    stream.println("var rshape: Array[Int] = null")

    val savedLoopAction = withLoopAction
    withLoopAction = emitGenArrayAction _

    for (withNode <- wl)
      emitBlock(withNode)

    // compute the new shape
    val index = getNewIndex
    stream.println("// reconstructing the shape")
    stream.println("val shape: Array[Int] = new Array(rshape.length + " + quoteValue(shpSym) + ".length)")
    stream.println("var i_" + index + " = 0")
    stream.println("while (i_" + index + " < " + quoteValue(shpSym) + ".length) {")
    stream.println("shape(i_" + index + ") = " + quoteValue(shpSym) + "(i_" + index + ")")
    stream.println("i_" + index + " += 1")
    stream.println("}")
    stream.println("while (i_" + index + " < shape.length) {")
    stream.println("shape(i_" + index + ") = rshape(i_" + index + " - " + quoteValue(shpSym) + ".length)")
    stream.println("i_" + index + " += 1")
    stream.println("}")
    stream.println("(shape, result)")
    stream.println("}")

    withLoopAction = savedLoopAction
  }

  def emitModArray[A](sym: Sym[_], wl: List[Exp[MDArray[A]]], array: Exp[MDArray[A]])(implicit stream: PrintWriter) = {

    val arraySym: Sym[_] = array.asInstanceOf[Sym[_]]

    def emitModArrayAction(iv: String, resultShape: String, resultValue: String): Unit = {
      stream.println("val mainIndex: Int = flattenModArray(" + quoteShape(arraySym) + ", " + iv + ")")
      stream.println("// copy new content")
      val index = getNewIndex
      stream.println("var innerIndex_" + index + ": Int = 0 ")
      stream.println("while (innerIndex_" + index + " < " + resultValue + ".length) {")
      stream.println("result(mainIndex + innerIndex_" + index + ") = " + resultValue + "(innerIndex_" + index + ")")
      stream.println("innerIndex_" + index + " += 1")
      stream.println("} // while (innerIndex_" + index + " ...")
    }

    stream.println("{")
    stream.println("var result: Array[" + strip(sym.Type) + "] = new Array[" + strip(sym.Type) + "](" + quoteValue(arraySym) + ".length)")
    val index = getNewIndex
    stream.println("var i_" + index + ": Int = 0")
    stream.println("while (i_" + index + " < result.length) {")
    stream.println("result(i_" + index + ") = " + quoteValue(arraySym) + "(i_" + index + ")")
    stream.println("i_" + index + " += 1")
    stream.println("}")

    val savedLoopAction = withLoopAction
    withLoopAction = emitModArrayAction _

    for (withNode <- wl)
      emitBlock(withNode)

    withLoopAction = savedLoopAction

    stream.println("(" + quoteShape(arraySym) + ", " + quoteValue(arraySym) + ")")
    stream.println("}")
  }

  def emitFoldArray(sym: Sym[_], withNode: Exp[MDArray[_]], neutral: Exp[_], foldTerm1: Exp[_], foldTerm2: Exp[_], foldExpr: Exp[_])(implicit stream: PrintWriter) = {

    val neutralSym = neutral.asInstanceOf[Sym[_]]
    val foldTerm1Sym = foldTerm1.asInstanceOf[Sym[_]]
    val foldTerm2Sym = foldTerm2.asInstanceOf[Sym[_]]
    val foldExprSym = foldExpr.asInstanceOf[Sym[_]]

    def emitFoldArrayAction(iv: String, resultShape: String, resultValue: String) =
      stream.println("foldAction(result, " + resultValue + ")")

    stream.println("{")
    stream.println("var result: Array[" + strip(neutralSym.Type) + "] = " + quoteValue(neutralSym))
    stream.println("val " + quoteShape(foldTerm1Sym) + ": Array[Int] = " + quoteShape(neutralSym))
    stream.println("val " + quoteShape(foldTerm2Sym) + ": Array[Int] = " + quoteShape(neutralSym))

    stream.println("def foldAction(" + quoteValue(foldTerm1Sym) + ": Array[" + strip(foldTerm1Sym.Type) + "], " + quoteValue(foldTerm2Sym) + ": Array[" + strip(foldTerm2Sym.Type) + "])= {")
    // emit loop content
    emitBlock(foldExprSym)
    stream.println("result = " + quoteValue(getBlockResult(foldExprSym)))
    stream.println("}")   // Emit the loop action

    val savedLoopAction = withLoopAction
    withLoopAction = emitFoldArrayAction _
    emitBlock(withNode)
    withLoopAction = savedLoopAction

    stream.println("(" + quoteShape(neutralSym) + ", result)")
    stream.println("}")
  }

  // support for blocks
  val BLOCKS = System.getProperty("StagedSAC.blocks", "false").toLowerCase == "true"
  val BLOCK_SIZE = 64

  def emitWithLoopNestedFor(index: Int, maxIndex: Int, withNodeSym: Sym[_], withLoop: WithNode[_],
                            emitAction: (String, String, String) => Unit, lastLevelForLoops: List[(String, String)] = Nil)
                           (implicit stream: PrintWriter): Unit = {

    def emitForRange(lower: String, upper: String) = {
      // emit loop and filter
      stream.println(quoteValue(withLoop.sym) + "(" + index + ") = " + lower)
      stream.println("while (" + quoteValue(withLoop.sym) + "(" + index +") <= " + upper + ") {")
      stream.println("if ((" + quoteValue(withLoop.sym) + "(" + index +") - " + quoteValue(withLoop.lb) + "(" + index +")) % " + quoteValue(withLoop.step) + "(" + index +") <= " + quoteValue(withLoop.width) + "(" + index +")) {")
      // emit nested loops
      emitWithLoopNestedFor(index+1, maxIndex, withNodeSym, withLoop, emitAction, lastLevelForLoops)
      // emit loop and filter end
      stream.println("} // if (" + quoteValue(withLoop.sym) + "(" + index +") ...")
      stream.println(quoteValue(withLoop.sym) + "(" + index +") += 1")
      stream.println("} // while (" + quoteValue(withLoop.sym) + "(" + index +") ...")
    }

    def emitForRangeBlock(lower: String, upper: String) = {
      // emit loop and filter
      val givenIndex = getNewIndex
      stream.println(quoteValue(withLoop.sym) + "_block(" + index +") = " + lower)
      stream.println("while (" + quoteValue(withLoop.sym) + "_block(" + index +") <= " + upper + ") {")
      // emit nested loops
      val newStart = quoteValue(withLoop.sym) + "(" + index +") = " + quoteValue(withLoop.sym) + "_block(" + index +") * " + BLOCK_SIZE + "\n" +
                     "while (" + quoteValue(withLoop.sym) + "(" + index +") < (" + quoteValue(withLoop.sym) + "_block(" + index +") + 1) * " + BLOCK_SIZE + ") {\n" +
                     "if ((" + quoteValue(withLoop.sym) + "(" + index +") - " + quoteValue(withLoop.lb) + "(" + index +")) % " + quoteValue(withLoop.step) + "(" + index +") <= " + quoteValue(withLoop.width) + "(" + index +")) {\n"
      val newFinish= "} // if ((" + quoteValue(withLoop.sym) + "(" + index +") ...\n" +
                     quoteValue(withLoop.sym) + "(" + index +") += 1\n" +
                     "} // while (" + quoteValue(withLoop.sym) + "(" + index +") ...\n"
      emitWithLoopNestedFor(index+1, maxIndex, withNodeSym, withLoop, emitAction, lastLevelForLoops ::: List((newStart, newFinish)))
      // emit loop and filter end
      stream.println(quoteValue(withLoop.sym) + "_block(" + index +") += 1")
      stream.println("} // while (" + quoteValue(withLoop.sym) + "_block(" + index +") ...")
    }

    if (index < maxIndex) {
      // emit loop header
      if (BLOCKS) {
        stream.println("if (ll(" + index + ") / " + BLOCK_SIZE + " == ul(" + index + ") / " + BLOCK_SIZE + ") {")
          // same as before
          emitForRange("ll(" + index + ")", "ul(" + index + ")")
        stream.println("} else {")
          // split into three cases:
          // generating the iv in three steps
          // lb           <= iv  < (lb/64 + 1) * 64
          // (lb/64 + 1)  <= iv' < ub/64  [ 64 blocks ]
          // (ub/64) * 64 <= iv  < ub
          // attention: the case of lb/64 == ub/64 => lb <= iv < ub :)
          emitForRange("ll(" + index + ")", "(ll(" + index + ") / " + BLOCK_SIZE + " + 1) * " + BLOCK_SIZE)
          emitForRangeBlock("ll(" + index + ") / " + BLOCK_SIZE + " + 1", "ul(" + index + ") / " + BLOCK_SIZE)
          emitForRange("(ul(" + index + ") / " + BLOCK_SIZE + ") * " + BLOCK_SIZE, "ul(" + index + ")")
        stream.println("} // with loop range if")
      } else
        emitForRange("ll(" + index + ")", "ul(" + index + ")")
    } else {
      // emit blocks
      for((start, finish) <- lastLevelForLoops)
        stream.println(start)

      // perform the with loop action
      stream.println("// the action of this loop:")
      stream.println("withLoopAction()")

      for((start, finish) <- lastLevelForLoops.reverse)
        stream.println(finish)
    }
  }

  def emitWithLoopModifier(withNodeSym: Sym[_], withLoop: WithNode[_], emitAction: (String, String, String) => Unit)(implicit stream: PrintWriter) = {
    // emit existing constraints
    stream.println("// with: " + withLoop.toString)

    // generate bounds
    stream.println("val ll: Array[Int] = new Array(" + quoteValue(withLoop.lb) + ".length)")
    stream.println("val ul: Array[Int] = new Array(" + quoteValue(withLoop.ub) + ".length)")
    stream.println("var " + quoteValue(withLoop.sym) + ": Array[Int] = new Array(" + quoteValue(withLoop.ub) + ".length)")
    stream.println("val " + quoteShape(withLoop.sym) + ": Array[Int] = Array(ll.length)")
    stream.println("val " + quoteValue(withLoop.sym) + "_block: Array[Int] = new Array(ll.length)")
    val index = getNewIndex
    stream.println("var i_" + index + ": Int = 0")
    stream.println("while (i_" + index + " < " + quoteValue(withLoop.ub) + ".length) {")
    stream.println("ll(i_" + index + ") = if (" + quote(withLoop.lbStrict) + ") " + quoteValue(withLoop.lb) + "(i_" + index + ")" + " + 1 else " + quoteValue(withLoop.lb) + "(i_" + index + ")")
    stream.println("ul(i_" + index + ") = if (" + quote(withLoop.ubStrict) + ") " + quoteValue(withLoop.ub) + "(i_" + index + ")" + " - 1 else " + quoteValue(withLoop.ub) + "(i_" + index + ")")
    stream.println(quoteValue(withLoop.sym) + "(i_" + index + ") = ll(i_" + index + ")")
    stream.println("i_" + index + " += 1")
    stream.println("} // while (i" + index + " ...")

    stream.println("def withLoopAction() = {")
    // emit loop content
    emitBlock(withLoop.expr)
    // emit loop action
    emitAction(quoteValue(withLoop.sym), quoteShape(getBlockResult(withLoop.expr)), quoteValue(getBlockResult(withLoop.expr)))
    stream.println("}")

    // emit actual with loop
    getValueLength(withLoop.lb) match {
      case Some(size) =>

        System.err.println("With loop specialization to rank " + size + " with blocks=" + BLOCKS)
        // emit with loop
        emitWithLoopNestedFor(0, size, withNodeSym, withLoop, emitAction, Nil)

      case _ =>

        System.err.println("With loop with no specialization!")

        // start loop
        stream.println("do {")
        // perform the with loop action
        stream.println("// the action of this loop:")
        stream.println("withLoopAction()")
        // emit loop finish
        stream.println(quoteValue(withLoop.sym) + " = nextInLine(" + quoteValue(withLoop.sym) + ", ll, ul, " + quoteValue(withLoop.lb) + ", " + quoteValue(withLoop.step) + ", " + quoteValue(withLoop.width) + ")")
        stream.println("} while (" + quoteValue(withLoop.sym) + " ne null)")
    }
  }

  def val_quote(a: Any): String = a match {
    case s: String => "\"" + s.replace("\"","\\\"") + "\""
    case n: Number => n.toString
    case b: Boolean => b.toString
    case _ => sys.error("unable to generate value for: " + a.toString)
  }

  override def performTyping[A: Manifest, B: Manifest](x: Exp[A], y: Exp[B]): Unit =
    TY.doTyping(y, false)

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
    //emitChecks(sym, rhs)
    emitShapeValue(sym, rhs)

    rhs match {
      case kc: KnownAtCompileTime[_] =>
        emitShapeDecl(sym)
        stream.println("Array(" + kc.value.shape.content.mkString(", ") + ")")
        emitValDecl(sym)
        // TODO: Should be able to use quote
        val m = strip(sym.Type)
        if (m == "String")
          stream.println("Array(" + kc.value.content.map("\"" + _ + "\"").mkString(", ") + ") //" + m)
        else if (m == "Char")
          stream.println("Array(" + kc.value.content.map("'" + _ + "'").mkString(", ") + ") //" + m)
        else
          stream.println("Array(" + kc.value.content.mkString(", ") + ") //" + m)

      case kr: KnownAtRuntime[_] =>
        // don't emit anything :)
      case fl: FromList[_] =>
        fl.value match {
          case Const(list) =>
            emitShapeDecl(sym)
            stream.println("Array(" + list.length + ")")
            emitValDecl(sym)
            stream.println("Array(" + list.mkString(", ") + ")")
          case _ =>
            emitShapeDecl(sym)
            stream.println("Array(" + quote(fl.value) + ".length)")
            emitValDecl(sym)
            stream.println(quote(fl.value))
        }
      case fa: FromArray[_] =>
        fa.value match {
          case Const(array) =>
            emitShapeDecl(sym)
            stream.println("Array(" + array.length + ")")
            emitValDecl(sym)
            stream.println("Array(" + array.mkString(", ") + ")")
          case _ =>
            emitShapeDecl(sym)
            stream.println("Array(" + quote(fa.value) + ".length)")
            emitValDecl(sym)
            stream.println(quote(fa.value))
        }
      case fv: FromValue[_] =>
        fv.value match {
          case Const(scalar) =>
            emitShapeDecl(sym)
            stream.println("Array()")
            emitValDecl(sym)
            stream.println("Array(" + scalar + ")")
          case _ =>
            emitShapeDecl(sym)
            stream.println("Array()")
            emitValDecl(sym)
            stream.println("Array(" + quote(fv.value) + ")")
        }
      case fml: FromMDArrayList[_] => fml.list match {
        case Nil =>
          // empty symbol
          emitShapeDecl(sym)
          stream.println("Array(0)")
          emitValDecl(sym)
          stream.println("Array()")          
        case head::rest =>
          val index = getNewIndex
          stream.println("var i_" + index + ": Int = 1")
          stream.println("var j_" + index + ": Int = 0")
          stream.println("var size_" + index + ": Int = " + quoteValue(head) + ".length")
          emitShapeDecl(sym)
          stream.println("new Array(1 + " + quoteShape(head) + ".length)")
          stream.println(quoteShape(sym) + "(0) = " + fml.list.length)
          stream.println("while (i_" + index + " < " + quoteShape(sym) + ".length) {")
          stream.println(quoteShape(sym) +"(i_" + index + ") = " + quoteShape(head) + "(i_" + index + " - 1)")
          stream.println("i_" + index + " += 1")
          stream.println("} // while (i_" + index + " < ...)")
          emitValDecl(sym)
          stream.println("new Array(prod(" + quoteShape(sym) + "))")
          for (listIndex <- Range(0, fml.list.length)) {
            stream.println("i_" + index + " = 0")
            stream.println("while (i_" + index + " < size_" + index + ") {")
            stream.println(quoteValue(sym) +"(i_" + index + " + size_" + index + " * " + listIndex + ") = " + quoteValue(fml.list(listIndex)) + "(i_" + index + ")")
            stream.println("i_" + index + " += 1")
            stream.println("} // while (i_" + index + " < ...)")        
          }
        }
      case tl: ToList[_] =>
        stream.println("val " + quote(sym) + ": " + remap(sym.Type) + " = " + quoteValue(tl.value))
      case ta: ToArray[_] =>
        stream.println("val " + quote(sym) + ": " + remap(sym.Type) + " = " + quoteValue(ta.value))
      case tv: ToValue[_] =>
        stream.println("val " + quote(sym) + ": " + remap(sym.Type) + " = " + quoteValue(tv.value) + "(0)")
      case td: ToDim[_] =>
        stream.println("val " + quote(sym) + ": Int = " + quoteShape(td.a) + ".length")
      case ts: ToShape[_] =>
        emitShapeDecl(sym)
        stream.println("Array(" + quoteShape(ts.a) + ".length)")
        emitValDecl(sym)
        stream.println(quoteShape(ts.a))
      case rs: Reshape[_] =>
        emitShapeDecl(sym)
        stream.println(quoteValue(rs.shp))
        emitValDecl(sym)
        stream.println(quoteValue(rs.a))
      case sel: Sel[_] =>
        val index = getNewIndex
        val flatIndex = getNewIndex
        stream.println("var i_" + index + ": Int = " + quoteValue(sel.iv) + ".length")
        emitShapeDecl(sym)
        stream.println("new Array(" + quoteShape(sel.a) + ".length -" + quoteValue(sel.iv) + ".length)")
        stream.println("while (i_" + index + " < " + quoteShape(sel.a) + ".length) {")
        stream.println(quoteShape(sym) +"(i_" + index + " - " + quoteValue(sel.iv) + ".length) = " + quoteShape(sel.a) + "(i_" + index + ")")
        stream.println("i_" + index + " += 1")
        stream.println("} // while (i_" + index + " < ...)")
        stream.println("val flat_" + flatIndex + " = flatten(" + quoteValue(sel.iv) + ", " + quoteShape(sel.a) + ")")
        emitValDecl(sym)
        stream.println("new Array(prod(" + quoteShape(sym) + "))")
        stream.println("i_" + index + " = 0")
        stream.println("while (i_" + index + " < " + quoteValue(sym) + ".length) {")
        stream.println(quoteValue(sym) +"(i_" + index + ") = " + quoteValue(sel.a) + "(i_" + index + " + flat_" + flatIndex + ")")
        stream.println("i_" + index + " += 1")
        stream.println("} // while (i_" + index + " < ...)")
      case cat: Cat[_] => cat.d match {
//        case KnownAtCompileTime(mdArray) if ((shape(mdArray).content().length == 0)&&(mdArray.content()(0) == 0)) =>
//          val index = getNewIndex
//          stream.println("var i_" + index + ": Int = 1")
//          emitShapeDecl(sym)
//          stream.println("new Array(" + quoteShape(cat.a) + ".length)")
//          stream.println(quoteShape(sym) + "(0) = " + quoteShape(cat.a) + "(0) + " + quoteShape(cat.b) + "(0)")
//          stream.println("while (i_" + index + " < " + quoteShape(sel.a) + ".length) {")
//          stream.println(quoteShape(sym) +"(i_" + index + ") = " + quoteShape(sel.a) + "(i_" + index + ")")
//          stream.println("i_" + index + " += 1")
//          stream.println("} // while (i_" + index + " < ...)")
//          emitValDecl(sym)
//          stream.println("new Array(" + quoteValue(cat.a) + ".length + " + quoteValue(cat.b) + ".length)")
//          stream.println("i_" + index + " = 0")
//          stream.println("while (i_" + index + " < " + quoteValue(sym.a) + ".length) {")
//          stream.println(quoteValue(sym) +"(i_" + index + ") = " + quoteValue(cat.a) + "(i_" + index + ")")
//          stream.println("i_" + index + " += 1")
//          stream.println("} // while (i_" + index + " < ...)")
//          stream.println("i_" + index + " = 0")
//          stream.println("while (i_" + index + " < " + quoteValue(cat.b) + ".length) {")
//          stream.println(quoteValue(sym) +"(i_" + index + " + " + quoteValue(cat.a) + ".length + ) = " + quoteValue(cat.b) + "(i_" + index + ")")
//          stream.println("i_" + index + " += 1")
//          stream.println("} // while (i_" + index + " < ...)")
        case _ =>
          sys.error("NOT IMPLEMENTED YET!")
      }
      case in: InfixOp[_, _] =>
        // emit operation for (array OP array) or (array OP scalar)
        def emitOperation(scalar: Boolean) = {
          //stream.println("var timer" + sym.id + " = - System.currentTimeMillis")
          val index = getNewIndex
          emitOperationPrologue(sym, in.array1, index)
          scalar match {
            case true => stream.println(quoteValue(sym) + "(i_" + index +") = (" + quoteValue(in.array1) + "(i_" + index +") " + in.opName + "  " + quoteValue(in.array2) + "(0))")
            case false => stream.println(quoteValue(sym) + "(i_" + index +") = (" + quoteValue(in.array1) + "(i_" + index +") " + in.opName + "  " + quoteValue(in.array2) + "(i_" + index +"))")
          }
          emitOperationEpilogue(sym, in.array1, index)
          //stream.println("timer" + sym.id + " += System.currentTimeMillis")
          //stream.println("println(\"executed operation " + quote(sym) + " = " + quote(in.array1) + " " + in.opName + " " + quote(in.array2) + "(scalar = " + scalar + ") in \" + " + "timer" + sym.id + " + \"ms\")")
        }
        getShapeLength(in.array2) match {
          case Some(0) => // we have a scalar element
            emitOperation(true)
          case Some(_) => // we have an array
            emitOperation(false)
          case None => // we don't know what's there
            //TODO: Find out why this is the most common case
            stream.println("// WARNING: Operation not specialized on {arrays|scalars}!")
            stream.println("val " + quote(sym) + ": " + remap(sym.typeManifest) + " = {")
            stream.println("if (" + quoteShape(in.array2) + ".length == 0) {")
            emitOperation(true)
            stream.println(quote(sym))
            stream.println("} else {")
            emitOperation(false)
            stream.println(quote(sym))
            stream.println("}")
            stream.println("}")
        }
      case un: UnaryOp[_, _] =>
        val index = getNewIndex
        emitOperationPrologue(sym, un.array, index)
        stream.println(quoteValue(sym) + "(i_" + index +") = " + un.opName + quoteValue(un.array) + "(i_" + index +")")
        emitOperationEpilogue(sym, un.array, index)
      case wh: Where[_] =>
        val index = getNewIndex
        emitOperationPrologue(sym, wh.array1, index)
        stream.println(quoteValue(sym) + "(i_" + index +") = if (" + quoteValue(wh.cond) + "(i_" + index +")) " + quoteValue(wh.array1) + "(i_" + index +") else " + quoteValue(wh.array2) + "(i_" + index +")")
        emitOperationEpilogue(sym, wh.array1, index)
      case va: Values[_] =>
        val index = getNewIndex
        emitShapeDecl(sym)
        stream.println("Array(" + quoteValue(va.dim) + "(0))")
        emitValDecl(sym)
        stream.println("new Array[Int](" + quoteValue(va.dim) + "(0))")
        stream.println("var i_" + index + ": Int = 0")
        stream.println("while (i_" + index + " < " + quoteValue(sym) + ".length) {")
        stream.println(quoteValue(sym) + "(i_" + index + ") = " + quoteValue(va.value) + "(0)")
        stream.println("i_" + index + " += 1")
        stream.println("} // while (i_" + index + " ...")
      case wn: WithNode[_] =>
        emitWithLoopModifier(sym, wn, withLoopAction)
      case ga: GenArrayWith[_] =>
        stream.println("val " + quote(sym) + ": " + remap(sym.typeManifest) + " = ")
        emitGenArray(sym, ga.lExpr, ga.shp)
        stream.println
      case ma: ModArrayWith[_] =>
        stream.println("val " + quote(sym) + ": " + remap(sym.typeManifest) + " = ")
        emitModArray(sym, ma.lExpr, ma.a)
        stream.println
      case fa: FoldArrayWith[_] =>
        stream.println("val " + quote(sym) + ": " + remap(sym.typeManifest) + " = ")
        emitFoldArray(sym, fa.wExpr, fa.neutral, fa.foldTerm1, fa.foldTerm2, fa.foldExpression)
        stream.println
      case soa: ScalarOperatorApplication[_,_,_] =>
        emitShapeDecl(sym)
        stream.println("Array()")
        emitValDecl(sym)
        stream.println("Array(((a: " + soa.getMfA.toString + ", b: " + soa.getMfB.toString + ") => a " + soa.operator + " b)(" + quoteValue(soa.a) + "(0), " + quoteValue(soa.b) + "(0)))")
      // If must also be translated to account for the scope changes
      // TODO: Is there an architecture where it's not necessary to do this?
      case ite: IfThenElse[_] =>
        stream.println("val " + quote(sym) + ": " + remap(sym.typeManifest) + " = ")
        // The condition is always Rep[Boolean] therefore we can use the value directly
        stream.println("if (" + quote(ite.cond) + ") {")
        TY.withinDifferentScopes(sym,
          (ite.thenp.asInstanceOf[Sym[_]], () => {
            emitBlock(ite.thenp)
            stream.println(quote(ite.thenp))
            stream.println("} else {")
          })::
          (ite.elsep.asInstanceOf[Sym[_]], () => {
            emitBlock(ite.elsep)
            stream.println(quote(ite.elsep))
            stream.println("}")
          })::
          Nil)
      case ite: DeliteIfThenElse[_] => // handle delite ite, too
        stream.println("val " + quote(sym) + ": " + remap(sym.typeManifest) + " = ")
        // The condition is always Rep[Boolean] therefore we can use the value directly
        stream.println("if (" + quote(ite.cond) + ") {")
        TY.withinDifferentScopes(sym,
          (ite.thenp.asInstanceOf[Sym[_]], () => {
            emitBlock(ite.thenp)
            stream.println(quote(ite.thenp))
            stream.println("} else {")
          })::
          (ite.elsep.asInstanceOf[Sym[_]], () => {
            emitBlock(ite.elsep)
            stream.println(quote(ite.elsep))
            stream.println("}")
          })::
          Nil)
      case NewVar(x) =>
        stream.println("//NEWVAR")
        val symIsResult = !deliteResult.isEmpty && (deliteResult.get contains sym)
        if (symIsResult)
          emitValDef(sym, "new generated.scala.Ref((" + quoteShape(x) + ", " + quoteValue(x) + "))")
        else
          emitVarDef((sym.asInstanceOf[Sym[Variable[Any]]]), "(" + quoteShape(x) + ", " + quoteValue(x) + ")")
      case Assign(Variable(lhs), x) =>
        stream.println("//ASSIGN")
        val isInput = (deliteInputs intersect syms(rhs)).nonEmpty
        if (isInput)
          emitValDef(sym, quote(lhs) + ".set(" + quote(x) + ")")
        else
          emitAssignment(quote(lhs), "(" + quote(x) + ")")
      case ReadVar(Variable(x)) =>
        stream.println("//READVAR")
        val isInput = (deliteInputs intersect syms(rhs)).nonEmpty
        if (isInput)
          stream.println("val " + quote(sym) + ": " + remap(sym.typeManifest) + " = " + quote(x) + ".get")
        else
          stream.println("val " + quote(sym) + ": " + remap(sym.typeManifest) + " = " + quote(x))
      case pn: PrintLn => pn.x match {
        case s: Sym[_] if (s.Type.erasure == classOf[MDArray[Any]]) =>
          emitValDef(sym, "println(getString(" + quoteShape(s) + ", " + quoteValue(s) + "))")
        case _ =>
          super.emitNode(sym, rhs)
      }
      case st: ToString[_] =>
        emitSymDecl(sym)
        stream.println("getString(" + quoteShape(st.value) + ", " + quoteValue(st.value) + ")")
      case rd: ReadMDArray[_] =>
        stream.println("val " + quote(sym) + ": " + remap(sym.typeManifest) + " = ")
        stream.println("// " + rd.fileName.Type)
        val m = strip(sym.Type)
        if (m == "Int")
          stream.println("readMDArrayInt(" + quoteValue(rd.fileName) + "(0))")
        else if (m == "Double")
          stream.println("readMDArrayDouble(" + quoteValue(rd.fileName) + "(0))")
        else if (m == "Float")
          stream.println("readMDArrayFloat(" + quoteValue(rd.fileName) + "(0))")
        else if (m == "Boolean")
          stream.println("readMDArrayBoolean(" + quoteValue(rd.fileName) + "(0))")
        else if (m == "Char")
          stream.println("readMDArrayChar(" + quoteValue(rd.fileName) + "(0))")
        else
          sys.error("Unable to generate readMDArray: Unrecognized type " + m)
      case wr: WriteMDArray[_] =>
        emitSymDecl(sym)
        stream.println("writeMDArray[" + wr.getManifest + "](" + quoteValue(wr.fileName) + "(0), " + quoteShape(wr.array) + ", " + quoteValue(wr.array) + ")")
      case st: StartTimer =>
        emitSymDecl(sym)
        stream.println("startTimer()")
      case st: StopTimer =>
        emitSymDecl(sym)
        stream.println("stopTimer()")
      case _ =>
        stream.println("// emitting outside!")
        emittedOutsideSyms = sym::emittedOutsideSyms
        super.emitNode(sym, rhs)
    }
  }


  override def emitSource[A,B](f: Exp[A] => Exp[B], className: String, stream: PrintWriter)(implicit mA: Manifest[A], mB: Manifest[B]): List[(Sym[Any], Any)] = {
    super.emitSource(f, className, stream)(mA, mB)
  }

  override def emitKernelHeader(syms: List[Sym[Any]], vals: List[Sym[Any]], vars: List[Sym[Any]], resultType: String, resultIsVar: Boolean, external: Boolean)(implicit stream: PrintWriter): Unit = {

    val oldEmittedInKernel = emittedInKernel
    emittedInKernel = true
    // unfortunately pair deconstruction is not supported in the method definition => it needs to be inside and add the
    // elements outside as
    super.emitKernelHeader(syms, vals, vars, resultType, resultIsVar, external)
    emittedInKernel = oldEmittedInKernel

    stream.println("// syms: " + syms.mkString(" "))
    stream.println("// vals: " + vals.mkString(" "))
    stream.println("// vars: " + vars.mkString(" "))

    // since the kernel header was generated as "inside" we need to really bring the symbols inside
    for (s <- vals)
      if (s.Type.erasure == classOf[MDArray[Any]])
        stream.println("val (" + quoteShape(s) + ", " + quoteValue(s) + "): " + remap(s.Type) + " = (" + super.quote(s) + ")")
  }

  override def emitKernelFooter(syms: List[Sym[Any]], vals: List[Sym[Any]], vars: List[Sym[Any]], resultType: String, resultIsVar: Boolean, external: Boolean)(implicit stream: PrintWriter): Unit = {

    stream.println("// emittedOustideSyms: " + emittedOutsideSyms.map(super.quote(_)).mkString(" "))

    stream.println("// syms: " + syms.mkString(" "))
    stream.println("// vals: " + vals.mkString(" "))
    stream.println("// vars: " + vars.mkString(" "))

    // The kernel footer should generate symbols as seen from outside
    val oldEmittedInKernel = emittedInKernel
    //emittedInKernel = true
    super.emitKernelFooter(syms, vals, vars, resultType, resultIsVar, external)
    emittedInKernel = oldEmittedInKernel
  }

  override def emitImports(implicit stream:PrintWriter): Unit = {
    stream.println("import datastruct.scala.MDArrayRuntimeSupport._")
  }

  override def quote(x: Exp[Any]) : String =
    if (!emittedInKernel)
      x match {
        case sym: Sym[_] if (sym.Type.erasure == classOf[MDArray[Any]]) =>
          if (emittedOutsideSyms.contains(sym))
            super.quote(x)
          else
            "(" + quoteShape(sym) + ", " + quoteValue(sym) + ")"
        case _ =>
          super.quote(x)
      }
    else
      super.quote(x)
}
