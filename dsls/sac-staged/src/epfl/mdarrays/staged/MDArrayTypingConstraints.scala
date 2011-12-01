package epfl.mdarrays.staged

import _root_.scala.virtualization.lms.common._
import _root_.ppl.delite.framework.codegen.delite.primitives.{DeliteIfThenElseExp,DeliteScalaGenIfThenElse,DeliteWhileExp,DeliteScalaGenWhile,DeliteScalaGenVariables}
import epfl.mdarrays.library.scala._
import epfl.mdarrays.library.scala.Conversions._

import java.io.{Writer, PrintWriter}
import collection.immutable.HashMap


trait MDArrayTypingConstraints extends BaseGenMDArray with DeliteScalaGenIfThenElse with DeliteScalaGenWhile with DeliteScalaGenVariables with ScalaGenMiscOps with MDArrayTypingUnifier {

  val IR: StagedSACExp
  import IR._

  override type Symbol = Sym[_]
  override type Expression = Exp[_]
  override def getId(s: Symbol): Int = s.id

  protected def addConstraints(tl: List[TypingConstraint]): Unit
  protected def addSymbol(sym: Sym[_]): Unit
  protected def createSubScope(ifSym: Sym[_], sym: Sym[_])(action: => Unit): Unit

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter): Unit = {
    //System.out.println("emitNode: " + sym + " <= " + rhs)
    val nodeConstraints =
      // generic reconstruct value from shape
      ReconstructValueFromShape(ValueVar(sym), ShapeVar(sym), postReq, rhs)::
      // and the specific constraints for each node
      (rhs match {
        // We only analyze the "special" cases here
        case DeliteWhile(cond, body) =>
          emitBlock(cond)
          emitBlock(body)
          getConstraints(sym, rhs)
        case DeliteIfThenElse(cond, thenp, elsep, _) =>
          createSubScope(sym, thenp.asInstanceOf[Sym[_]]) { emitBlock(thenp) }
          createSubScope(sym, elsep.asInstanceOf[Sym[_]]) { emitBlock(elsep) }
          getConstraints(sym, rhs)
        case IfThenElse(cond, thenp, elsep) =>
          createSubScope(sym, thenp.asInstanceOf[Sym[_]]) { emitBlock(thenp) }
          createSubScope(sym, elsep.asInstanceOf[Sym[_]]) { emitBlock(elsep) }
          getConstraints(sym, rhs)
        case WithNode(lb, lbStrict, ub, ubStrict, step, width, ivSym, expr) =>
          // emit the expression constraints
          emitBlock(expr)
          // add symbol
          addSymbol(ivSym)
          getConstraints(sym, rhs)
        case GenArrayWith(withLoops, shape) =>
          assert(withLoops.length >= 1)
          // emit with loop constraints
          for (withLoop <- withLoops)
            emitBlock(withLoop)
          getConstraints(sym, rhs)
        case ModArrayWith(withLoops, array) =>
          assert(withLoops.length >= 1)
          // emit with loop constraints
          for (withLoop <- withLoops)
            emitBlock(withLoop)
          getConstraints(sym, rhs)
        case FoldArrayWith(withLoop, neutral, foldTerm1, foldTerm2, foldExpression) =>
          // emit with loop and fold expression constraints
          emitBlock(withLoop)
          // add symbols
          addSymbol(foldTerm1)
          addSymbol(foldTerm2)
          emitBlock(foldExpression)
          getConstraints(sym, rhs)
        case Timed(oper) =>
          emitBlock(oper)
          getConstraints(sym, rhs)
        case _ =>
          getConstraints(sym, rhs)
      })
    // now what do we do with these constraints?
    addConstraints(nodeConstraints)
    addSymbol(sym)
  }

  def getConstraints(sym: Sym[_], rhs: Def[_]): List[TypingConstraint] = rhs match {
    case KnownAtCompileTime(value) =>
      Equality(ShapeVar(sym), Lst(value.shape.map(i => toValue(i))), postReq, rhs)::
      Equality(ValueVar(sym), Lst(value.content.map((i: Any) => toValue(i)).toList), postReq, rhs)::Nil
    case KnownAtRuntime(name) =>
      Nil
    case FromList(list) =>
      Equality(ShapeVar(sym), Lst(getNewUnknownElement::Nil), postReq, rhs)::Nil
    case FromArray(array) =>
      Equality(ShapeVar(sym), Lst(getNewUnknownElement::Nil), postReq, rhs)::Nil
    case FromMDArrayList(list) => list match {
      case Nil =>
        Equality(ShapeVar(sym), Lst(Value(0)::Nil), postReq, rhs)::Nil
      case head::rest =>
        EqualityAeqBcatC(ShapeVar(sym), Lst(Value(list.length)::Nil), ShapeVar(head), postReq, rhs)::
        rest.map((x: Exp[_]) => Equality(ShapeVar(head), ShapeVar(x), preReq, rhs))
    }
    case FromValue(value) =>
      Equality(ShapeVar(sym), Lst(Nil), postReq, rhs)::Nil
    case ToList(value) =>
      LengthEqualityAeqB(ShapeVar(value), Lst(getNewUnknownElement::Nil), preReq, rhs)::
      Equality(ShapeVar(sym), ShapeVar(value), postReq, rhs)::Nil
    case ToArray(value) =>
      LengthEqualityAeqB(ShapeVar(value), Lst(getNewUnknownElement::Nil), preReq, rhs)::
      Equality(ShapeVar(sym), ShapeVar(value), postReq, rhs)::Nil
    case ToValue(value) =>
      LengthEqualityAeqB(ShapeVar(value), Lst(Nil), preReq, rhs)::
      Equality(ShapeVar(sym), Lst(Nil), postReq, rhs)::Nil
    case ToDim(array) =>
      Equality(ShapeVar(sym), Lst(Nil), postReq, rhs) ::
      Equality(ValueVar(sym), Lst(getNewUnknownElement::Nil), postReq, rhs)::
      EqualityToLengthOf(ValueVar(sym), ShapeVar(array), postReq, rhs)::Nil
    case ToShape(array) =>
      Equality(ValueVar(sym), ShapeVar(array), postReq, rhs)::
      Equality(ShapeVar(sym), Lst(getNewUnknownElement::Nil), postReq, rhs)::
      EqualityToLengthOf(ShapeVar(sym), ShapeVar(array), postReq, rhs)::Nil
    case Reshape(shape, array) =>
      LengthEqualityAeqB(ShapeVar(shape), Lst(getNewUnknownElement :: Nil), preReq, rhs)::
      EqualProduct(ValueVar(shape), ShapeVar(array), preReq, rhs)::
      Equality(ShapeVar(sym), ValueVar(shape), postReq, rhs)::Nil
    case Sel(iv, array) =>
      LengthEqualityAeqB(ShapeVar(iv), Lst(getNewUnknownElement::Nil), preReq, rhs)::
      PrefixLt(ShapeVar(array), ValueVar(iv), ShapeVar(sym), preReq, rhs)::
      SuffixEq(ShapeVar(array), ValueVar(iv), ShapeVar(sym), postReq, rhs)::Nil
    case Cat(d, a, b) =>
      LengthEqualityAeqB(ShapeVar(d), Lst(Nil), preReq, rhs)::
      LengthEqualityAeqB(ShapeVar(a), Lst(getNewUnknownElement::Nil), preReq, rhs)::
      LengthEqualityAeqB(ShapeVar(a), ShapeVar(b), preReq, rhs)::
      Equality(ValueVar(d), Lst(getNewUnknownElement::Nil), preReq, rhs)::
      LessThanLengthOf(ValueVar(d), ShapeVar(a), preReq, rhs)::
      EqualityExceptFor(ValueVar(d), ShapeVar(a), ShapeVar(b), preReq, rhs)::
      LengthEqualityAeqB(ShapeVar(sym), ShapeVar(a), postReq, rhs)::
      EqualityShAeqShBplusShCalongD(ShapeVar(sym), ShapeVar(a), ShapeVar(b), ValueVar(d), postReq, rhs)::Nil
    case Values(dim, value) =>
      Equality(ShapeVar(dim), Lst(Nil), preReq, rhs)::
      Equality(ShapeVar(value), Lst(Nil), preReq, rhs)::
      Equality(ShapeVar(sym), ValueVar(dim), postReq, rhs)::
      EqualityAeqDimTimesValue(ValueVar(sym), ValueVar(dim), ValueVar(value), postReq, rhs)::Nil
    case InfixOp(array1, array2, op, opName) =>
      EqualityOrScalar(ShapeVar(array1), ShapeVar(array2), preReq, rhs)::
      Equality(ShapeVar(sym), ShapeVar(array1), postReq, rhs)::Nil
    case UnaryOp(array, op, opName) =>
      Equality(ShapeVar(sym), ShapeVar(array), postReq, rhs)::Nil
    case Where(cond, array1, array2) =>
      Equality(ShapeVar(cond), ShapeVar(array1), preReq, rhs)::
      Equality(ShapeVar(cond), ShapeVar(array2), preReq, rhs)::
      Equality(ShapeVar(sym), ShapeVar(array1), postReq, rhs)::Nil
    case WithNode(lb, lbStrict, ub, ubStrict, step, width, ivSym, expr) =>
      LengthEqualityAeqB(ShapeVar(lb), Lst(getNewUnknownElement::Nil), preReq, rhs)::
      Equality(ShapeVar(lbStrict), Lst(Nil), preReq, rhs)::
      Equality(ShapeVar(ubStrict), Lst(Nil), preReq, rhs)::
      Equality(ShapeVar(ub), ShapeVar(lb), preReq, rhs)::
      Equality(ShapeVar(step), ShapeVar(lb), preReq, rhs)::
      Equality(ShapeVar(width), ShapeVar(lb), preReq, rhs)::
      Equality(ShapeVar(ivSym), ShapeVar(lb), postReq, rhs)::
      Equality(ShapeVar(sym), ShapeVar(expr), postReq, rhs)::
      LessThanConstraint(ValueVar(lb), ValueVar(ub), preReq, rhs)::Nil
    case GenArrayWith(withLoops, shape) =>
      assert(withLoops.length >= 1)
      withNodeListConstraints(withLoops, rhs):::
      Equality(ShapeVar(shape), Lst(getNewUnknownElement::Nil), preReq, rhs)::
      withLoops.flatMap(wn =>
        Equality(ShapeVar(shape), ShapeVar(recoverWithNode(wn).lb), preReq, rhs)::
        PrefixLt(ValueVar(shape), ValueVar(recoverWithNode(wn).lb), ShapeVar(wn), preReq, rhs)::Nil
        // let the expr(iv) return any dimension
        // SuffixEq(ValueVar(shape), ValueVar(recoverWithNode(wn).lb), ShapeVar(wn), preReq, rhs)::Nil
      ) :::
      EqualityAeqBcatC(ShapeVar(sym), ValueVar(shape), ShapeVar(withLoops.head), postReq, rhs)::Nil
    case ModArrayWith(withLoops, array) =>
      assert(withLoops.length >= 1)
      withNodeListConstraints(withLoops, rhs):::
      withLoops.flatMap(wn =>
        Equality(ShapeVar(recoverWithNode(wn).lb), Lst(getNewUnknownElement::Nil), preReq, rhs)::
        EqualityToLengthOf(ShapeVar(recoverWithNode(wn).lb), ShapeVar(array), preReq, rhs)::
        PrefixLt(ShapeVar(array), ValueVar(recoverWithNode(wn).lb), ShapeVar(wn), preReq, rhs)::
        SuffixEq(ShapeVar(array), ValueVar(recoverWithNode(wn).lb), ShapeVar(wn), preReq, rhs)::Nil
      ) :::
      Equality(ShapeVar(sym), ShapeVar(array), postReq, rhs)::Nil
    case FoldArrayWith(withLoop, neutral, foldTerm1, foldTerm2, foldExpression) =>
      Equality(ShapeVar(neutral), ShapeVar(withLoop), preReq, rhs)::
      Equality(ShapeVar(foldTerm1), ShapeVar(neutral), postReq, rhs)::
      Equality(ShapeVar(foldTerm2), ShapeVar(neutral), postReq, rhs)::
      Equality(ShapeVar(foldExpression), ShapeVar(withLoop), preReq, rhs)::
      Equality(ShapeVar(sym), ShapeVar(neutral), postReq, rhs)::Nil
    case NewVar(x) =>
      Equality(ShapeVar(sym), ShapeVar(x), postReq, rhs)::Nil
    case ReadVar(Variable(x)) =>
      Equality(ShapeVar(sym), ShapeVar(x), postReq, rhs)::Nil
    case Assign(Variable(lhs), x) =>
      Equality(ShapeVar(lhs), ShapeVar(x), postReq, rhs)::Nil
    case DeliteWhile(cond, body) =>
      Equality(ShapeVar(cond), Lst(Nil), preReq, rhs)::Nil
    case DeliteIfThenElse(cond, thenp, elsep, _) =>
      Equality(ShapeVar(cond), Lst(Nil), preReq, rhs)::
//      Equality(ShapeVar(thenp), ShapeVar(elsep), preReq, rhs)::
//      Equality(ShapeVar(sym), ShapeVar(thenp), postReq, rhs)::Nil
      CommonDenominator(ShapeVar(sym), ShapeVar(thenp), ShapeVar(elsep), postReq, rhs)::Nil
    case IfThenElse(cond, thenp, elsep) =>
      Equality(ShapeVar(cond), Lst(Nil), preReq, rhs)::
//      Equality(ShapeVar(thenp), ShapeVar(elsep), preReq, rhs)::
//      Equality(ShapeVar(sym), ShapeVar(thenp), postReq, rhs)::Nil
      CommonDenominator(ShapeVar(sym), ShapeVar(thenp), ShapeVar(elsep), postReq, rhs)::Nil
    case ScalarOperatorApplication(/*function, */operator, operand1, operand2) =>
      Equality(ShapeVar(operand1), Lst(Nil), preReq, rhs)::
      Equality(ShapeVar(operand2), Lst(Nil), preReq, rhs)::
      Equality(ShapeVar(sym), Lst(Nil), postReq, rhs)::Nil
    case Argument(base, index) =>
      Nil // arguments are not constrained in any way
    case PrintLn(x) =>
      //Equality(x, Lst(Nil), preReq, rhs)::Nil
      Nil
    case ToString(x) =>
      Nil
    case ReadMDArray(fileName, possibleShape) =>
      Equality(ShapeVar(fileName), Lst(Nil), preReq, rhs)::(possibleShape match {
        case Some(shape) => Equality(ShapeVar(sym), Lst(shape.map(toValue)), preReq, rhs)::Nil
        case None => Nil
      })
    case WriteMDArray(fileName, array) =>
      Equality(ShapeVar(fileName), Lst(Nil), preReq, rhs)::Nil
    case StartTimer(afterComputing) =>
      Nil
    case StopTimer(afterComputing) =>
      Nil
    case IntegerEqeqeq(o1, o2) =>
      Equality(ShapeVar(o1), Lst(Nil), preReq, rhs)::
      Equality(ShapeVar(o2), Lst(Nil), preReq, rhs)::
      Equality(ShapeVar(sym), Lst(Nil), postReq, rhs)::
      Nil
    case IntegerLt(o1, o2) =>
      Equality(ShapeVar(o1), Lst(Nil), preReq, rhs)::
      Equality(ShapeVar(o2), Lst(Nil), preReq, rhs)::
      Equality(ShapeVar(sym), Lst(Nil), postReq, rhs)::
      Nil
    case IntegerMinus(o1, o2) =>
      Equality(ShapeVar(o1), Lst(Nil), preReq, rhs)::
      Equality(ShapeVar(o2), Lst(Nil), preReq, rhs)::
      Equality(ShapeVar(sym), Lst(Nil), postReq, rhs)::
      Nil
    case IntConst(o) =>
      Equality(ShapeVar(sym), Lst(Nil), postReq, rhs)::
      Nil
    case Timed(op) =>
      Equality(ShapeVar(sym), ShapeVar(op), postReq, rhs)::
      Equality(ValueVar(sym), ValueVar(op), postReq, rhs)::
      Nil
    case _ =>
//      System.err.println("other nodes: " + sym + " <= " + rhs)
      super.emitNode(sym, rhs)(null) // for Reify() - careful, if it outputs anything it will crash&burn!
      Nil
  }

  def toValue(i: Any): TypingElement = i match {
    case i: Int => Value(i)
    case _ => getNewUnknownElement
  }

  def recoverWithNode(e: Exp[MDArray[_]]): WithNode[_] =
    findDefinition(e.asInstanceOf[Sym[_]]).get.rhs.asInstanceOf[WithNode[_]]

  def withNodeListConstraints(withNodeList: List[Exp[MDArray[_]]], node: Any): List[TypingConstraint] = {
    val f = withNodeList.head // first node
    // map the rest of the nodes
    withNodeList.tail.map(e => Equality(ShapeVar(recoverWithNode(f).lb), ShapeVar(recoverWithNode(e).lb), preReq, node)) :::
    withNodeList.tail.map(e => Equality(ShapeVar(f), ShapeVar(e), preReq, node))
  }

  override def emitValDef(sym: Sym[Any], rhs: String)(implicit stream: PrintWriter): Unit = {}
  override def emitSource[A,B](f: Exp[A] => Exp[B], className: String, stream: PrintWriter)(implicit mA: Manifest[A], mB: Manifest[B]): List[(Sym[Any],Any)] = Nil
}
