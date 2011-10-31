package ppl.dsl.optiql.ops

import ppl.dsl.optiql.datastruct.scala.container.{DataTable, Grouping}
import ppl.dsl.optiql.datastruct.scala.ordering.OrderedQueryable
import java.io.PrintWriter
import scala.virtualization.lms.common.{Base, ScalaGenFat, BaseFatExp}
import scala.virtualization.lms.internal.GenericFatCodegen
import scala.reflect.SourceContext
import ppl.dsl.optiql.OptiQLExp
import ppl.delite.framework.datastructures.FieldAccessOpsExp
import ppl.delite.framework.datastruct.scala._
import ppl.delite.framework.collections._
import ppl.delite.framework.ops.DeliteCollectionOps

trait QueryableOps extends GenericCollectionOps with DeliteCollectionOps {

  //type TransparentProxy[+T] = Rep[T]

  implicit def repToQueryableOps[TSource:Manifest](r: Rep[DataTable[TSource]]) = new QOpsCls(r)
  implicit def repOQtoOQOps[TSource:Manifest](r: Rep[OrderedQueryable[TSource]]) = new OQOpsCls(r)
  implicit def HashMapToQueryableOps[TSource: Manifest, TKey: Manifest](hm: Rep[HashMap[TKey,Bucket[TSource]]]) = new QOpsCls(queryable_hashmap_toDatatable(hm))
  implicit def repGroupingToQueryableOps[TKey:Manifest, TSource:Manifest](g: Rep[Grouping[TKey, TSource]]) = new QOpsCls(queryable_grouping_toDatatable(g))

  //override def __forward[A,B,C](self: TransparentProxy[A], method: String, x: TransparentProxy[B]*): TransparentProxy[C] = throw new RuntimeException("forwarding to " + method)

  class QOpsCls[TSource:Manifest](s: Rep[DataTable[TSource]]) {
    def Where(predicate: Rep[TSource] => Rep[Boolean]) = queryable_where(s, predicate)
	def GroupBy[TKey:Manifest](keySelector: Rep[TSource] => Rep[TKey]) = queryable_groupby(s,keySelector)
    def GroupByHash[TKey:Manifest](keySelector: Rep[TSource] => Rep[TKey]) = queryable_groupby_hash(s,keySelector)
    def OrderBy[TKey:Manifest](keySelector: Rep[TSource] => Rep[TKey]) = queryable_orderby(s, keySelector)
    def OrderByDescending[TKey:Manifest](keySelector: Rep[TSource] => Rep[TKey]) = queryable_orderbydescending(s, keySelector)        
	def Select[TResult:Manifest](resultSelector: Rep[TSource] => Rep[TResult]) = queryable_select(s, resultSelector)
	def Sum(sumSelector: Rep[TSource] => Rep[Double]) = queryable_sum(s, sumSelector)
	def Average(avgSelector: Rep[TSource] => Rep[Double]) = queryable_average(s, avgSelector)
    def Min(minSelector: Rep[TSource] => Rep[Double]) = queryable_min(s, minSelector)
    def Join[TSecond:Manifest](second: Rep[DataTable[TSecond]]) = new JoinableOps(s, second)
	def Count() = queryable_count(s)
  }
  
  class OQOpsCls[TSource:Manifest](oq: Rep[OrderedQueryable[TSource]]) {
    def ThenBy[TKey:Manifest](keySelector: Rep[TSource] => Rep[TKey]) = queryable_thenby(oq, keySelector) 
    def End(): Rep[DataTable[TSource]] = queryable_end(oq)    
  }
  
  class JoinableOps[TFirst:Manifest, TSecond:Manifest](first: Rep[DataTable[TFirst]], second: Rep[DataTable[TSecond]]) {
    def Where(predicate: Rep[TSecond] => Rep[Boolean]) = new JoinableOps(first, second.Where(predicate))
    def WhereEq[TKey2:Manifest](firstKeySelector: Rep[TFirst] => Rep[TKey2],secondKeySelector: Rep[TSecond] => Rep[TKey2]) = new Joinable2(first, firstKeySelector, second, secondKeySelector)
  }
  
  class Joinable2[TFirst:Manifest, TSecond:Manifest, TKey2:Manifest](
    val first: Rep[DataTable[TFirst]],
    val firstKeySelector: Rep[TFirst] => Rep[TKey2],
    val second: Rep[DataTable[TSecond]],
    val secondKeySelector: Rep[TSecond] => Rep[TKey2] 
  ) {
    def Select[TResult:Manifest](resultSelector: (Rep[TFirst], Rep[TSecond]) => Rep[TResult]) = queryable_join2(first, firstKeySelector, second, secondKeySelector, resultSelector)
  }
  
  //Grouping stuff
  def infix_key[TKey:Manifest, TSource:Manifest](g: Rep[Grouping[TKey, TSource]]) = queryable_grouping_key(g) 

  def queryable_where[TSource:Manifest](s: Rep[DataTable[TSource]], predicate: Rep[TSource] => Rep[Boolean]): Rep[DataTable[TSource]]
  def queryable_groupby[TSource:Manifest, TKey:Manifest](s: Rep[DataTable[TSource]], keySelector: Rep[TSource] => Rep[TKey]): Rep[DataTable[Grouping[TKey, TSource]]]
  def queryable_groupby_hash[TSource:Manifest, TKey:Manifest, Coll <: DataTable[TSource]: Manifest](s: Rep[Coll], keySelector: Rep[TSource] => Rep[TKey]): Rep[HashMap[TKey, Bucket[TSource]]]
  def queryable_orderby[TSource:Manifest, TKey:Manifest](s: Rep[DataTable[TSource]], keySelector: Rep[TSource] => Rep[TKey]): Rep[OrderedQueryable[TSource]]
  def queryable_orderbydescending[TSource:Manifest, TKey:Manifest](s: Rep[DataTable[TSource]], keySelector: Rep[TSource] => Rep[TKey]): Rep[OrderedQueryable[TSource]]
  def queryable_thenby[TSource:Manifest, TKey:Manifest](oq: Rep[OrderedQueryable[TSource]], keySelector: Rep[TSource] => Rep[TKey]): Rep[OrderedQueryable[TSource]]
  def queryable_end[TSource:Manifest](oq: Rep[OrderedQueryable[TSource]]): Rep[DataTable[TSource]]
  def queryable_select[TSource:Manifest, TResult:Manifest](s: Rep[DataTable[TSource]], resultSelector: Rep[TSource] => Rep[TResult]): Rep[DataTable[TResult]]
  def queryable_sum[TSource:Manifest](s: Rep[DataTable[TSource]], sumSelector: Rep[TSource] => Rep[Double]): Rep[Double]
  def queryable_average[TSource:Manifest](s: Rep[DataTable[TSource]], avgSelector: Rep[TSource] => Rep[Double]): Rep[Double]
  def queryable_min[TSource:Manifest](s: Rep[DataTable[TSource]], minSelector: Rep[TSource] => Rep[Double]): Rep[TSource]
  def queryable_count[TSource:Manifest](s: Rep[DataTable[TSource]]): Rep[Int]  
  def queryable_join2[TFirst:Manifest, TSecond:Manifest, TKey2:Manifest, TResult:Manifest](first: Rep[DataTable[TFirst]], firstKeySelector: Rep[TFirst] => Rep[TKey2], 
    second: Rep[DataTable[TSecond]], secondKeySelector: Rep[TSecond] => Rep[TKey2], resultSelector: (Rep[TFirst], Rep[TSecond]) => Rep[TResult]):Rep[DataTable[TResult]]
  
  def queryable_hashmap_toDatatable[TKey:Manifest, TSource:Manifest](hm: Rep[HashMap[TKey, Bucket[TSource]]]): Rep[DataTable[Grouping[TKey, TSource]]]
  def queryable_grouping_toDatatable[TKey:Manifest, TSource:Manifest](g: Rep[Grouping[TKey, TSource]]): Rep[DataTable[TSource]]
  def queryable_grouping_key[TKey:Manifest, TSource:Manifest](g: Rep[Grouping[TKey, TSource]]): Rep[TKey]
  
  
}

trait QueryableOpsExp extends QueryableOps with BaseFatExp {
  this: QueryableOps with TraversableOpsExp with OptiQLExp =>

  case class QueryableWhere[TSource:Manifest](in: Exp[DataTable[TSource]], cond: Exp[TSource] => Exp[Boolean]) extends DeliteOpFilter[TSource, TSource,DataTable[TSource]] {
    def alloc = DataTable[TSource]()
    def func = e => e
    val size = in.size    
  }
     
  case class QueryableSelect[TSource:Manifest, TResult:Manifest](in: Exp[DataTable[TSource]], func: Exp[TSource] => Exp[TResult]) extends DeliteOpMap[TSource, TResult, DataTable[TResult]] {
	  def alloc = DataTable[TResult](in.size)
    val size = in.size
  }
  
  
  //these are hacked up for now untill we have proper Delite support
  case class HackQueryableGroupBy[TSource:Manifest, TKey:Manifest](in: Exp[DataTable[TSource]], v:Sym[TSource], key: Exp[TKey]) extends Def[DataTable[Grouping[TKey, TSource]]]    
  case class HackQueryableSum[TSource:Manifest](in:Exp[DataTable[TSource]], sym: Sym[TSource], value: Exp[Double]) extends Def[Double]
  case class HackQueryableMin[TSource:Manifest, TKey:Manifest](in: Exp[DataTable[TSource]], v: Sym[TSource], key: Exp[TKey]) extends Def[TSource]
  case class HackQueryableJoin2[TFirst:Manifest, TSecond:Manifest, TKey2:Manifest, TResult:Manifest](first: Exp[DataTable[TFirst]], second: Exp[DataTable[TSecond]], 
    firstV: Sym[TFirst], secondV: Sym[TSecond], firstKey: Exp[TKey2], secondKey: Exp[TKey2], rfv: Sym[TFirst], rsv: Sym[TSecond], result:Exp[TResult]) extends Def[DataTable[TResult]]
  case class HackQueryableOrderBy[TSource:Manifest, TKey:Manifest](in: Exp[DataTable[TSource]], v: Sym[TSource], key: Exp[TKey]) extends Def[OrderedQueryable[TSource]]
  case class HackQueryableOrderByDescending[TSource:Manifest, TKey:Manifest](in: Exp[DataTable[TSource]], v: Sym[TSource], key: Exp[TKey]) extends Def[OrderedQueryable[TSource]]
  case class HackQueryableThenBy[TSource:Manifest, TKey:Manifest](in: Exp[OrderedQueryable[TSource]], v: Sym[TSource], key: Exp[TKey]) extends Def[OrderedQueryable[TSource]]
  case class HackQueryableEnd[TSource:Manifest](in:Exp[OrderedQueryable[TSource]]) extends Def[DataTable[TSource]]
  
  /*
  case class QueryableSum[TSource:Manifest](s: Exp[DataTable[TSource]], sumSelector: Rep[TSource] => Rep[Double]) extends DeliteOpReduce[Double] {
	  val size = copyTransformedOrElse(_.size)(s.size)
	  
    def func = (a,b) => sumSelector(a) + sumSelector(b)
    val zero = unit(0.0f)
	  /*
    val body: Def[Double] = DeliteReduceElem[Double](
		  func = sumSelector(s(v)),
		  zero = unit(0.0f),
		  rV = rV,
		  rFunc = rV._1 + rV._2,
      stripFirst = false
	  )*/
  }*/
  case class QueryableAverage[TSource:Manifest](s: Exp[DataTable[TSource]], avgSelector: Rep[TSource] => Rep[Double]) extends Def[Double]
  
  //case class QueryableCount[TSource:Manifest](s: Exp[DataTable[TSource]]) extends Def[Int]

  case class QueryableHashMapToDataTable[TSource:Manifest, TKey:Manifest](hm: Rep[HashMap[TKey, Bucket[TSource]]]) extends Def[DataTable[Grouping[TKey,TSource]]]
  case class QueryableGroupingToDataTable[TSource:Manifest, TKey:Manifest](g: Rep[Grouping[TKey, TSource]]) extends Def[DataTable[TSource]]
  case class QueryableGroupingKey[TSource:Manifest, TKey:Manifest](g: Rep[Grouping[TKey, TSource]]) extends Def[TKey]

  def queryable_select[TSource:Manifest, TResult:Manifest](s: Rep[DataTable[TSource]], resultSelector: Rep[TSource] => Rep[TResult]) = {
//	val v = fresh[TSource]
//	val func = reifyEffects(resultSelector(v))
	QueryableSelect(s, resultSelector)
  }
  
  def queryable_where[TSource:Manifest](s: Exp[DataTable[TSource]], predicate: Exp[TSource] => Exp[Boolean]) = QueryableWhere(s,predicate)
  //def traversable_groupby[T: Manifest, Coll <: DeliteCollection[T]: Manifest, K: Manifest](in: Exp[Coll], f: Exp[T] => Exp[K]): Exp[HashMap[K, Bucket[T]]] = reflectEffect(TraversableGroupBy[T, K, Coll](in, f))
  def queryable_groupby_hash[TSource:Manifest, TKey:Manifest, Coll <: DataTable[TSource]: Manifest](s: Exp[Coll], keySelector: Exp[TSource] => Exp[TKey]) = TraversableGroupBy[TSource, TKey, Coll](s, keySelector)
  def queryable_groupby[TSource:Manifest, TKey:Manifest](s: Exp[DataTable[TSource]], keySelector: Exp[TSource] => Exp[TKey]) = {
    val v = fresh[TSource]
    val key = keySelector(v)
    HackQueryableGroupBy(s, v, key)
  }
  def queryable_sum[TSource:Manifest](s: Rep[DataTable[TSource]], sumSelector: Rep[TSource] => Rep[Double]) = {
    val sym = fresh[TSource]
    val value = sumSelector(sym)
    HackQueryableSum(s,sym,value)
  }
  def queryable_average[TSource:Manifest](s: Rep[DataTable[TSource]], avgSelector: Rep[TSource] => Rep[Double]) = s.Sum(avgSelector)/s.size()
  def queryable_min[TSource:Manifest](s: Rep[DataTable[TSource]], minSelector: Rep[TSource] => Rep[Double]): Rep[TSource] = {
    val v = fresh[TSource]
    val key = minSelector(v)
    HackQueryableMin(s,v,key)
  }
  
  def queryable_count[TSource:Manifest](s: Rep[DataTable[TSource]]) = s.size()
  
  def queryable_orderby[TSource:Manifest, TKey:Manifest](s: Rep[DataTable[TSource]], keySelector: Rep[TSource] => Rep[TKey]): Rep[OrderedQueryable[TSource]] = {
    val v = fresh[TSource]
    val key = keySelector(v)
    HackQueryableOrderBy(s,v,key)
  }
  def queryable_orderbydescending[TSource:Manifest, TKey:Manifest](s: Rep[DataTable[TSource]], keySelector: Rep[TSource] => Rep[TKey]): Rep[OrderedQueryable[TSource]] = {
    val v = fresh[TSource]
    val key = keySelector(v)
    HackQueryableOrderByDescending(s,v,key)
  }
  def queryable_thenby[TSource:Manifest, TKey:Manifest](s: Rep[OrderedQueryable[TSource]], keySelector: Rep[TSource] => Rep[TKey]): Rep[OrderedQueryable[TSource]] = {
    val v = fresh[TSource]
    val key = keySelector(v)
    HackQueryableThenBy(s,v,key)
  }  
  def queryable_end[TSource:Manifest](in: Rep[OrderedQueryable[TSource]]): Rep[DataTable[TSource]] = HackQueryableEnd(in)
  
  def queryable_join2[TFirst:Manifest, TSecond:Manifest, TKey2:Manifest, TResult:Manifest](first: Rep[DataTable[TFirst]], firstKeySelector: Rep[TFirst] => Rep[TKey2], 
    second: Rep[DataTable[TSecond]], secondKeySelector: Rep[TSecond] => Rep[TKey2], resultSelector: (Rep[TFirst], Rep[TSecond]) => Rep[TResult]):Rep[DataTable[TResult]] = {
    val fv = fresh[TFirst]
    val sv = fresh[TSecond]
    val fkey = firstKeySelector(fv)
    val skey = secondKeySelector(sv)
    val rfv = fresh[TFirst]
    val rsv = fresh[TSecond]
    val result = resultSelector(rfv,rsv)
    HackQueryableJoin2(first, second, fv,sv,fkey,skey, rfv, rsv, result)    
  }
  
  def queryable_hashmap_toDatatable[TKey:Manifest, TSource:Manifest](hm: Rep[HashMap[TKey, Bucket[TSource]]]) = QueryableHashMapToDataTable(hm)
  def queryable_grouping_toDatatable[TKey:Manifest, TSource:Manifest](g: Rep[Grouping[TKey, TSource]]) = QueryableGroupingToDataTable(g)
  def queryable_grouping_key[TKey:Manifest, TSource:Manifest](g: Rep[Grouping[TKey, TSource]]): Rep[TKey] = QueryableGroupingKey(g)
  
  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {    
    case QueryableWhere(s,p) => queryable_where(f(s), p)
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]] //todo fix asInstanceOf
  
  override def syms(e: Any): List[Sym[Any]] = e match { 
    //case QueryableGroupBy(s,v,k) => syms(s) 
    case _ => super.syms(e)
  }
  
  override def boundSyms(e: Any): List[Sym[Any]] = e match {    
    case HackQueryableGroupBy(s,v,k) => v::syms(k)
    case HackQueryableSum(in,v,value) => v::syms(value)
    case HackQueryableOrderBy(in,v,key) => v::syms(key)
    case HackQueryableMin(in,v,key) => v::syms(key)
    case HackQueryableJoin2(frst,scnd,fv,sv, fkey,skey,rfv, rsv, res) => fv::sv::rfv::rsv::syms(fkey):::syms(skey):::syms(res)
    case HackQueryableOrderByDescending(in,v,key) => v::syms(key)
    case HackQueryableThenBy(in,v,key) => v::syms(key)
	case _ => super.boundSyms(e)
  }  
  
}

trait ScalaGenQueryableOps extends ScalaGenFat {  
  val IR: QueryableOpsExp with OptiQLExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
	case HackQueryableGroupBy(s, v, k) =>  {
	  stream.println("val " + quote(sym) + " =  " + quote(s) + ".GroupBy( " + quote(v) + " => {")	  
	  emitBlock(k)	 
	  stream.println(quote(getBlockResult(k)) + "})")
	}
    case HackQueryableSum(s,sym2,value) => {
      stream.println("val " + quote(sym) + " = " + quote(s) + ".Sum( " + quote(sym2) + " => {")
      emitBlock(value)
      stream.println(quote(getBlockResult(value)) + "})")    
    }
    case HackQueryableMin(s,sym2,value) => {
      stream.println("val " + quote(sym) + " = " + quote(s) + ".Min( " + quote(sym2) + " => {")
      emitBlock(value)
      stream.println(quote(getBlockResult(value)) + "})")    
    }
    case HackQueryableOrderBy(s, v, k) =>  {
	  stream.println("val " + quote(sym) + " =  " + quote(s) + ".OrderBy( " + quote(v) + " => {")	  
	  emitBlock(k)	 
	  stream.println(quote(getBlockResult(k)) + "})")
	}
    case HackQueryableThenBy(s, v, k) =>  {
	  stream.println("val " + quote(sym) + " =  " + quote(s) + ".ThenBy( " + quote(v) + " => {")	  
	  emitBlock(k)	 
	  stream.println(quote(getBlockResult(k)) + "})")
	}
    case HackQueryableOrderByDescending(s, v, k) =>  {
	  stream.println("val " + quote(sym) + " =  " + quote(s) + ".OrderByDescending( " + quote(v) + " => {")	  
	  emitBlock(k)	 
	  stream.println(quote(getBlockResult(k)) + "})")
	}
    case HackQueryableEnd(s) => emitValDef(sym, quote(s) + ".End")
    case HackQueryableJoin2(frst,scnd,fv,sv, fkey,skey,rfv, rsv, res) => {
      stream.println("val " + quote(sym) + " =  " + quote(frst) + ".Join( " + quote(scnd) + " ).WhereEq( \n" + quote(fv) + " => { ")	  
	  emitBlock(fkey)	 
	  stream.println(quote(getBlockResult(fkey)) + "}, \n" +  quote(sv) + " => { ")
      emitBlock(skey)
      stream.println(quote(getBlockResult(skey)) + " }).Select( (" + quote(rfv) + "," + quote(rsv) + ") => { ")
      emitBlock(res)
      stream.println(quote(getBlockResult(res)) + "})")
    }    	
    case QueryableHashMapToDataTable(hm) => emitValDef(sym, "generated.scala.container.DataTable.convertHashMapToDataTable(" + quote(hm) + ")")
	case QueryableGroupingToDataTable(g) => emitValDef(sym, "generated.scala.container.DataTable.convertIterableToDataTable(" + quote(g) + ")")
	case QueryableGroupingKey(g) => emitValDef(sym, quote(g) + ".key")
    case _ => super.emitNode(sym,rhs)
  }
}
