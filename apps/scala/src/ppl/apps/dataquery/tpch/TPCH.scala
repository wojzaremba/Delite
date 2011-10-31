package ppl.apps.dataquery.tpch

import ppl.dsl.optiql.{OptiQLApplication, OptiQLApplicationRunner}

object TPCHQ1 extends OptiQLApplicationRunner with TPCHQ1Trait
object TPCHQ2 extends OptiQLApplicationRunner with TPCHQ2Trait

trait TPCHBaseTrait extends OptiQLApplication {

  val queryName: String

  //TODO: unfortunately the compiler magic only works with anonymous classes, but we want a name for these...
  // creating a named class with a single anonymous subclass (as below) can confuse the struct logic
  // (manifest for both types passed at different places)
  /*abstract class Region extends Result {
    val r_regionkey: Rep[Int]
    val r_name: Rep[String]
    val r_comment: Rep[String]
  }

  def Region(key: Rep[Int], name: Rep[String], comment: Rep[String]) = new Region {
    val r_regionkey = key
    val r_name = name
    val r_comment = comment
  } */

  //the verbose version
  abstract class Region extends Result
  def Region(key: Rep[Int], name: Rep[String], comment: Rep[String]) = struct[Region]("r_regionkey"->key,"r_name"->name,"r_comment"->comment)
  def infix_r_regionkey(r: Rep[Region]) = field[Int](r, "r_regionkey")
  def infix_r_name(r: Rep[Region]) = field[String](r, "r_name")
  def infix_r_comment(r: Rep[Region]) = field[String](r, "r_comment")

  type LineItem = Result
  type Nation = Result
  type Part = Result
  type PartSupplier = Result
  type Supplier = Result

  type LineItemTable = DataTable[LineItem]
  type NationTable = DataTable[Nation]
  type PartTable = DataTable[Part]
  type PartSupplierTable = DataTable[PartSupplier]
  type RegionTable = DataTable[Region]
  type SupplierTable = DataTable[Supplier]

  //var customers: Rep[CustomerTable] = _
  var lineItems: Rep[LineItemTable] = _
  //var orders: Rep[OrderTable] = _
  var nations: Rep[NationTable] = _
  var parts: Rep[PartTable] = _
  var partSuppliers: Rep[PartSupplierTable] = _
  var regions: Rep[RegionTable] = _
  var suppliers: Rep[SupplierTable] = _

    
  def printUsage = {
    println("Usage: TPCH <input tpch directory>")
    exit(-1)
  }
  
  def query(): Rep[_]
  
  def main() = {
    println("TPCH style benchmarking " + queryName )
    if (args.length < 1) printUsage
    
    val tpchDataPath = args(0) 

    //load TPCH data
    lineItems = TPCH.loadLineItems(tpchDataPath)
    nations = TPCH.loadNations(tpchDataPath)
    parts = TPCH.loadParts(tpchDataPath)
    partSuppliers = TPCH.loadPartSuppliers(tpchDataPath)
    regions = TPCH.loadRegions(tpchDataPath)
    suppliers = TPCH.loadSuppliers(tpchDataPath)
    println("Loading Complete")	
    tic(lineItems,nations, parts, partSuppliers, regions, suppliers)     
    query()
  }

}

//val res = lineItems Select(e => new Result { val l_shipdate = e.l_shipdate  }) Where(_.l_shipdate <= Date("1998-12-01"))
trait TPCHQ1Trait extends TPCHBaseTrait {
  val queryName = "Q1"  
  def query() = {           
    val q = lineItems Where(_.l_shipdate <= Date("1998-12-01")) GroupBy(l => (l.l_returnflag,l.l_linestatus)) Select(g => new Result {
      val returnFlag = g.key._1
      val lineStatus = g.key._2
      val sumQty = g.Sum(_.l_quantity)
      val sumBasePrice = g.Sum(_.l_extendedprice)
      val sumDiscountedPrice = g.Sum(l => l.l_extendedprice * (1.0d - l.l_discount))
      val sumCharge = g.Sum(l=> l.l_extendedprice * (1.0d - l.l_discount) * (1.0d + l.l_tax))
      val avgQty = g.Average(_.l_quantity)
      val avgPrice = g.Average(_.l_extendedprice)
      val avgDiscount = g.Average(_.l_discount)
      val countOrder = g.Count            
    }) OrderBy(_.returnFlag) ThenBy(_.lineStatus) End() 
    toc(q)
    q.printAsTable()
  }    
}


trait TPCHQ2Trait extends TPCHBaseTrait {
  val queryName = "Q2"  
  
  def query() = {
    val q = parts.Where(p => {val res:Rep[Boolean] = p.p_size == 15; res}).Where(_.p_type.endsWith("BRASS")).Join(partSuppliers).WhereEq(_.p_partkey, _.ps_partkey).Select((p, ps) => new  Result {
      val p_partkey = p.p_partkey
      val p_mfgr = p.p_mfgr
      val ps_suppkey = ps.ps_suppkey
      val ps_supplycost = ps.ps_supplycost
    }).Join(suppliers).WhereEq(_.ps_suppkey,_.s_suppkey).Select((j, s) => new Result {
      val s_acctbal = s.s_acctbal
      val s_name = s.s_name
      val p_partkey = j.p_partkey
      val p_mfgr = j.p_mfgr
      val s_address = s.s_address
      val s_phone = s.s_phone
      val s_comment = s.s_comment
      val ps_supplycost = j.ps_supplycost
      val s_nationkey = s.s_nationkey
    }).Join(nations).WhereEq(_.s_nationkey, _.n_nationkey).Select((j, n) => new Result {
      val s_acctbal = j.s_acctbal
      val s_name = j.s_name
      val n_name = n.n_name
      val p_partkey = j.p_partkey
      val p_mfgr = j.p_mfgr
      val s_address = j.s_address
      val s_phone = j.s_phone
      val s_comment = j.s_comment
      val ps_supplycost = j.ps_supplycost
      val n_regionkey = n.n_regionkey
    }).Join(regions).WhereEq(_.n_regionkey, _.r_regionkey).Select((j, r) => new Result {
      val s_acctbal = j.s_acctbal
      val s_name = j.s_name
      val n_name = j.n_name
      val p_partkey = j.p_partkey
      val p_mfgr = j.p_mfgr
      val s_address = j.s_address
      val s_phone = j.s_phone
      val s_comment = j.s_comment
      val ps_supplycost = j.ps_supplycost
      val r_name = r.r_name
    }).Where(_.r_name == "EUROPE").Where(j1 => j1.ps_supplycost == {
      val pssc = partSuppliers.Where(_.ps_partkey == j1.p_partkey).
        Join(suppliers).WhereEq(_.ps_suppkey, _.s_suppkey).Select((ps, s) => new Result {
        val ps_supplycost = ps.ps_supplycost
        val s_nationkey = s.s_nationkey
       }).Join(nations).WhereEq(_.s_nationkey, _.n_nationkey).Select((jj1, n) => new Result {
         val ps_supplycost = jj1.ps_supplycost
         val n_regionkey = n.n_regionkey
       }).Join(regions).WhereEq(_.n_regionkey, _.r_regionkey).Select((jj2, r) => new Result {
         val ps_supplycost = jj2.ps_supplycost
         val r_name = r.r_name
       }).Where(_.r_name == "EUROPE").Min(_.ps_supplycost); if(pssc != null) pssc.ps_supplycost else -10}
    ) OrderByDescending(_.s_acctbal) ThenBy(_.n_name) ThenBy(_.s_name) ThenBy(_.p_partkey) End()
    toc(q)
    q.printAsTable(10)
  }    
} 