package ppl.dsl.optiql

import scala.virtualization.lms.common.Base

trait Types extends Base {

  trait Date

  trait Grouping[TKey, TElement]

  trait OrderedQueryable[TSource]

}
