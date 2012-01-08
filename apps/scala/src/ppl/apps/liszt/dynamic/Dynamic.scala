package ppl.apps.liszt.laplace

import ppl.dsl.deliszt._
import ppl.dsl.deliszt.MetaInteger._
import ppl.dsl.deliszt.lib._

object DynamicRunner extends DeLisztApplicationRunner with Dynamic


trait Dynamic extends DeLisztApplication with SimpleMesh {



  def main(): Unit = {
    val mesh2 = SquareMesh(1., 0.5)
    for (v0 <- vertices(mesh)) {
      println("ID of verticle from mesh loaded from liszt.cfg : " + ID(v0))
      for (f <- faces(mesh2)) {
	println("flipID : " + ID(flip(f)))
    	for(v <- vertices(f)) {	
		println("v ID : " + ID(v))		
  	}
      }
    }
  }
}
