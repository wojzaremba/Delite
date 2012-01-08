package ppl.apps.liszt.laplace

import ppl.dsl.deliszt._
import ppl.dsl.deliszt.MetaInteger._
import ppl.dsl.deliszt.lib._

object PoissonRunner extends DeLisztApplicationRunner with Poisson


trait Poisson extends DeLisztApplication with SimpleMesh {



  def main(): Unit = {
    val mesh2 = SquareMesh(1., 0.5)
    for (f <- faces(mesh2)) {
	println(ID(flip(f)))
    	for(v <- vertices(f)) {	
		println(ID(v))		
	}
    }
  }
}
