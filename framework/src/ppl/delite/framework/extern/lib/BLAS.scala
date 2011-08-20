package ppl.delite.framework.extern.lib

import ppl.delite.framework._
import ppl.delite.framework.codegen.scala._
import ppl.delite.framework.extern.xplatform._
import java.io._

object BLAS extends ExternalLibrary {
  val configFile = "BLAS.config"
  //val target = "scala" // this should be well-typed, but we don't have an IR reference yet, so we need to generalize that...  
  val libName = "scalaBLAS"
  val ext = "c"
  
  override val header = """
#include <stdlib.h>
#include <stdio.h>

#include <jni.h>
#include "mkl.h"
"""
}
