package ppl.delite.framework.extern.lib

import ppl.delite.framework._
import ppl.delite.framework.codegen.scala._
import ppl.delite.framework.extern.xplatform._
import java.io._

object cuBLAS extends ExternalLibrary {
  val configFile = "cuBLAS.config"
  val libName = "cudaBLAS"
  val ext = "cu"

  override val header = """
#include <stdlib.h>
#include <stdio.h>
#include <limits>
#include <cuda_runtime.h>
#include <cublas.h>
"""
}
