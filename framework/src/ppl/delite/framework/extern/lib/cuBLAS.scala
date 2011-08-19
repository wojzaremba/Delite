package ppl.delite.framework.extern.lib

import ppl.delite.framework._
import ppl.delite.framework.codegen.scala._
import java.io._

object cuBLAS extends ExternalLibrary {
  val libName = "cudaBLAS"
  val ext = "cu"

  override val header = """
#include <stdlib.h>
#include <stdio.h>
#include <limits>
#include <cuda_runtime.h>
#include <cublas.h>
"""
  
  val javaHome = System.getProperty("java.home")
  val cudaHome = config \\ "home" text
  val arch = config \\ "arch" text
  val code = config \\ "code" text

  val compiler = os match {
    case "windows" => new ExternalLibraryCompiler {
      val path = "nvcc"
      val flags = scala.collection.mutable.ListBuffer[String]()
      flags += """/nologo"""
      flags += """/w"""
      flags += """/O3"""
      flags += """/I"%s\..\include"""".format(javaHome)
      flags += """/I"%s\..\include\win32"""".format(javaHome)
      flags += "/LD"
      val args = flags.toList
      val output = List("/Fe:%s")
    }

    case "linux" => new ExternalLibraryCompiler {
      val path = "nvcc"
      val flags = scala.collection.mutable.ListBuffer[String]()
      flags += """-w"""
      flags += """-O3"""
      flags += """-I"%s/../include"""".format(javaHome)
      flags += """-I"%s/../include/linux"""".format(javaHome)
      flags += "-shared -Xcompiler -fPIC"
      val args = flags.toList
      val output = List("-o", "%s")
    }

    case "mac" => new ExternalLibraryCompiler {
      val path = "nvcc"
      val flags = scala.collection.mutable.ListBuffer[String]()
      flags += """-w"""
      flags += """-O3"""
      flags += """-I"/System/Library/Frameworks/JavaVM.framework/Headers""""
      flags += "-dynamiclib -fPIC"
      val args = flags.toList
      val output = List("-o", "%s")
    }

    case _ => {
      sys.error("operating system %s is not supported".format(System.getProperty("os.name")))
    }
  }
}
