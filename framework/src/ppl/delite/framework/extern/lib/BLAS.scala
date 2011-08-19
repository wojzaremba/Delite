package ppl.delite.framework.extern.lib

import ppl.delite.framework._
import ppl.delite.framework.codegen.scala._
import ppl.delite.framework.extern.xplatform._
import java.io._

object BLAS extends ExternalLibrary {
  val configFile = "BLAS.xml"
  //val target = "scala" // this should be well-typed, but we don't have an IR reference yet, so we need to generalize that...  
  val libName = "scalaBLAS"
  val ext = "c"
  
  override val header = """
#include <stdlib.h>
#include <stdio.h>

#include <jni.h>
#include "mkl.h"
"""

  val javaHome = System.getProperty("java.home")
  val blasHome = (config \\ "home" text).expandEnvironmentVars
  val arch = config \\ "arch" text

  val compiler = os match {
    case "windows" => new ExternalLibraryCompiler {
      val path = "icl"
      override val init = """call "%s\bin\ipsxe-comp-vars.bat" %s""".format(blasHome, arch)
      val flags = scala.collection.mutable.ListBuffer[String]()
      flags += """/nologo"""
      flags += """/w"""
      flags += """/O3"""
      flags += """/I%s\..\include""".format(javaHome)
      flags += """/I%s\..\include\win32""".format(javaHome)
      flags += """/I%s\mkl\include""".format(blasHome)
      flags += """/LIBPATH:%s\mkl\lib""".format(blasHome)
      flags += """/LIBPATH:%s\mkl\lib\%s""".format(blasHome, arch)
      flags += """/LIBPATH:%s\compiler\lib""".format(blasHome)
      flags += """/LIBPATH:%s\compiler\lib\%s""".format(blasHome, arch)
      flags ++= List("mkl_intel_lp64.lib", "mkl_intel_thread.lib", "mkl_core.lib", "libiomp5mt.lib")
      flags += "/LD"
      val args = flags.toList
      val output = List("/Fe:%s")
    }

    case "linux" => new ExternalLibraryCompiler {
      val path = "icc"
      override val init = "source %s/bin/compilervars.sh %s".format(blasHome, arch)
      val flags = scala.collection.mutable.ListBuffer[String]()
      flags += """-w"""
      flags += """-O3"""
      flags += """-I%s/../include""".format(javaHome)
      flags += """-I%s/../include/linux""".format(javaHome)
      flags += """-I%s/mkl/include""".format(blasHome)
      // unfortunately, intel libs cannot be linked statically, since the distribution provides neither libmkl_mc3.a nor libmkl_def.a
      // that's why, unlike on Windows, we've got to either hardcode the libpath in LD_LIBRARY_PATH or provide it every time when running delite
      // flags ++= List("-Bstatic", "-static-intel")
      flags += """-L%s/mkl/lib""".format(blasHome)
      flags += """-L%s/mkl/lib/%s""".format(blasHome, arch)
      flags += """-L%s/lib""".format(blasHome)
      flags += """-L%s/lib/%s""".format(blasHome, arch)
      flags ++= List("-lmkl_intel_lp64", "-lmkl_intel_thread", "-lmkl_core", "-liomp5", "-lmkl_mc3", "-lmkl_def", "-lgfortran")
      flags ++= List("-shared", "-fPIC")
      val args = flags.toList
      val output = List("-o", "%s")
    }

    case "mac" => new ExternalLibraryCompiler {
      val path = "icc"
      override val init = "source %s/bin/compilervars.sh %s".format(blasHome, arch)
      val flags = scala.collection.mutable.ListBuffer[String]()
      flags += """-w"""
      flags += """-O3"""
      flags += """-I/System/Library/Frameworks/JavaVM.framework/Headers"""
      flags += """-I%s/mkl/include""".format(blasHome)
      // unfortunately, intel libs cannot be linked statically, since the distribution does not provide libmkl_mc3.a
      // that's why, unlike on Windows, we've got to either hardcode the libpath in DYLD_LIBRARY_PATH or provide it every time when running delite
      // flags ++= List("-static-intel")
      flags += """-L%s/mkl/lib""".format(blasHome)
      flags += """-L%s/mkl/lib/%s""".format(blasHome, arch)
      flags += """-L%s/lib""".format(blasHome)
      flags += """-L%s/lib/%s""".format(blasHome, arch)
      flags ++= List("-lmkl_intel_lp64", "-lmkl_intel_thread", "-lmkl_core", "-liomp5", "-lmkl_mc3")
      flags ++= List("-dynamiclib", "-fPIC")
      val args = flags.toList
      val output = List("-o", "%s")
    }

    case _ => {
      sys.error("operating system %s is not supported".format(System.getProperty("os.name")))
    }
  }
}
