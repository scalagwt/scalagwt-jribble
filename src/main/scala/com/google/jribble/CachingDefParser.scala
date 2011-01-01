package com.google.jribble

import ast.DeclaredType
import java.io.File

trait CachingDefParser extends DefParser {

  val cacheDir = new File(System.getProperty("user.dir"), "jribbleCache")
  println("Caching folder is %1s".format(cacheDir.toString))

  override def parse(in: java.io.Reader, name: String): Either[DeclaredType, String] = {
    val file = new File(cacheDir, name + ".bin")
    if (file.exists) {
      println("Reading %1s from cache".format(name))
      val ois  = new java.io.ObjectInputStream(new java.io.FileInputStream(file))
      val r = ois.readObject.asInstanceOf[ast.DeclaredType]
      ois.close()
      Left(r)
    } else {
      file.getParentFile.mkdirs()
      file.createNewFile()
      super.parse(in, name) match {
        case x@Left(r) => {
          val oss  = new java.io.ObjectOutputStream(new java.io.FileOutputStream(file))
          oss.writeObject(r)
          oss.close()
          x
        }
        case x => x
      }
    }
  }

}
