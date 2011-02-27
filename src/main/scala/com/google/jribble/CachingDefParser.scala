package com.google.jribble

import ast.DeclaredType
import java.io.File
import java.math.BigInteger

trait CachingDefParser extends DefParser {

  val cacheDir = new File(System.getProperty("user.dir"), "jribbleCache")
  println("Caching folder is %1s".format(cacheDir.toString))

  override def parse(in: java.io.Reader, name: String): Either[DeclaredType, String] = {
    val hash = {
      val r = md5(in)
      in.reset()
      r
    }
    val file = new File(cacheDir, String.format("%1s-%2s.bin", name, hash))
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

  private def md5(in: java.io.Reader): String = {
    val m = java.security.MessageDigest.getInstance("MD5")
    val data = read(in).getBytes
    m.update(data, 0, data.length)
    val i = new BigInteger(1,m.digest())
    String.format("%1$032X", i)
  }

  private def read(in: java.io.Reader): String = {
    val arr = new Array[Char](8*1024) //8K at a time
    val buf = new StringBuffer();
    var n: Int = 0

    while ({n = in.read(arr, 0, arr.length); n} > 0)
      buf.append(arr, 0, n);

    buf.toString();
  }

}
