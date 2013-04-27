package edu.umass.cs.iesl.harshal.voltext

import java.io.{FileInputStream, InputStreamReader, BufferedReader, File}
import collection.mutable.HashMap
import java.util.zip.GZIPInputStream

/**
 * @author harshal
 * @date: 4/16/13
 */
object Document {
  def concat(files:Seq[File])={
    val map = HashMap[Int,Int]()
    for (f <- files){
      val reader = getReader(f)
      var line = reader.readLine()
      while(line!=null){
        val split = line.split("\t")
        if (split.length==2) map(split(0).toInt) = map.getOrElseUpdate(split(0).toInt,0)+split(1).toInt
        line = reader.readLine()
      }
    }
    map
  }

  def getReader(f:File) = new BufferedReader(new InputStreamReader(new GZIPInputStream(new FileInputStream(f))))

}
