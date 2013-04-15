package edu.umass.cs.iesl.harshal.voltext

import io.Source
import collection.mutable

/**
 * @author harshal
 * @date: 4/15/13
 */
object FinancialDictionary {

  def buildDictionary(dir : java.io.File)={
    val dictionary = new mutable.HashMap[String,Int]()
    val files = dir.listFiles().filter(_.getName.endsWith(".csv"))
    for (file<-files){
      for (line<-Source.fromFile(file).getLines()) {
        dictionary+= line.split(",")(0).toLowerCase->0
      }
    }
    dictionary
  }
}
