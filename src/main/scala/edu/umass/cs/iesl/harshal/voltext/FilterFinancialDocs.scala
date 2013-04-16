package edu.umass.cs.iesl.harshal.voltext

import io.Source
import collection.mutable
import java.io.File

/**
 * @author harshal
 * @date: 4/15/13
 */
object FinancialDictionary {

  def buildDictionary(dir : File)={
    val dictionary = new mutable.HashMap[String,Int]()
    val files = dir.listFiles().filter(_.getName.endsWith(".csv"))
    for (file<-files){
      for (line<-Source.fromFile(file).getLines()) {
        dictionary+= line.split(",")(0).toLowerCase->0
      }
    }
    dictionary
  }

  def buildRareWordDictionary(file:File,rarity:Int)={
    val dictionary = new mutable.HashMap[String,Int]()
    for (line<-Source.fromFile(file).getLines()) {
      val split = line.split(",")
      if(split(1).toInt>0 && split(1).toInt<=rarity)
        dictionary+= split(0).toLowerCase->0
    }
    dictionary
  }
}
