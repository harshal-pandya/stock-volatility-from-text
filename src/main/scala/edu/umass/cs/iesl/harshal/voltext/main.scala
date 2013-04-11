package edu.umass.cs.iesl.harshal.voltext

import java.io.{FileOutputStream, PrintWriter, File}
import actors.Futures._
import collection.mutable.ArrayBuffer

/**
 * @author harshal
 * @date: 4/6/13
 */
object Runner extends App{
  val NYT_END_DATE = new Date("2007-06-19")

  val nyt_dir = new File(args(0))

  if (!nyt_dir.isDirectory) {
    println("NYT dir not found")
    System.exit(0)
  }

  val counts = ArrayBuffer[Int]()
//  val wordSet = Set("ibm","international business machines","international business machines corporation","ibm corporation","ibm corp","i b m","i b m corp","i b m corporation")
  val wordSet = Set("ibm","i.b.m.")
  val entryDate = "1987-01-01"
  val offset = TimelineManager(entryDate)

  val currentDate = new Date(entryDate)

//  for (i <- 0 until offset) {
  //    counts+=getCountForTheDay(currentDate.date)
  //    currentDate.incrementDate
  //  }

  while(currentDate<=NYT_END_DATE){
      counts+=getCountForTheDay(currentDate.date)
      currentDate.incrementDate
  }

  val writer = new PrintWriter(new FileOutputStream(new File(args(1))))
  for (count <- counts){
    writer.write(count+"\n")
  }
  writer.close()

  def getCountForTheDay(date:String):Int = {
    println(nyt_dir.getAbsolutePath+"/"+dateToFilePath(date))
    val fs = new File(nyt_dir.getAbsolutePath+"/"+dateToFilePath(date)).listFiles()
    println(fs.size)
    val tasks = for (f <- fs) yield future {
      val c = MentionCounter(f,wordSet,true)
      if(c>0) println(f.getAbsolutePath)
      c
    }
    fs.toSeq.par.map(MentionCounter(_,wordSet,true))
    val counts = awaitAll(60000L,tasks:_*)
    counts.flatten.foldLeft(0)((acc,c)=>acc+c.asInstanceOf[Int])
  }
  def dateToFilePath(date:String,del:String="-") = date.split(del).mkString("/")
}