package edu.umass.cs.iesl.harshal.voltext

import java.io._
import io.Source
import java.text.SimpleDateFormat
import java.util.{Calendar, Locale}
import collection.mutable

/**
 * @author harshal
 * @date: 4/11/13
 */
object DataLoader {
  case class StartDate(y:Int,m:Int,d:Int)
  val IBM_START = StartDate(1987,0,1)
  val GS_START = StartDate(1999,4,1)
  val UN_START = IBM_START
  val UL_START = StartDate(1988,0,1)//1988-01-05
  def fillInHolidays(file:File,out:File){
    val cal = Calendar.getInstance()
    val trueCal = Calendar.getInstance()
    trueCal.set(UL_START.y,UL_START.m,UL_START.d)
    val reader = new BufferedReader(new FileReader(file))
    val writer = new PrintWriter(out)
    try{
      var line = reader.readLine()
      while(line!=null){
        val fields = line.split(",")
        val dateField = fields(0)
        val stockPrice = fields(6).toDouble
        println(dateField)
        val date = new SimpleDateFormat("yyyy-MM-dd", Locale.US).parse(dateField)
        cal.setTime(date)
        println(cal.getTime)
        while(!dateCompare(trueCal,cal)){
          trueCal.add(Calendar.DATE, 1)
          println(trueCal.getTime)
          writer.write(0+"\n")
        }
        writer.write(stockPrice+"\n")
        line = reader.readLine()
        trueCal.add(Calendar.DATE, 1)
      }
      println("Final True date "+trueCal.getTime)
    }
    finally {
      if(writer ne null)
        writer.close()
      if (reader ne null)
        reader.close()
    }
//    /iesl/canvas/sameer/dat/nyt/data/1987/01/01
//    232
//    /iesl/canvas/sameer/dat/nyt/data/1987/01/01/0000154.xml
//    /iesl/canvas/sameer/dat/nyt/data/1987/01/01/0000187.xml

    //2013-03-14,212.15,215.86,212.15,215.80,5505500,215.80
  }
//  def ibmc(file:File,out:File){
//    val cal = Calendar.getInstance()
//    val trueCal = Calendar.getInstance()
//    trueCal.set(1987,0,1)
//    val sb = new StringBuilder
//    val reader = new BufferedReader(new FileReader(file))
//    val writer = new PrintWriter(out)
//    try{
//      var line = reader.readLine()
//      while(line!=null){
//        if (!line.endsWith(".xml") && !line.startsWith("/")){
//          val dateField = line.split("/data/")(1)
//          println(dateField)
//          val date = new SimpleDateFormat("yyyy/MM/dd", Locale.US).parse(dateField)
//          cal.setTime(date)
//          println(cal.getTime)
//          while(!dateCompare(trueCal,cal)){
//            trueCal.add(Calendar.DATE, 1)
//            println(trueCal.getTime)
//            writer.write(0+"\n")
//          }
//          while()
//        }
//
//
//        writer.write(stockPrice+"\n")
//        line = reader.readLine()
//        trueCal.add(Calendar.DATE, 1)
//      }
//      println("Final True date "+trueCal.getTime)
//    }
//    finally {
//      if(writer ne null)
//        writer.close()
//      if (reader ne null)
//        reader.close()
//    }
////    /iesl/canvas/sameer/dat/nyt/data/1987/01/01
////    232
////    /iesl/canvas/sameer/dat/nyt/data/1987/01/01/0000154.xml
////    /iesl/canvas/sameer/dat/nyt/data/1987/01/01/0000187.xml
//
//    //2013-03-14,212.15,215.86,212.15,215.80,5505500,215.80
//  }

  def dateCompare(cal1:Calendar,cal2:Calendar) = (cal1.get(Calendar.YEAR)==cal2.get(Calendar.YEAR)) &&
    (cal1.get(Calendar.MONTH)==cal2.get(Calendar.MONTH)) && (cal1.get(Calendar.DAY_OF_MONTH)==cal2.get(Calendar.DAY_OF_MONTH))

  def processLogFile(r:File,filter:File,o:File){
//    val trueCal = Calendar.getInstance()
//    trueCal.set(1987,0,1)
    val set = filterByFinancialFiles(filter)
//    val writer = new PrintWriter(new FileWriter(o))
    var c = 0
    var n = 0
    for (line <- Source.fromFile(r).getLines()){
      val split = line.split("\t")
      if(split.length!=2){
        //val cal = toDate(line)
//        while(!dateCompare(trueCal,cal)){
//          trueCal.add(Calendar.DATE, 1)
//          println(trueCal.getTime)
//          writer.write(0+"\n")
//        }
//        trueCal.add(Calendar.DATE, 1)
        n+=1
        if (n==6548) println(line)
//        writer.write(c+"\n")
//        c=0
      }
      else{
        if (set(split(0)) && split(1).toInt > 4) c+=1
        //if(split(1).toInt > 1)
        //c+=1
      }
    }
//    writer.close
  }

  def toDate(s:String)={
    val fields = s.split("/").takeRight(3)
    val c = Calendar.getInstance()
    c.set(fields(0).toInt,fields(1).toInt-1,fields(2).toInt)
    c
  }

  def main(args:Array[String]){
    val file = new File(args(0))
    val outFile = new File(args(1))
    val filterFile = new File(args(2))
//    fillInHolidays(file,outFile)
    processLogFile(file,filterFile,outFile)
  }

  def filterByFinancialFiles(file:File)={
    val set = new mutable.HashSet[String]()
    for (line<-Source.fromFile(file).getLines()){
      set+=line
    }
    set
  }
}
