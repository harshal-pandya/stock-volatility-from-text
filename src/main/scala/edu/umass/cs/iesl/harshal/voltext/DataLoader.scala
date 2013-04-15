package edu.umass.cs.iesl.harshal.voltext

import java.io._
import io.Source
import java.text.SimpleDateFormat
import java.util.{Calendar, Locale}

/**
 * @author harshal
 * @date: 4/11/13
 */
object DataLoader {
  def ibmv(file:File,out:File){
    val cal = Calendar.getInstance()
    val trueCal = Calendar.getInstance()
    trueCal.set(1987,0,1)
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

  def processLogFile(r:File,o:File){
    val writer = new PrintWriter(new FileWriter(o))
    var c = 0
    for (line <- Source.fromFile(r).getLines()){
      val split = line.split("\t")
      if(split.length!=2){
        writer.write(c+"\n")
        c=0
      }
      else{
        if (split(1).toInt > 1) c+=1
      }
    }
    writer.close
  }

  def main(args:Array[String]){
    val file = new File(args(0))
    val outFile = new File(args(1))
    ibmv(file,outFile)
//    processLogFile(file,outFile)
  }
}
