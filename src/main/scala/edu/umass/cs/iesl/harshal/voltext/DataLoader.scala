package edu.umass.cs.iesl.harshal.voltext

import java.io.{PrintWriter, FileReader, BufferedReader, File}
import io.Source
import java.text.SimpleDateFormat
import java.util.{Calendar, Locale}

/**
 * @author harshal
 * @date: 4/11/13
 */
object DataLoader {
  def ibm(file:File,out:File){
    val cal = Calendar.getInstance()
    val trueCal = Calendar.getInstance()
    trueCal.set(1987,1,1)
    val sb = new StringBuilder
    val reader = new BufferedReader(new FileReader(file))
    var line = reader.readLine()
    line = reader.readLine()
    while(line!=null){
      val fields = line.split(",")
      val dateField = fields(0)
      val stockPrice = fields(6).toDouble
      val date = new SimpleDateFormat("yyyy-MM-dd", Locale.US).parse(dateField)
      cal.setTime(date)
      while(!dateCompare(trueCal,cal)){
        trueCal.add(Calendar.DATE, 1)
        sb.append(0+"\n")
      }
      sb.append(stockPrice+"\n")
      line = reader.readLine()
    }
    println("Final True date "+trueCal.getTime)
    reader.close()

    val writer = new PrintWriter(out)
    try{
    writer.write(sb.toString())
    }
    finally {
      if(writer ne null)
        writer.close()
    }
      //2013-03-14,212.15,215.86,212.15,215.80,5505500,215.80
  }

  def dateCompare(cal1:Calendar,cal2:Calendar) = (cal1.get(Calendar.YEAR)==cal2.get(Calendar.YEAR)) &&
    (cal1.get(Calendar.MONTH)==cal2.get(Calendar.MONTH)) && (cal1.get(Calendar.DAY_OF_MONTH)==cal2.get(Calendar.DAY_OF_MONTH))

  def main(args:Array[String]){
    val file = new File(args(0))
    val outFile = new File(args(1))
    ibm(file,outFile)
  }
}
