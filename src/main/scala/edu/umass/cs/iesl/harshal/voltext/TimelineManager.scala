package edu.umass.cs.iesl.harshal.voltext

import java.util.Calendar

/**
 * @author harshal
 * @date: 4/6/13
 */
object TimelineManager {

  def apply(entryDate:String)={
    val fields = entryDate.split("/")
    val start = getDayOfWeek(fields(0).trim.toInt,fields(1).trim.toInt,fields(2).trim.toInt)
    val initialOffest = 7-start
    initialOffest
  }

  def getDayOfWeek(year:Int,month:Int,day:Int)={
    val c = Calendar.getInstance()
    c.set(year,month-1,day-1)
    c.get(Calendar.DAY_OF_WEEK)
  }

}
