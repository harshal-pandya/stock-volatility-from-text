package edu.umass.cs.iesl.harshal.voltext

import java.util.Calendar
import java.text.SimpleDateFormat

/**
 * @author harshal
 * @date: 4/6/13
 */
object TimelineManager {

  def apply(entryDate:String,del:String="-")={
    val fields = entryDate.split(del)
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


class Date{
  val sdf = new SimpleDateFormat("yyyy-MM-dd")
  val c = Calendar.getInstance()
  def incrementDate =  {
    c.add(Calendar.DATE, 1)
    sdf.format(c.getTime())
  }

  def <=(date:Date) = !this.c.after(date.c)
  def this(dt:String) = {
    this()
    c.setTime(sdf.parse(dt))
  }

  def date = sdf.format(c.getTime())
}