package edu.umass.cs.iesl.harshal.voltext

//import redis.clients.jedis.Jedis
import java.io.{FileOutputStream, OutputStreamWriter, File}
import MentionCounter._
import collection.mutable.HashMap
import java.util.zip.GZIPOutputStream

/**
 * @author harshal
 * @date: 4/15/13
 */
object NGrams {
//  def redis(host:String,port:Int,tm:Int) = new Jedis(host,port,tm)
  def unigram(file:File,isXml:Boolean)={
    val tf = new HashMap[String,Int]()
    val fText = fileText(file)
    val tokens = tokenize(if (isXml) stripXml(fText) else fText)
    for (token<-tokens){
      val stem = if (isNumber(token)) number else PorterStemmer.stem(token)
      tf.getOrElseUpdate(stem,0)
      tf(stem)+=1
    }
    tf
  }
  def writer(outFile:File,tf:HashMap[String,Int]){
    val writer = new OutputStreamWriter(new GZIPOutputStream(new FileOutputStream(outFile)),"UTF-8")
    try{
      tf.foreach(kv=>writer.write(kv._1+"\t"+kv._2+"\n"))
    }
    finally {
      if (writer ne null){
        writer.close()
      }
    }
  }
  def isNumber(x: String) = x forall(ch=> ch =='.' || ch.isDigit || ch ==',')
  val number = "###NUMBER###"
}
