package edu.umass.cs.iesl.harshal.voltext

//import redis.clients.jedis.Jedis
import java.io.{FileOutputStream, OutputStreamWriter, File}
import MentionCounter._
import collection.mutable.HashMap
import java.util.zip.GZIPOutputStream
//import collection.mutable
import edu.umass.cs.iesl.harshal.voltext.StopWords

/**
 * @author harshal
 * @date: 4/15/13
 */
object NGrams {
  val index = new Index
  val number = "###NUMBER###"
  val lookupTable = HashMap[String,Int]()
  val termFrequency = HashMap[String,Int]()
  //  def jedis(host:String,port:Int,tm:Int) = new Jedis(host,port,tm)
  def unigram(file:File,filter:collection.Set[String],isXml:Boolean)={
    val tf = new HashMap[Int,Int]()
    val fText = fileText(file)
    val tokens = tokenize(if (isXml) stripXml(fText) else fText)
    for (token<-tokens){
      val stem = PorterStemmer.stem(token)
      if (filter(stem)){
        val code = lookupTable.getOrElseUpdate(stem,index.incr)
        tf.getOrElseUpdate(code,0)
        tf(code)+=1
      }
    }
    tf
  }

  def fillTables(file:File,isXml:Boolean){
    val fText = fileText(file)
    val tokens = tokenize(if (isXml) stripXml(fText) else fText)
    for (token<-tokens){
      if (!rules(token.toLowerCase())){
        val stem = PorterStemmer.stem(token)
        termFrequency(stem)=termFrequency.getOrElseUpdate(stem,0)+1
        //val code = lookupTable.getOrElseUpdate(stem,index.incr)
      }
    }
  }

  def rules(token:String):Boolean={
    token match {
      case t if(t.startsWith("'")) => true
      case t if(t.find(_.isLetter)==None) => true
      case t if(isNumber(token)) => true
      case _ => StopWords(token)
    }
  }

  def writer(outFile:File,tf:HashMap[Int,Int]){
    if(outFile.exists()) outFile.delete()
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
  def saveLookupTable(f:File){
    val writer = new OutputStreamWriter(new GZIPOutputStream(new FileOutputStream(f)))
    try{
      lookupTable.toSeq.sortBy(_._2).foreach(kv=>writer.write(kv._1+"\n"))
    }
    finally {
      if (writer ne null){
        writer.close()
      }
    }
  }
}

class Index{
  private var i = 0
  def incr = {
    i+=1
    i
  }
}