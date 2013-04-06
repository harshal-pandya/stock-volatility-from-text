package edu.umass.cs.iesl.harshal.voltext

import java.io.{BufferedReader, FileInputStream, InputStreamReader, File}
import java.util.zip.GZIPInputStream
import cc.factorie.app.nlp.segment.ClearSegmenter
import collection.mutable.ArrayBuffer

/**
 * @author harshal
 * @date: 4/6/13
 */
object MentionCounter {

  def apply(file:File,words:Set[String],isXml:Boolean=false)={
    val fText = fileText(file)
    val tokens = tokenize(stripXml(fText))
    counter(tokens,words)
  }

  def counter(tokens:ArrayBuffer[String],words:Set[String]) = tokens.foldLeft(0)((acc,t)=> if (words(t.toLowerCase)) acc+1 else acc+0 )

  def tokenize(text:String): ArrayBuffer[String] = ClearSegmenter.tokenizer.getTokenList(text).map(_.text)

  def stripXml(text:String) = xml.XML.loadString(text).text

  def fileText(file:File)={
    val reader = getReader(file)
    var line = reader.readLine()
    val sb = new StringBuilder
    while(line!=null){
      sb.append(line+"\n")
      line = reader.readLine()
    }
    sb.toString()
  }

  def getReader(file:File,enc:String="UTF-8"):BufferedReader = {
    if (file.getName.endsWith(".gz")) new BufferedReader(new InputStreamReader(new GZIPInputStream(new FileInputStream(file)),enc))
    else new BufferedReader(new InputStreamReader(new FileInputStream(file),enc))
  }

}
