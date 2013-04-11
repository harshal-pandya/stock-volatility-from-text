package edu.umass.cs.iesl.harshal.voltext

import java.io.{BufferedReader, FileInputStream, InputStreamReader, File}
import java.util.zip.GZIPInputStream
import cc.factorie.app.nlp.segment.ClearSegmenter
import edu.stanford.nlp.ie.crf.CRFClassifier

/**
 * @author harshal
 * @date: 4/6/13
 */
object MentionCounter {
  val serializedClassifier = "/iesl/canvas/harshal/development/voltext/english.all.3class.distsim.crf.ser.gz"
  val classifier = CRFClassifier.getClassifierNoExceptions(serializedClassifier)
  val NameExtractor = """<ORGANIZATION>(.+?)</ORGANIZATION>""".r
  def apply(file:File,words:Set[String],isXml:Boolean=false)={
    val fText = fileText(file)
//    val taggedText = ner(stripXml(fText))
//    val matches = find(taggedText)
//    val tokens = for (org<-matches) yield {
//      val NameExtractor(name) = org
//      stripPunctuation(name)
//    }
    val tokens = tokenize(stripXml(fText))
    if (counter(tokens,words)>0) 1
    else 0
  }

  def find(text:String) = """<ORGANIZATION>.+?</ORGANIZATION>""".r.findAllIn(text).toList

  def counter(tokens:Seq[String],words:Set[String]) = tokens.foldLeft(0)((acc,t)=> if (words(t.toLowerCase.trim)) acc+1 else acc+0 )

  def tokenize(text:String) = ClearSegmenter.tokenizer.getTokenList(text).map(_.text)

  def stripPunctuation(text:String) = text.toLowerCase.replaceAll("""[^\p{L}]"""," ").replaceAll("""\s+"""," ")

  def ner(text:String) = classifier.classifyWithInlineXML(text)

  def stripXml(text:String) = xml.XML.loadString(text).text

  def fileText(file:File)={
    val reader = getReader(file)
    reader.readLine()
    reader.readLine()
    var line = reader.readLine()
    val sb = new StringBuilder
    while(line!=null){
      sb.append(line+"\n")
      line = reader.readLine()
    }
    reader.close()
    sb.toString()
  }

  def getReader(file:File,enc:String="UTF-8"):BufferedReader = {
    if (file.getName.endsWith(".gz")) new BufferedReader(new InputStreamReader(new GZIPInputStream(new FileInputStream(file)),enc))
    else new BufferedReader(new InputStreamReader(new FileInputStream(file),enc))
  }

}
