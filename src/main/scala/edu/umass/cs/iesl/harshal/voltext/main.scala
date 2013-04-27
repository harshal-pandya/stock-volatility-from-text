package edu.umass.cs.iesl.harshal.voltext

import java.io.{OutputStreamWriter, FileOutputStream, PrintWriter, File}
import actors.Futures._
import collection.mutable.ArrayBuffer
import io.Source
import java.util.zip.GZIPOutputStream
import collection.mutable.HashMap

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
//  val wordSet = Set("goldman sachs","the goldman sachs group, inc.","the goldman sachs group inc.","the goldman sachs group inc","the goldman sachs group, inc",
//    "goldman sachs group","the goldman sachs group","the goldman sachs inc.","goldman sachs inc.","goldman sachs inc","goldman sachs inc")
//  val wordSet = Set("ibm","international business machines","international business machines corporation","ibm corporation","ibm corp","i b m","i b m corp","i b m corporation")
//  val wordSet = Set("ibm","i.b.m.")
  val wordSet = Set("unilever")
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
    val tasks = for (f <- fs) yield future {
      val c = MentionCounter(f,wordSet,true)
      if(c>0) println(f.getAbsolutePath+"\t"+c)
      c
    }
    //fs.toSeq.par.map(MentionCounter(_,wordSet,true))
    val counts = awaitAll(60000L,tasks:_*)
    counts.flatten.foldLeft(0)((acc,c)=>acc+c.asInstanceOf[Int])
  }
  def dateToFilePath(date:String,del:String="-") = date.split(del).mkString("/")
}

object Runner2 extends App{
  val filenames = new File(args(0))
  val dictionaryDir = new File(args(1))
  val rarity = args(3).toInt
//  val dictionary = FinancialDictionary.buildDictionary(dictionaryDir)
//  val dictionary = FinancialDictionary.buildRareWordDictionary(dictionaryDir,rarity)
  val dictionary = HashMap("stocks"->0,"stock"->0,"profit"->0,"profits"->0,"loss"->0,"losses"->0)
  val fileList = new ArrayBuffer[String]()
  for (filename<-Source.fromFile(filenames).getLines()){
    val f = new File(filename)
    val res = MentionCounter(f,dictionary,true)
    if (res){
      fileList+=filename
    }
    println(filename)
  }
  writeToFile(fileList,args(2))
  //writeToFile(dictionary.toSeq.sortBy(_._2),args(3))

  def writeToFile[A](s:Seq[A],outF:String){
    val writer = new OutputStreamWriter(new FileOutputStream(outF),"UTF-8")
    try{
      s.foreach(a=>writer.write(a.toString+"\n"))
    }
    finally {
      if (writer ne null){
        writer.close()
      }
    }
  }
}

object Ngramer extends App{
  val filenames = new File(args(0))
  val lookUpTableF = new File(args(2))
  for (filename <- Source.fromFile(filenames).getLines()){
    val f = new File(filename)
    NGrams.fillTables(f,true)
  }
  val filter = NGrams.termFrequency.filter(_._2>25).keySet
  println("Filter Size : "+filter.size)
  for (filename <- Source.fromFile(filenames).getLines()){
    val f = new File(filename)
    val tf = NGrams.unigram(f,filter,true)
    val temp = f.getAbsolutePath.split("/")
    val outFile = new File(args(1)+temp.takeRight(4).take(3).mkString("/")+"/"+temp.last.split("""\.""")(0)+".tsv.gz")
    outFile.getParentFile.mkdirs()
    NGrams.writer(outFile,tf)
  }
  NGrams.saveLookupTable(lookUpTableF)
  println("Vocabulary size : "+NGrams.lookupTable.size)
  val writer = new OutputStreamWriter(new GZIPOutputStream(new FileOutputStream(args(3))))
  try{
    NGrams.termFrequency.toSeq.sortBy(_._2).foreach(kv=>writer.write(kv._1+"\t"+kv._2+"\n"))
  }
  finally {
    if (writer ne null){
      writer.close()
    }
  }
}


object Features extends App{
  val ibm_dir = new File(args(0))
  val D = args(1).toInt
  val V = args(2).toInt
  val NYT_END_DATE = new Date("2007-06-19")
  val entryDate = "1987-01-01"
  val currentDate = new Date(entryDate)
  while(currentDate<=NYT_END_DATE){
    val files = getFilesForTheWeek
//    println("concating "+files.length+" files")
    val term_freq = Document.concat(files).toMap
//    println("Term Frequency : "+term_freq.size)
    val FV = new FeatureVector(V)
//    println("D="+term_freq.values.sum)
    val maxd = term_freq.values.max
    val tf = FV.tf(term_freq,maxd)
//    println(tf)
    write(tf,new File(args(3)))
    val tfidf = FV.tfidf(term_freq,maxd,D)
//    println(tfidf)
    write(tfidf,new File(args(4)))
    val logp = FV.log1p(term_freq)
//    println(logp.filter(_!=0).mkString(" | "))
    write(logp,new File(args(5)))
  }
  def write(fv:Array[Double],file:File){
    val writer = new OutputStreamWriter(new GZIPOutputStream(new FileOutputStream(file,true)),"UTF-8")
    try{
      writer.write(fv.mkString(",")+"\n")
    }
    finally {
      if (writer ne null){
        writer.close()
      }
    }
  }

  def getFilesForTheWeek:Seq[File] = {
    val files =
    for (day <- 1 to 7) yield {
      val date = currentDate.date
      val fs = new File(ibm_dir.getAbsolutePath+"/"+dateToFilePath(date))
      currentDate.incrementDate
      if (fs.exists()) {
        fs.listFiles()
      }else{
        Array[File]()
      }
    }
    files.flatten
  }
  def dateToFilePath(date:String,del:String="-") = date.split(del).mkString("/")
}