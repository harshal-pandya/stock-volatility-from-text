package edu.umass.cs.iesl.harshal.voltext

import cc.mallet.types._
import cc.mallet.pipe._
import cc.mallet.topics._
import java.util._
import java.util.regex._
import java.io._
import scala.Predef._
import io.Source

/**
 * @author harshal
 * @date: 4/27/13
 */
object TopicModelling extends App{

  val fileList = new File(args(0))
  var i = 1
  var j = 1
  val tempList = new ArrayList[Instance]()
  for (line <- Source.fromFile(fileList).getLines()){
    val split = line.split("\t")
    if(split.length!=2){
      if (i==1 && j==2){
        run(tempList.iterator())
        j=1
        i+=1
        println("Week No : "+ i+"\n")
      }
      else if (j==7){
        run(tempList.iterator())
        j=1
        i+=1
      }
      else{
        j+=1
      }
    }
    else{
      val fText = MentionCounter.fileText(new File(split(0)))
      val in = new Instance(MentionCounter.stripXml(fText).trim,"X",split(0),"NYT")
      tempList.add(in)
    }
  }
  def run(instanceIt:java.util.Iterator[Instance]){
    val pipeList = new ArrayList[Pipe]()
    pipeList.add(new CharSequenceLowercase())
    pipeList.add(new CharSequence2TokenSequence(Pattern.compile("\\p{L}[\\p{L}\\p{P}]+\\p{L}")))
    pipeList.add(new TokenSequenceRemoveStopwords(new File("stoplists/en.txt"), "UTF-8", false, false, false))
    pipeList.add(new TokenSequence2FeatureSequence())

    val instances = new InstanceList (new SerialPipes(pipeList))
    instances.addThruPipe(instanceIt)

    val numTopics = 100
    val model = new ParallelTopicModel(numTopics, 1.0, 0.01)
    // Use two parallel samplers, which each look at one half the corpus and combine
    //  statistics after every iteration.
    model.setNumThreads(2)

    // Run the model for 50 iterations and stop (this is for testing only,
    //  for real applications, use 1000 to 2000 iterations)
    model.setNumIterations(10)
    model.estimate()

    // Show the words and topics in the first instance

    // The data alphabet maps word IDs to strings
    val dataAlphabet = instances.getDataAlphabet()

    val tokens = model.getData().get(0).instance.getData().asInstanceOf[FeatureSequence]
    val topics = model.getData().get(0).topicSequence

    for (position <- 0 until tokens.getLength()) {
      println("%s-%d " format(dataAlphabet.lookupObject(tokens.getIndexAtPosition(position)),topics.getIndexAtPosition(position)))
    }
    // Estimate the topic distribution of the first instance,
    //  given the current Gibbs state.
    val topicDistribution = model.getTopicProbabilities(0)

    // Get an array of sorted sets of word ID/count pairs
    val topicSortedWords = model.getSortedWords()

    // Show top 5 words in topics with proportions for the first document
    for (topic <- 0 until numTopics){
      val iterator = topicSortedWords.get(topic).iterator()

      println("%d\t%.3f\t" format( topic, topicDistribution(topic)))
      var rank = 0
      while (iterator.hasNext() && rank < 5) {
        val idCountPair = iterator.next()
        println("%s (%.0f) " format(dataAlphabet.lookupObject(idCountPair.getID()), idCountPair.getWeight()))
        rank+=1
      }
    }
  }
}
