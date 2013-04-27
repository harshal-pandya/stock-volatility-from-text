package edu.umass.cs.iesl.harshal.voltext

import io.Source

/**
 * @author harshal
 * @date: 4/20/13
 */
object StopWords {
  private val set = Source.fromFile("/iesl/canvas/harshal/development/voltext/src/resources/stopwords").getLines().map(_.trim).toSet
  def apply(s:String) = set(s)
}
