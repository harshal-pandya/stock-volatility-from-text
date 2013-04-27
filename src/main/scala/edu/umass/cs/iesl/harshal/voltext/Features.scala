package edu.umass.cs.iesl.harshal.voltext

/**
 * @author harshal
 * @date: 4/16/13
 */
class FeatureVector(V:Int) {
  def tf(freq:Map[Int,Int],d:Int)={
    val FV = Array.ofDim[Double](V)
    for (v<-0 until V){
        FV(v) = FeatureUtils.tf(freq.getOrElse(v,0),d)
    }
    FV
  }
  def tfidf(freq:Map[Int,Int],d:Int,N:Int)={
    val Feature_Matrix = Array.ofDim[Double](V)
    for (v<-0 until V){
      Feature_Matrix(v) = FeatureUtils.tfidf(freq.getOrElse(v,0),d,N)
    }
    Feature_Matrix
  }
  def log1p(freq:Map[Int,Int])={
    val Feature_Matrix = Array.ofDim[Double](V)
    for (v<-0 until V){
      Feature_Matrix(v) = math.log1p(freq.getOrElse(v,0).toDouble)
    }
    Feature_Matrix
  }
}
object FeatureUtils{
  def tf(f:Int,d:Int) = f.toDouble/d
  def tfidf(f:Int,d:Int,N:Int) = if (f>0) (f.toDouble/d)*math.log(N.toDouble/f) else 0.0
}

