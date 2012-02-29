package org.kizuna.odai.giuniu

object mapBetween {

  implicit def seqToExtSeq[A](seq: Seq[A]) = new {
    def mapBetween[B](f: (A, A) => B): Iterator[B] = {
      seq.sliding(2).map(s => f(s(0), s(1)))
    }
  }

  def main(args: Array[String]) {
    val range2 = 1 to 10
    println(range2.mapBetween(_ + _) mkString ",")
    println(range2.mapBetween(_ * _) mkString ",")
    println(range2.mapBetween(_ - _) mkString ",")
  }

}
