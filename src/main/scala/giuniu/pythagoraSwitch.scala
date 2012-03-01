package org.kizuna.odai.giuniu

import scala.math._
import scala.collection.GenSeq
import scala.collection.parallel.immutable.ParSeq

object pythagoraSwitch {

  def main(args: Array[String]): Unit = {
    val max = 1000
    execute(max, searchByFor)
    execute(max, searchByMap)
    execute(max, searchByParMap)
  }

  def execute(max:Int, f: Int => GenSeq[(Int, Int, Int)]) = {
    val start = System.nanoTime()
    val result = f(max)
    val end = System.nanoTime()
    //result foreach println
    println("count:" + result.length)
    println("time:" + ((end - start) / 1000000))
  }

  def searchByFor(max: Int): Seq[(Int, Int, Int)] = {
    for (
      c <- 3 to max by 2;
      c2 = c.toLong * c;
      b <- (c / sqrt(2)).ceil.toInt until c;
      a2 = c2 - b.toLong * b;
      a = sqrt(a2).toInt;
      if (a2 == a.toLong * a) && (gcd(c, gcd(b, a)) == 1)
    ) yield (a, b, c)
  }

  def searchByMap(max: Int): Seq[(Int, Int, Int)] = {
    (3 to max by 2) flatMap(c => {
      val c2 = c.toLong * c
      (c / sqrt(2)).ceil.toInt until c filter (b=> {
        val a2 = c2 - b.toLong * b
        val a = sqrt(a2).toInt
        (a2 == a.toLong * a) && (gcd(c, gcd(b, a)) == 1)
      }) map (b => {
        val a2 = c2 - b.toLong * b
        val a = sqrt(a2).toInt
        (a, b, c)
      })
    })
  }

  def searchByParMap(max: Int): ParSeq[(Int, Int, Int)] = {
    (3 to max by 2).par flatMap(c => {
      val c2 = c.toLong * c
      (c / sqrt(2)).ceil.toInt until c withFilter (b=> {
        val a2 = c2 - b.toLong * b
        val a = sqrt(a2).toInt
        (a2 == a.toLong * a) && (gcd(c, gcd(b, a)) == 1)
      }) map (b => {
        val a2 = c2 - b.toLong * b
        val a = sqrt(a2).toInt
        (a, b, c)
      })
    })
  }

  def gcd(max: Int, min: Int): Int = (max % min) match {
    case 0 => min
    case r => gcd(min, r)
  }
}
