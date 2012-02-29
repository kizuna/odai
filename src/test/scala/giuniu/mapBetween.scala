package org.kizuna.odai.giuniu

import org.scalatest.FunSuite
import org.kizuna.odai.giuniu.mapBetween._

class MapBetween extends FunSuite {

  test("空のListは空のIteratorを返す") {
    val result = List[Int]().mapBetween(_+_)
    assert(result.size === 0)
  }

  test("1 to 10 mapBetween (_+_)") {
    assert((1 to 10).mapBetween(_+_).toList == List(3,5,7,9,11,13,15,17,19))
  }

}
