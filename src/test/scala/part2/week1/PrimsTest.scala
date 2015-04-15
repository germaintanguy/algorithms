package part2.week1

import java.util

import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by germaintanguy on 15/04/15.
 */
class PrimsTest extends FlatSpec with Matchers  {

  "Prims compute" should "return the the overall cost of a minimum spanning tree" in {

    val graph = Array(
      Array(Double.MaxValue,1d,Double.MaxValue,4d),
      Array(1d,Double.MaxValue,2d,Double.MaxValue),
      Array(Double.MaxValue,2d,Double.MaxValue,3d),
      Array(4d,Double.MaxValue,3d,Double.MaxValue)
    )
    val vertices : util.HashSet[Int] = new util.HashSet()
    vertices.add(0)
    vertices.add(1)
    vertices.add(2)
    vertices.add(3)

    val cost = Prims.compute(graph,vertices,0)

    println(cost)
    cost should be(6.0)
  }

}
