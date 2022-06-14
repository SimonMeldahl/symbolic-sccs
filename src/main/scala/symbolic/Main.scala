package symbolic

import symbolic.BDD.*
import symbolic.BooleanNetwork.*
import symbolic.colored.Heuristic.*
import symbolic.colored.LockStep.*
import symbolic.colored.Skeleton.*
import symbolic.colored.Test.*
import symbolic.colored.*

import scala.util.Random

object Main {
  def main(args: Array[String]): Unit = {
    time(Simple)
    time(LockStep)
    time(Skeleton)
    time(Skeletoff)
    time(Heuristic)
  }

  def time(f: (BDD, BDD | Map[BDDVar, BDD], BDD) => List[BDD]): Unit = {
    val runs = 5
    var sum: Long = 0
    for (_ <- 1 to runs) {
      reset()
      val (v, e, _, c) = readAeon(g2a)
      val start = System.currentTimeMillis()
      f(v, e, c)
      val end = System.currentTimeMillis()
      sum += (end - start)
    }
    println(s"$f time = ${sum/runs}")
  }

  def example1(): Unit = {
    reset()

    val (v, c) = gen(4, 1)
    val E = e(0, 0, 2) \/
      e(0, 0, 1) \/
      e(1, 0, 0) \/
      e(2, 0, 1) \/
      e(3, 0, 2)

    printGraph(v, E, c, Simple(v, E, c))
  }

  def example2(): Unit = {
    reset()

    val (v, c) = gen(9, 2)
    val E = e(0, 0, 2) \/
      e(1, 0, 0) \/
      e(2, 0, 0) \/
      e(2, 0, 1) \/
      e(2, 0, 4) \/
      e(3, 0, 2) \/
      e(3, 0, 5) \/
      e(4, 0, 5) \/
      e(5, 0, 6) \/
      e(6, 0, 8) \/
      e(7, 0, 5) \/
      e(8, 0, 7) \/
      e(0, 1, 2) \/
      e(1, 1, 0) \/
      e(2, 1, 0) \/
      e(2, 1, 1) \/
      e(2, 1, 4) \/
      e(3, 1, 2) \/
      e(3, 1, 5) \/
      e(4, 1, 5) \/
      e(5, 1, 6) \/
      e(6, 1, 8) \/
      e(7, 1, 5) \/
      e(8, 1, 7)

    printGraph(v, E, c, Skeleton(v, E, c))
  }

  def example3(): Unit = {
    reset()

    val (v, c) = gen(4, 3)
    val E = e(0, 0, 1) \/
      e(1, 0, 2) \/
      e(2, 0, 3) \/
      e(3, 0, 0) \/
      e(1, 1, 2) \/
      e(2, 1, 1) \/
      e(3, 1, 2) \/
      e(0, 2, 2) \/
      e(2, 2, 0) \/
      e(2, 2, 1)

    printGraph(v, E, c, Skeleton(v, E, c))
  }

  def example4(): Unit = {
    reset()

    val (v, c) = gen(4, 1)
    val E = e(0, 0, 2) \/
      e(0, 0, 3) \/
      e(1, 0, 2) \/
      e(2, 0, 0) \/
      e(3, 0, 0) \/
      e(3, 0, 1) \/
      e(3, 0, 2)

    printGraph(v, E, c, LockStep(v, E, c))
  }

  def example5(): Unit = {
    reset()

    val (v, c) = gen(3, 2)
    val E = e(0, 0, 1) \/
      e(0, 0, 2) \/
      e(1, 0, 0) \/
      e(1, 0, 2) \/
      e(2, 0, 1) \/
      e(0, 1, 2) \/
      e(1, 1, 0) \/
      e(2, 1, 0)

    printGraph(v, E, c, LockStep(v, E, c))
  }

  def example6(): Unit = {
    reset()

    val (v, c) = gen(3, 2)
    val E = e(0, 0, 2) \/
      e(1, 0, 0) \/
      e(1, 0, 2) \/
      e(2, 0, 0) \/
      e(2, 0, 1) \/
      e(0, 1, 1) \/
      e(1, 1, 0) \/
      e(1, 1, 2)

    printGraph(v, E, c, LockStep(v, E, c))
  }

  def example7(): Unit = {
    reset()
    val (v, c) = gen(4, 1)
    val E = e(0, 0, 1) \/ e(0, 0, 2) \/ e(1, 0, 0) \/ e(2, 0, 3) \/ e(3, 0, 1)
    printGraph(v, E, c, LockStep(v, E, c))
  }

  def example8(): Unit = {
    reset()
    val (v, c) = gen(4, 1)
    val E = e(2,0,0) \/ e(3,0,2)
    printGraph(v, E, c, Skeleton(v, E, c))
  }

  def example9(): Unit = {
    reset()
    val (v, c) = gen(5, 2)
    val E = e(0,0,1) \/ e(0,0,3) \/ e(3,0,2) \/ e(3,0,4) \/ e(4,0,3) \/
      e(0,1,1) \/ e(2,1,1) \/ e(3,1,0) \/ e(3,1,2) \/ e(3,1,4) \/ e(4,1,3)
    printGraph(v, E, c, Skeleton(v, E, c))
  }
  
  def example10(): Unit = {
    reset()
    val (v, c) = gen(3, 1)
    val E = e(0,0,1) \/ e(1,0,0) \/ e(0,0,2) \/ e(2,0,1)
    printGraph(v, E, c, LockStep(v, E, c))
  }

  def skeleExample(): Unit = {
    def cluster(start: Int, end: Int, color: Int): BDD = {
      val res = FALSE
      for (i <- start to end) {
        for (j <- start to end if i != j) {
          res \/= e(i, color, j)
        }
      }
      res
    }

    reset()
    val (v, c) = gen(27, 2)
    // 0 = blå, 1 = rød
    val E = cluster(0,2,0) \/ cluster(3,5,0) \/ cluster(6,8,0) \/
      cluster(9,11,0) \/ cluster(12,14,0) \/ cluster(15,17,0) \/
      cluster(18,20,0) \/ cluster(21,23,0) \/ cluster(24,26,0) \/
      e(4,0,1) \/ e(7,0,4) \/ e(13,0,7) \/ e(13,0,10) \/
      e(16,0,13) \/ e(19,0,13) \/ e(22,0,19) \/ e(25,0,22) \/
      cluster(0,2,1) \/ cluster(3,5,1) \/ cluster(6,8,1) \/
      cluster(9,11,1) \/ cluster(12,14,1) \/ cluster(15,17,1) \/
      cluster(18,20,1) \/ cluster(21,23,1) \/ cluster(24,26,1) \/
      e(1,1,4) \/ e(4,1,7) \/ e(7,1,13) \/ e(10,1,13) \/
      e(13,1,16) \/ e(13,1,19) \/ e(19,1,22) \/ e(22,1,25)

    val sccs = Heuristic(v, E, c)
    printGraph(v, E, c, sccs)
  }

  /**
   * Generate BDD for specified number of vertices and colors
   *
   * @param nodeCount  number of nodes
   * @param colorCount number of colors
   * @return BDD for sets of nodes and colors
   */
  def gen(nodeCount: Int, colorCount: Int): (BDD, BDD) = {
    def createVars(count: Int): List[BDDVar] =
      (count - 1).toBinaryString.map(_ => createVar()).toList

    def dnf(l: List[BDDVar], count: Int): BDD = {
      (0 until count).foldLeft(FALSE)((res, n) => res \/ encodeNode(n, l))
    }

    fromVars = createVars(nodeCount)
    toVars = createVars(nodeCount)
    interleaveFromToVars()
    colorVars = createVars(colorCount)
    indexVars = createVars(nodeCount)

    (dnf(fromVars, nodeCount), dnf(colorVars, colorCount))
  }

  /**
   * Generate specified directed edge as BDD.
   * Note: edge is specified in decimal notation, not binary
   *
   * @param from  from vertex
   * @param color color of edge
   * @param to    to vertex
   * @return BDD representation of edge
   */
  def e(from: Int, color: Int, to: Int): BDD = {
    encodeNode(from, fromVars) /\ encodeNode(color, colorVars) /\ encodeNode(to, toVars)
  }
  
  def pre(S: BDD)(using E: BDD | Map[BDDVar, BDD]): BDD = E match {
    case E@_: BDD => BDD.pre(S, E)
    case E@_: Map[BDDVar, BDD] => BooleanNetwork.pre(S, E)
  }

  def post(S: BDD)(using E: BDD | Map[BDDVar, BDD]): BDD = E match {
    case E@_: BDD => BDD.post(S, E)
    case E@_: Map[BDDVar, BDD] => BooleanNetwork.post(S, E)
  }
}
