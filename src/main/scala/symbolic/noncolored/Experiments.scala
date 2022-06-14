package symbolic.noncolored

import symbolic.*
import symbolic.BDD.*
import symbolic.BooleanNetwork.*
import symbolic.SyntheticGraphs.*
import symbolic.Main.gen

object Experiments {
  def main(args: Array[String]): Unit = {
    timeOneColor()
    timeAllColors()
    benchmarks()
  }

  def timeOneColor(): Unit = {
    def pickIthColor(i: Int, c: BDD): BDD = {
      var colors = c
      for (_ <- 1 to i) {
        colors = colors \ pick(colors, colorVars)
      }
      pick(colors, colorVars)
    }
    def timeOneColor(f: (BDD, BDD | Map[BDDVar, BDD]) => List[BDD]): Unit = {
      val runs = 5
      var times = List[Long]()
      for (_ <- 1 to runs) {
        reset()
        val (v, e, _, c) = readAeon(buddingIron)
        val color = pickIthColor(0, c)
//        val color = pickIthColor(1000, c)
//        val color = pickIthColor(50000, c)

//        val (v, e, c) = G1$()
//        val (v, e, c) = G2$()
//        val (v, e, c) = G3$()
//        val color = pick(c, colorVars)
        val monoEdges = exist(e /\ color, colorVars.reduce(and))
        val start = System.currentTimeMillis()
        f(v, monoEdges)
        val end = System.currentTimeMillis()
        times = (end - start) :: times
      }
      println(s"$f times (avg = ${times.sum / times.length}) = ${times.mkString(", ")}")
    }

    timeOneColor(Simple)
    timeOneColor(LockStep)
    timeOneColor(Skeleton)
    timeOneColor(Skeletoff)
    timeOneColor(Heuristic)
  }

  def timeAllColors(): Unit = {
    def timeAllColors(f: (BDD, BDD | Map[BDDVar, BDD]) => List[BDD]): Unit = {
      val runs = 5
      var times = List[Long]()
      for (_ <- 1 to runs) {
        reset()
        var sum: Long = 0
        val (v, e, _, c) = readAeon(g2a)
//        val (v, e, _, c) = readAeon(buddingOrlando)
//        val (v, e, _, c) = readAeon(tcr)
//        val (v, e, c) = G1()
//        val (v, e, c) = G2()
//        val (v, e, c) = G3()
        var colors = c
        while (colors != FALSE) {
          val start = System.currentTimeMillis()
          val color = pick(colors, colorVars)
          val monoEdges = exist(e /\ color, colorVars.reduce(and))
          f(v, monoEdges)
          colors = colors \ color
          val end = System.currentTimeMillis()
          sum += (end - start)
        }
        times = sum :: times
      }
      println(s"$f times (avg = ${times.sum / times.length}) = ${times.mkString(", ")}")
    }

    timeAllColors(Simple)
    timeAllColors(LockStep)
    timeAllColors(Skeleton)
    timeAllColors(Skeletoff)
    timeAllColors(Heuristic)
  }

  def benchmarks(): Unit = {
    def time(f: (BDD, BDD | Map[BDDVar, BDD]) => List[BDD], g: () => (BDD, BDD)): Unit = {
      val runs = 50
      var times = List[Long]()
      for (_ <- 1 to runs) {
        val (v, e) = g()
        val start = System.currentTimeMillis()
        f(v, e)
        val end = System.currentTimeMillis()
        times = (end - start) :: times
      }
      println(s"$f times (avg = ${times.sum / times.length}) = ${times.mkString(", ")}")
    }

    for (i <- 0 to 10) {
      println(s"i = $i")
      def graph(): (BDD, BDD) = {
        reset()
        val (v, c) = gen(2 ** 10, 1)
        val (gv, ge) = cross(encodeNode(0, fromVars), FALSE, line, 2 ** (10 - i), c)
        val (_, e) = cross(gv, ge, cycle, 2 ** i, c)
        (v, exist(e, colorVars.reduce(and)))
      }
      time(Simple, graph)
      time(LockStep, graph)
      time(Skeleton, graph)
      time(Skeletoff, graph)
      time(Heuristic, graph)
      println("------------------------")
    }
  }
}
