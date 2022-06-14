package symbolic

import BDD.*
import Main.*
import symbolic.colored.{Heuristic, LockStep, Simple, Skeletoff, Skeleton}

object SyntheticGraphs {
  def main(args: Array[String]): Unit = {
    benchmarks()
  }

  extension (x: Int) {
    def **(y: Int): Int = math.pow(x, y).toInt
  }

  def G1(): (BDD, BDD, BDD) = {
    reset()
    val (v, c) = gen(2**15, 4)
    val (gv, ge) = cross(encodeNode(0, fromVars), FALSE, line, 2**5, encodeNode(0, colorVars) \/ encodeNode(1, colorVars))
    val (gv1, ge1) = cross(gv, ge, line, 2**5, encodeNode(1, colorVars) \/ encodeNode(2, colorVars))
    val (_, e) = cross(gv1, ge1, cycle, 2**5, encodeNode(3, colorVars))
    (v, e, c)
    // Simple = 1948, LockStep = 688, Skeleton = 1791, Heuristic = 1415, Skeletoff = 1117, SkeletoffHeuristic = 758
  }

  def G2(): (BDD, BDD, BDD) = {
    reset()
    val (v, c) = gen(2**15, 4)
    val (gv, ge) = cross(encodeNode(0, fromVars), FALSE, cycle, 2**5, encodeNode(0, colorVars) \/ encodeNode(1, colorVars))
    val (gv1, ge1) = cross(gv, ge, cycle, 2**5, encodeNode(1, colorVars) \/ encodeNode(2, colorVars))
    val (_, e) = cross(gv1, ge1, line, 2**5, encodeNode(3, colorVars))
    (v, e, c)
  }

  def G3(): (BDD, BDD, BDD) = {
    reset()
    val (v, c) = gen(2**16, 4)
    val (gv, ge) = cross(encodeNode(0, fromVars), FALSE, cycle, 2**5, c)
    val (gv1, ge1) = cross(gv, ge, line, 2**3, c \ encodeNode(3, colorVars))
    val (gv2, ge2) = cross(gv1, ge1, cycle, 2**3, encodeNode(0, colorVars) \/ encodeNode(1, colorVars))
    val (_, e) = cross(gv2, ge2, line, 2**5, encodeNode(0, colorVars))
    (v, e, c)
    // Simple = 3962, LockStep = 3272, Skeleton = 23813, Heuristic = 18842, SkeletoffHeuristic = 5087
  }

  def G1$(): (BDD, BDD, BDD) = {
    reset()
    val (v, c) = gen(2**18, 1)
    val (gv, ge) = cross(encodeNode(0, fromVars), FALSE, line, 2**6, encodeNode(0, colorVars))
    val (gv1, ge1) = cross(gv, ge, line, 2**6, encodeNode(0, colorVars))
    val (_, e) = cross(gv1, ge1, cycle, 2**6, encodeNode(0, colorVars))
    (v, e, c)
  }

  def G2$(): (BDD, BDD, BDD) = {
    reset()
    val (v, c) = gen(2**18, 1)
    val (gv, ge) = cross(encodeNode(0, fromVars), FALSE, cycle, 2**6, encodeNode(0, colorVars))
    val (gv1, ge1) = cross(gv, ge, cycle, 2**6, encodeNode(0, colorVars))
    val (_, e) = cross(gv1, ge1, line, 2**6, encodeNode(0, colorVars))
    (v, e, c)
  }

  def G3$(): (BDD, BDD, BDD) = {
    reset()
    val (v, c) = gen(2**18, 1)
    val (gv, ge) = cross(encodeNode(0, fromVars), FALSE, cycle, 2**5, encodeNode(0, colorVars))
    val (gv1, ge1) = cross(gv, ge, line, 2**4, encodeNode(0, colorVars))
    val (gv2, ge2) = cross(gv1, ge1, cycle, 2**4, encodeNode(0, colorVars))
    val (_, e) = cross(gv2, ge2, line, 2**5, encodeNode(0, colorVars))
    (v, e, c)
  }

  def benchmarks(): Unit = {
    val size = 10
    for (i <- 0 to size) {
      println(s"i = $i")
      def graph(): (BDD, BDD, BDD) = {
        reset()
        val (v, c) = gen(2 ** size, 1)
        val (gv, ge) = cross(encodeNode(0, fromVars), FALSE, line, 2 ** (size - i), c)
        val (_, e) = cross(gv, ge, cycle, 2 ** i, c)
        (v, e, c)
      }
      time(Simple, graph)
      time(LockStep, graph)
      time(Skeleton, graph)
      time(Skeletoff, graph)
      time(Heuristic, graph)
      println("------------------------")
    }
  }

  def time(f: (BDD, BDD | Map[BDDVar, BDD], BDD) => List[BDD], g: () => (BDD, BDD, BDD)): Unit = {
    val runs = 10
    var times = List[Long]()
    for (_ <- 1 to runs) {
      val (v, e, c) = g()
      val start = System.currentTimeMillis()
      f(v, e, c)
      val end = System.currentTimeMillis()
      times = (end - start) :: times
    }
    println(s"$f times (avg = ${times.sum / times.length}) = ${times.mkString(", ")}")
  }

  def time(f: (BDD, BDD | Map[BDDVar, BDD], BDD) => List[BDD]): Unit = {
    val runs = 5
    var sum: Long = 0
    for (_ <- 1 to runs) {
      val (v, e, c) = G1()
      val start = System.currentTimeMillis()
      f(v, e, c)
      val end = System.currentTimeMillis()
      sum += (end - start)
    }
    println(s"$f time = ${sum/runs}")
  }

  def line(nodes: List[BDD], colors: BDD): BDD = {
    var E = FALSE
    for (i <- nodes.init.indices) {
      E = E \/ (nodes(i) /\ colors /\ getTo(nodes(i + 1)))
    }
    E
  }

  def cycle(nodes: List[BDD], colors: BDD): BDD = {
    line(nodes, colors) \/ (nodes.last /\ colors /\ getTo(nodes.head))
  }

  def cross(V: BDD, E: BDD, f: (List[BDDVar], BDD) => BDD, n: Int, colors: BDD): (BDD, BDD) = {
    var V$ = V
    var E$ = E
    var vs = Map[BDDVar, List[BDDVar]]()
    for (i <- 1 until n) {
      var E$$ = E
      var V$$ = V
      while (V$$ != FALSE) {
        val v = pick(V$$, fromVars)
        V$$ = V$$ \ v
        val v$ = encodeNode(Integer.parseInt(getBDDBinaryName(v, fromVars), 2) + i * V.satCount(fromVars.reduce(and)).toInt, fromVars)
        V$ = V$ \/ v$
        vs = vs + (v -> (vs.getOrElse(v, List()) :+ v$))
        var Ev = (E$$ /\ v) \/ (E$$ /\ getTo(v))
        var Ev$ = FALSE
        while (Ev != FALSE) {
          val e = pick(Ev, fromVars ++ colorVars ++ toVars)
          val from = exist(e, (colorVars ++ toVars).reduce(and))
          val color = exist(e, (fromVars ++ toVars).reduce(and))
          val to = exist(e, (fromVars ++ colorVars).reduce(and))

          if (from == v) Ev$ = Ev$ \/ (v$ /\ color /\ to)
          if (to == getTo(v)) Ev$ = Ev$ \/ (from /\ color /\ getTo(v$))
          Ev = Ev \ e
        }
        E$$ = (E$$ \ v \ getTo(v)) \/ Ev$
      }
      E$ = E$ \/ E$$
    }
    for ((v, v$s) <- vs) {
      E$ = E$ \/ f(v :: v$s, colors)
    }
    (V$, E$)
  }
}
