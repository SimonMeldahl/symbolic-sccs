package symbolic.noncolored

import symbolic.*
import symbolic.BDD.*
import symbolic.Main.{e, gen, main}

object Test {
  val INF = 100000000
  def main(args: Array[String]): Unit = {
    testAll(Simple)
    testAll(LockStep)
    testAll(Skeleton)
    testAll(Skeletoff)
    testAll(Heuristic)
    testAll(DFS)
  }

  def testAll(sccAlg: (BDD, BDD) => List[BDD]): Unit = {
    val r = scala.util.Random
    for (c <- 1 to 1) for (n <- 1 to 25) for (m <- 1 to 25)
      val g = List.tabulate(n, n)((i, j) => {
        if (i == j) 0
        else if (r.nextInt(m) != 0) INF
        else 1 + r.nextInt((1 << c) - 1)
      })
      test(sccAlg, g, c)
  }

  def simple(g: List[List[Int]], c: Int): List[List[Int]] = {
    val n = g.length
    val d = Array.tabulate(n, n)((i, j) => {
      if (i == j) 0
      else if (g(i)(j) == INF || (g(i)(j) & (1 << c)) == 0) INF
      else 1
    })
    for (k <- 0 until n) for (i <- 0 until n) for (j <- 0 until n)
      d(i)(j) = math.min(d(i)(j), d(i)(k) + d(k)(j))
    (0 until n).map(i => (0 until n).filter(j => d(i)(j) < INF && d(j)(i) < INF).toList).distinct.toList
  }

  def test(sccAlg: (BDD, BDD) => List[BDD], g: List[List[Int]], colorCount: Int): Unit = {
    val n = g.length
    g.foreach(l => assert(l.length == n))
    assert(colorCount < 30)
    reset()

    val (v, _) = gen(n, colorCount)
    var E = FALSE
    for (i <- 0 until n) for (j <- 0 until n) if (g(i)(j) != INF)
      for (k <- 0 until colorCount) if ((g(i)(j) & (1 << k)) != 0)
        E = E \/ e(i, k, j)

    val scc = sccAlg(v, exist(E, colorVars.reduce(and)))
    for (k <- 0 until colorCount)
      val color = encodeNode(k, colorVars)
      val s1 = simple(g, k).sorted
      val s2 = scc.map(s => exist(s /\ color, colorVars.reduce(and))).map(createSetList(_, fromVars)).filterNot(_.isEmpty).sorted
      assert(s1 == s2, {
        print(Console.RED)
        println(s"$sccAlg failed")
        print(Console.RESET)
        println(s"G = $g")
        println(s"simple = $s1")
        println(s"$sccAlg = $s2")
        sys.exit()
      })
  }
}
