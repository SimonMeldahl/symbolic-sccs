package symbolic

import net.sf.javabdd.*

type BDD = net.sf.javabdd.BDD
type BDDVar = BDD

given Ordering[BDD] with {
  override def compare(x: BDD, y: BDD): Int = x.`var`().compare(y.`var`())
}

object BDD {
  private val initialNodeTableSize: Int = 1000000
  private val initialCacheSize: Int = 1000000
  private var maxVarNum: Int = 100
  private var currentVarNum: Int = 0
  private var bdd: BDDFactory = JFactory.init(initialNodeTableSize, initialCacheSize)
  var fromVars: List[BDDVar] = List()
  var colorVars: List[BDDVar] = List()
  var toVars: List[BDDVar] = List()
  var indexVars: List[BDDVar] = List()
  var pairing: BDDPairing = bdd.makePair()
  reset()

  def reset(): Unit = {
    fromVars = List()
    colorVars = List()
    toVars = List()
    indexVars = List()
    maxVarNum = 100
    currentVarNum = 0
    bdd.done()
    bdd = JFactory.init(initialNodeTableSize, initialCacheSize)
    pairing = bdd.makePair()
    bdd.setVarNum(maxVarNum)
    bdd.setCacheRatio(64)
    bdd.setMinFreeNodes(0.05)
    bdd.setIncreaseFactor(1)
  }

  def createVar(): BDDVar = {
    val v = bdd.ithVar(currentVarNum)
    currentVarNum += 1
    if (currentVarNum >= maxVarNum) {
      maxVarNum *= 2
      bdd.setVarNum(maxVarNum)
    }
    v
  }

  /**
   * Interleaves from and to vars
   */
  def interleaveFromToVars(): Unit = {
    val fs = fromVars
    val ts = toVars
    fromVars = List()
    toVars = List()
    for ((v, i) <- (fs ++ ts).zipWithIndex) {
      if (i % 2 == 0) fromVars = fromVars :+ v
      else toVars = toVars :+ v
    }
  }

  def TRUE: BDD = bdd.one()

  def FALSE: BDD = bdd.zero()

  def copy(b: BDD): BDD = b.id()

  def not(b: BDD): BDD = b.not()

  def and(b1: BDD, b2: BDD): BDD = b1.and(b2)

  /**
   * @return b1 /\ b2 but overrides b1 with result and invalidates b2
   */
  def andWith(b1: BDD, b2: BDD): BDD = b1.andWith(b2)

  def or(b1: BDD, b2: BDD): BDD = b1.or(b2)

  /**
   * @return b1 \/ b2 but overrides b1 with result and invalidates b2
   */
  def orWith(b1: BDD, b2: BDD): BDD = b1.orWith(b2)

  def xor(b1: BDD, b2: BDD): BDD = b1.xor(b2)

  def imp(b1: BDD, b2: BDD): BDD = b1.imp(b2)

  def biimp(b1: BDD, b2: BDD): BDD = b1.biimp(b2)

  def exist(b1: BDD, b2: BDD): BDD = b1.exist(b2)

  /**
   * @return efficient exist(b1 /\ b2, b3)
   */
  def relProd(b1: BDD, b2: BDD, b3: BDD): BDD = b1.relprod(b2, b3)

  /**
   * Replace variable [[v]] with bdd [[g]] in [[f]]
   *
   * @note [[JDDFactory]] not supported
   * @return f[g/v]
   */
  def compose(f: BDD, g: BDD, v: BDDVar): BDD = {
    pairing.reset()
    pairing.set(v.`var`(), g)
    f.veccompose(pairing)
  }

  /**
   * Replace each variable vi with bdd gi in [[f]]
   *
   * @note [[JDDFactory]] not supported
   * @param gs list of bdds [g1, ..., gn]
   * @param vs list of variables [v1, ..., vn]
   * @return f[g1/v1, ..., gn/vn]
   */
  def compose(f: BDD, gs: List[BDD], vs: List[BDDVar]): BDD = {
    pairing.reset()
    pairing.set(vs.map(_.`var`()).toArray, gs.toArray)
    f.veccompose(pairing)
  }

  /**
   * @return edges [[E]] restricted to being within [[V]]
   */
  def restrictEdges(E: BDD | Map[BDDVar, BDD], V: BDD): BDD | Map[BDDVar, BDD] = E match {
    case E@_: BDD => E /\ V /\ compose (V, toVars, fromVars)
    case E@_: Map[BDDVar, BDD] => E.map((v, e) => (v, e /\ V))
  }
  
  def freeBdds(BDDs: (BDD | Map[BDDVar, BDD])*): Unit = BDDs.foreach {
    case B@_: BDD => B.free()
    case B@_: Map[BDDVar, BDD] => B.foreach((_, e) => e.free())
  }

  def approxColorSatCount(S: BDD): Int = colors(S).satCount(colorVars.reduce(and)).toInt

  def pre(S: BDD, E: BDD): BDD = relProd(compose(S, toVars, fromVars), E, toVars.reduce(and))

  def post(S: BDD, E: BDD): BDD = compose(relProd(S, E, fromVars.reduce(and)), fromVars, toVars)

  /**
   * @return BDD (only) describing values of [[vars]] in a satisfying assignment for [[S]]
   */
  def pick(S: BDD, vars: List[BDDVar]): BDD = {
    exist(S.satOne(vars.reduceOption(and).getOrElse(TRUE), false), (fromVars ++ colorVars ++ toVars).filterNot(vars.contains(_)).reduceOption(and).getOrElse(FALSE))
  }

  /**
   * Choose exactly one vertex for each color in S. 
   * Prefers setting to false.
   *
   * @param S colored set of vertices, S ⊆ V × C
   * @return bdd ⊆ V × C describing colors for each vertex
   */
  def pivots(S: BDD): BDD = {
    var S$, res = copy(S)
    for (v <- fromVars) {
      res /\= (S$ \ compose(S$ /\ !v, !v, v))
      S$ = update(S$, exist(S$, v))
    }
    res
  }

  /**
   * 'Dummy' function to update [[S]] to [[newS]] and then immediately free old value [[S]] (as opposed to waiting for garbage collection)
   * Note: [[S]] may not be used after.
   */
  def update(S: BDD, newS: BDD): BDD = {
    S.free()
    newS
  }

  def colors(S: BDD): BDD = exist(S, (fromVars ++ toVars).reduce(and))

  def printDot(s: String, b: BDD): Unit = {
    println("------------------------------")
    println(s"$s:")
    b.printDot()
    println("------------------------------")
  }

  /**
   * BDD representation of node from decimal to corresponding binary vars.
   *
   * @param node node number
   * @param vars type of node (from, color, to)
   * @return encoded node
   */
  def encodeNode(node: Int, vars: List[BDDVar]): BDD = {
    val binaryString = (node.toBinaryString.length until vars.length).foldLeft(node.toBinaryString)((res, _) => "0" + res)

    binaryString.zipWithIndex.foldLeft(TRUE) { case (res, (e, i)) =>
      if (e.asDigit == 0) res /\ !vars(i)
      else res /\ vars(i)
    }
  }

  /**
   * Get the toVars encoding of a node encoded using fromVars
   */
  def getTo(v: BDD): BDD = compose(v, toVars, fromVars)

  def getBDDBinaryName(S: BDD, vars: List[BDD]): String = vars.foldLeft("") { case (acc, v) =>
    if (S \ v != S) acc + 1
    else acc + 0
  }

  def printSccDots(V: BDD, E: BDD, C: BDD, sccs: List[BDD]): Unit = {
    printDot("V", V)
    printDot("E", E)
    printDot("C", C)
    printDot("Colors", colors(V /\ C))

    var remainingColors = C
    while (remainingColors != FALSE)
      val c = pick(remainingColors, colorVars)
      remainingColors = remainingColors \ c
      val colorName = getBDDBinaryName(c, colorVars)

      for ((scc, i) <- sccs.zipWithIndex)
        val s = exist(scc /\ c, colorVars.reduce(and))
        if (s != FALSE) printDot(s"$colorName-scc$i", s)
  }

  def createSetList(S: BDD, vars: List[BDDVar]): List[Int] = {
    var l: List[Int] = List()
    var s = S
    while (s != FALSE) {
      val node = pick(s, vars)
      s = s \ node
      val nodeName = getBDDBinaryName(node, vars)
      l = Integer.parseInt(nodeName, 2) :: l
    }
    l.sorted
  }

  def createEdgeList(E: BDD): List[(Int, Int, Int)] = {
    var l: List[(Int, Int, Int)] = List()
    var e = E
    while (e != FALSE) {
      val edge = pick(e, fromVars ++ colorVars ++ toVars)
      e = e \ edge
      val edgeName = getBDDBinaryName(edge, fromVars ++ colorVars ++ toVars)
      val (from, colorTo) = edgeName.splitAt(fromVars.length)
      val (color, to) = colorTo.splitAt(colorVars.length)
      l = (Integer.parseInt(from, 2), Integer.parseInt(color, 2), Integer.parseInt(to, 2)) :: l
    }
    l.sortBy((f, c, t) => (c, f, t))
  }

  def printGraph(V: BDD, E: BDD, C: BDD, sccs: List[BDD] = List()): Unit = {
    if (V != FALSE) println(s"V = ${createSetList(V, fromVars).mkString("[", ", ", "]")}")
    if (C != FALSE) println(s"C = ${createSetList(C, colorVars).mkString("[", ", ", "]")}")
    if (E != FALSE) println(s"E = ${createEdgeList(E).mkString("[", ", ", "]")}")

    val consoleColors = Map(
      0 -> Console.BLUE,
      1 -> Console.YELLOW,
      2 -> Console.GREEN,
      3 -> Console.RED,
      4 -> Console.WHITE,
      5 -> Console.CYAN,
      6 -> Console.MAGENTA)

    var remainingColors = C
    while (remainingColors != FALSE)
      val c = pick(remainingColors, colorVars)
      remainingColors = remainingColors \ c
      val colorName = Integer.parseInt(getBDDBinaryName(c, colorVars), 2)

      for ((scc, i) <- sccs.zipWithIndex)
        val s = exist(scc /\ c, colorVars.reduce(and))
        if (s != FALSE) println(consoleColors.getOrElse(colorName, "") + s"$colorName-SCC$i = ${createSetList(s, fromVars)}")
      print(Console.RESET)
  }
  
  var sccCounts: scala.collection.mutable.ListBuffer[BDD] = scala.collection.mutable.ListBuffer.empty[BDD]

  /**
   * Initialize counting of sccs. Must be called before calling [[updateSccsCounts]]
   * @param C All colors of the problem input
   */
  def initSccCounts(C: BDD): Unit = sccCounts = scala.collection.mutable.ListBuffer.from[BDD](List(C))

  /**
   * Update the number of sccs for each color by adding [[scc]]
   * @param scc newly found scc to update counts with
   */
  def updateSccsCounts(scc: BDD): Unit = {
    var colors = exist(scc, fromVars.reduce(and))
    for (i <- sccCounts.indices.reverse) {
      val item = sccCounts(i)
      if (item != FALSE && colors != FALSE) {
        val moveUp = item /\ colors
        if (moveUp != FALSE) {
          colors = update(colors, colors \ moveUp)
          if (sccCounts.length <= i + 1) sccCounts += FALSE
          sccCounts.update(i + 1, sccCounts(i + 1) \/ moveUp)
        }
        sccCounts.update(i, item \ moveUp)
      }
    }
  }

  /**
   * Output the number of sccs for each color
   * e.g. '7 SCCs: 42' means 42 colors have 7 sccs 
   */
  def printSccsCounts(): Unit = {
    for (i <- sccCounts.indices) {
      val count = approxColorSatCount(sccCounts(i))
      if (count != 0) println(s"$i SCCs: $count")
    }
  }

  extension (x: BDD) {
    def unary_! : BDD = not(x)

    def /\(y: BDD): BDD = and(x, y)

    def /\=(y: BDD): BDD = andWith(x, y)

    def \/(y: BDD): BDD = or(x, y)

    def \/=(y: BDD): BDD = orWith(x, y)

    def ==>(y: BDD): BDD = imp(x, y)

    def <==>(y: BDD): BDD = biimp(x, y)

    def \(y: BDD): BDD = x /\ !y

    def \=(y: BDD): BDD = x /\= !y
  }
}
