package symbolic

import symbolic.BDD.*
import symbolic.parsing.AeonParser

object BooleanNetwork {
  val g2a =
    """
    #position:CtrA:419,94
    #$CtrA:((((!CtrA & GcrA) & !CcrM) & !SciP) | ((CtrA & !CcrM) & !SciP))
    CtrA -> CtrA
    GcrA -> CtrA
    CcrM -| CtrA
    SciP -| CtrA
    #position:GcrA:325,135
    #$GcrA:(!CtrA & DnaA)
    CtrA -| GcrA
    DnaA -> GcrA
    #position:CcrM:462,222
    #$CcrM:((CtrA & !CcrM) & !SciP)
    CtrA -> CcrM
    CcrM -| CcrM
    SciP -| CcrM
    #position:SciP:506,133
    #$SciP:(CtrA & !DnaA)
    CtrA -> SciP
    DnaA -| SciP
    #position:DnaA:374,224
    #$DnaA:(((CtrA & !GcrA) & !DnaA) & CcrM)
    CtrA -> DnaA
    GcrA -| DnaA
    DnaA -| DnaA
    CcrM -> DnaA
    """

  val tcr =
    """
    #name:TCR Signalisation
    #description:<div class="field-label" style="font-weight: bold; color: rgb(83, 83, 83); font-family: san$
    #position:CD45:11,31
    #$CD45:CD45
    CD45 -> CD45
    #position:CD8:78,31
    #$CD8:CD8
    CD8 -> CD8
    #position:TCRlig:195,29
    #$TCRlig:TCRlig
    TCRlig -> TCRlig
    #position:TCRbind:327.84534733938375,168.29187720003932
    #$TCRbind:(TCRlig & !cCbl)
    TCRlig -> TCRbind
    cCbl -| TCRbind
    #position:cCbl:335,301
    #$cCbl:ZAP70
    ZAP70 -> cCbl
    #position:PAGCsk:183.44116736182693,99.70410092583916
    #$PAGCsk:!TCRbind
    TCRbind -| PAGCsk
    #position:LCK:82,208
    #$LCK:((CD45 & CD8) & !PAGCsk)
    CD45 -> LCK
    CD8 -> LCK
    PAGCsk -| LCK
    #position:Fyn:196.18895879547205,181.14794953708042
    #$Fyn:(((CD45 & !TCRbind) & LCK) | (CD45 & TCRbind))
    CD45 -> Fyn
    TCRbind -> Fyn
    LCK -> Fyn
    #position:ZAP70:87,302
    #$ZAP70:((LCK & !cCbl) & TCRphos)
    LCK -> ZAP70
    cCbl -| ZAP70
    TCRphos -> ZAP70
    #position:TCRphos:203,261
    #$TCRphos:(((!TCRbind & Fyn) | ((TCRbind & !LCK) & Fyn)) | (TCRbind & LCK))
    TCRbind -> TCRphos
    LCK -> TCRphos
    Fyn -> TCRphos
    """

  val buddingOrlando =
    """
    #position:CLN3:128,68
    #$CLN3:(((!YOX1 & ACE2) & SWI5) | (((YOX1 & !YHP1) & ACE2) & SWI5))
    YOX1 -| CLN3
    YHP1 -| CLN3
    ACE2 -> CLN3
    SWI5 -> CLN3
    #position:MBF:219,96
    #$MBF:CLN3
    CLN3 -> MBF
    #position:SBF:281,138
    #$SBF:(((((!MBF & !YOX1) & CLN3) | (((!MBF & YOX1) & !YHP1) & CLN3)) | (MBF & !YOX1)) | ((MBF & YOX1) & !YHP1))
    MBF -> SBF
    YOX1 -| SBF
    YHP1 -| SBF
    CLN3 -> SBF
    #position:YOX1:297,175
    #$YOX1:(MBF & SBF)
    MBF -> YOX1
    SBF -> YOX1
    #position:YHP1:286,254
    #$YHP1:((!MBF & SBF) | MBF)
    MBF -> YHP1
    SBF -> YHP1
    #position:HCM1:305,217
    #$HCM1:(MBF & SBF)
    MBF -> HCM1
    SBF -> HCM1
    #position:SFF:186,302
    $SFF:(SBF & HCM1)
    SBF -> SFF
    HCM1 -> SFF
    #position:ACE2:74,276
    #$ACE2:SFF
    SFF -> ACE2
    #position:SWI5:47,207
    #$SWI5:SFF
    SFF -> SWI5
    """

  val buddingIron =
    """
    #position:Yhp1:463,95
    $Yhp1:SMBF
    SMBF -> Yhp1
    #position:Cln3:403,58
    $Cln3:!Yhp1
    Yhp1 -| Cln3
    #position:SMBF:342,141
    #$SMBF:(((((!Cln3 & !SMBF) & Cln2) & !Clb2) | ((!Cln3 & SMBF) & !Clb2)) | (Cln3 & !Clb2))
    Cln3 -> SMBF
    SMBF -> SMBF
    Cln2 -> SMBF
    Clb2 -| SMBF
    #position:Cln2:143,74
    $Cln2:SMBF
    SMBF -> Cln2
    #position:Clb2:225,269
    $Clb2:((((((((!Clb2 & !Cdc20) & !CKI) & B) | ((((!Clb2 & Cdc20) & !CKI) & !Cdh1) & B)) | ((((Clb2 & !SFF) & !Cdc20) & !CKI) & B)) | (((((Clb2 & !SFF) & Cdc20) & !CKI) & !Cdh1) & B)) | (((Clb2 & SFF) & !Cdc20) & !CKI)) | ((((Clb2 & SFF) & Cdc20) & !CKI) & !Cdh1))
    Clb2 -> Clb2
    SFF -> Clb2
    Cdc20 -| Clb2
    CKI -| Clb2
    Cdh1 -| Clb2
    B -> Clb2
    #position:Clb5:482,179
    #$Clb5:(SMBF & !Cdc20)
    SMBF -> Clb5
    Cdc20 -| Clb5
    #position:Cdc20:363,436
    #$Cdc20:((Clb2 & SFF) & M)
    Clb2 -> Cdc20
    SFF -> Cdc20
    M -> Cdc20
    #position:SFF:313,314
    $SFF:(((((((!Clb2 & !Cdc20) & !CKI) & B) | ((((!Clb2 & Cdc20) & !CKI) & !Cdh1) & B)) | ((((Clb2 & !SFF) & !Cdc20) & !CKI) & B)) | (((((Clb2 & !SFF) & Cdc20) & !CKI) & !Cdh1) & B)) | (Clb2 & SFF))
    Clb2 -> SFF
    SFF -> SFF
    Cdc20 -| SFF
    CKI -| SFF
    Cdh1 -| SFF
    B -> SFF
    #position:CKI:68,167
    $CKI:((((((((!Cln2 & !Clb5) & !Clb2) & !Swi5) & CKI) | (((!Cln2 & !Clb5) & !Clb2) & Swi5)) | ((((!Cln2 & !Clb5) & Clb2) & Cdc14) & Swi5)) | (((!Cln2 & Clb5) & Cdc14) & Swi5)) | ((Cln2 & Cdc14) & Swi5))
    Cln2 -| CKI
    Clb5 -| CKI
    Clb2 -| CKI
    Cdc14 -> CKI
    Swi5 -> CKI
    CKI -> CKI
    #position:Cdh1:412,235
    $Cdh1:(((((!Cln2 & !Clb5) & !Clb2) | (((!Cln2 & !Clb5) & Clb2) & Cdc14)) | ((!Cln2 & Clb5) & Cdc14)) | (Cln2 & Cdc14))
    Cln2 -| Cdh1
    Clb5 -| Cdh1
    Clb2 -| Cdh1
    Cdc14 -> Cdh1
    #position:B:256,37
    $B:(((((!Cln2 & !Clb5) & B) & !CD) | ((!Cln2 & Clb5) & !CD)) | (Cln2 & !CD))
    Cln2 -> B
    Clb5 -> B
    B -> B
    CD -| B
    #position:M:260,489
    $M:((((!Clb2 & M) & !CD) | (((Clb2 & !S) & M) & !CD)) | ((Clb2 & S) & !CD))
    Clb2 -> M
    S -> M
    M -> M
    CD -| M
    #position:FEAR:195,443
    $FEAR:Cdc20
    Cdc20 -> FEAR
    #position:MEN:120,418
    #$MEN:(Clb2 & FEAR)
    Clb2 -> MEN
    FEAR -> MEN
    #position:Cdc14:68,354
    #$Cdc14:(FEAR & MEN)
    FEAR -> Cdc14
    MEN -> Cdc14
    #position:Swi5:184,340
    #$Swi5:((!Clb2 & SFF) | ((Clb2 & SFF) & Cdc14))
    Clb2 -| Swi5
    SFF -> Swi5
    Cdc14 -> Swi5
    #position:S:554,219
    $S:(((((!Clb5 & !Clb2) & S) & !CD) | ((!Clb5 & Clb2) & !CD)) | (Clb5 & !CD))
    Clb5 -> S
    Clb2 -> S
    S -> S
    CD -| S
    #position:CD:6,240
    $CD:(((FEAR & Cdc14) & M) & !CD)
    FEAR -> CD
    Cdc14 -> CD
    M -> CD
    CD -| CD
    """

  val example =
    """
    z -> x
    y -> x
    x -| x
    $x: z | (!x & y)
    x -> y
    z -|? y
    $y: f(x, z)
    """

  def main(args: Array[String]): Unit = {
    printDetails(example)
    printDetails(g2a)
    printDetails(tcr)
    printDetails(buddingOrlando)
  }

  private def printDetails(aeon: String): Unit = {
    reset()
    val (v, e, _, c) = readAeon(aeon)
    println(s"|C| = ${c.satCount(colorVars.reduce(and)).toLong}")
    println(s"|V| = ${v.satCount(fromVars.reduce(and)).toLong}")
    println(s"|E| = ${(e /\ c).satCount((fromVars ++ toVars ++ colorVars).reduce(and)).toLong}")
  }

  /**
   * Convert aeon specification of a parametrized boolean network into a fully specified graph G=(V,E,C)
   * For each parametrisation, compute each possible valuation as a color
   *
   * Initializes fromVars, colorVars, toVars of BDD object
   * fromVars, toVars are created in order of appearance in the aeon string
   *
   * NOTE: Regulations must contain spaces between arrows and operators (except '!', '(', ')' )
   *
   * NOTE: E output does not take into account the valid colors C. This is ok for our algorithms, 
   * as we always restrict the edges/result. This gives a smaller BDD. 
   * However, to use E independently use E /\ C to only get edges for the valid colors 
   *
   * @param aeon Parametrized boolean network in aeon format
   * @return BDDs for V, E, edgeFunctions, C
   */
  def readAeon(aeon: String): (BDD, BDD, Map[BDDVar, BDD], BDD) = {
    val (vars, regulations, updateFunctions) = parseLines(aeon)

    var varsToBdd: Map[String, BDDVar] = vars.map(v => v -> createVar()).toMap

    given funsToBdds: Map[String, List[BDDVar]] = makeFunctionTruthVars(updateFunctions)(using varsToBdd)

    for ((_, v) <- varsToBdd) fromVars = fromVars :+ v
    // vars need to be in sorted BDD order for BDD operations to work properly
    fromVars = fromVars.sorted

    for (_ <- fromVars) toVars = toVars :+ createVar()

    // interleave form and to vars, and redefine varsToBdd with this change in mind
    interleaveFromToVars()
    varsToBdd = varsToBdd.zipWithIndex.map{case ((s, _), i) => s -> fromVars(i)}.toMap
    given Map[String, BDDVar] = varsToBdd

    for ((_, bddVars) <- funsToBdds) colorVars = colorVars ++ bddVars
    // we always have at least 1 color
    if (colorVars.isEmpty) colorVars = createVar() :: colorVars
    // vars need to be in sorted BDD order for BDD operations to work properly
    colorVars = colorVars.sorted

    for (_ <- fromVars) indexVars = indexVars :+ createVar()

    var validColors = TRUE
    for ((u$, reg, v$) <- regulations) {
      val u = varsToBdd(u$)
      val f = makeFunctionTrue(updateFunctions(v$))

      val observability = if (!reg.contains('?')) exist(xor(relProd(f, u, u), relProd(f, !u, u)), fromVars.reduce(and)) else TRUE
      if (observability == FALSE) throw RuntimeException(s"No update functions satisfying observability for ${v$}. Regulation ${u$} $reg ${v$} violated")

      val monotonicity = if (reg.contains('>')) !relProd(relProd(!f, u, u), relProd(f, !u, u), fromVars.reduce(and))
      else if (reg.contains('|')) !relProd(relProd(!f, !u, u), relProd(f, u, u), fromVars.reduce(and))
      else throw RuntimeException(s"No monotonicity specified for regulation ${u$} $reg ${v$}")
      if (monotonicity == FALSE) throw RuntimeException(s"No update functions satisfying monotonicity for ${v$}. Regulation ${u$} $reg ${v$} violated")

      validColors /\= (observability /\= monotonicity)
    }
    if (validColors == FALSE) throw RuntimeException(s"No update functions satisfy given constraints")
    // then colorVars = [c], and only allow !c as we only have 1 color
    if (validColors == TRUE) validColors = validColors /\ !colorVars.head


    val completeUpdateFunctions = updateFunctions.map { case (v$, f$) =>
      val v = varsToBdd(v$)
      val f = makeFunctionTrue(f$)

      def isFunVarUsed(v: BDDVar): Boolean =
        uninterpretedFunctionCalls(f$).foldLeft(Set[BDD]())((acc, f) => acc ++ funsToBdds(f.substring(0, f.indexWhere(_ == '(')))).contains(v)

      val funVarsNotUsed = colorVars.filterNot(v => isFunVarUsed(v))

      if (funVarsNotUsed.isEmpty) v -> f /\ validColors
      else v -> f /\ exist(validColors, funVarsNotUsed.reduce(and))
    }

    //    val colorCounts = calculateSatisfyingColorCounts(completeUpdateFunctions)
    //    for ((v, c) <- colorCounts) println(s"Number of valid F_${varsToBdd.find((_, v$) => v$ == v).getOrElse(("?", FALSE))._1} = $c")
    //    println(s"Number of valid colors = ${colorCounts.values.filterNot(_ == 0).product}")


    // We have all possible combinations of fromVars
    val V = TRUE

    var E = FALSE
    for ((fromVar, f) <- completeUpdateFunctions) {
      val toVar = toVars(fromVars.indexOf(fromVar))
      val asyncCondition = TRUE
      for ((f, t) <- fromVars.zip(toVars) if (f, t) != (fromVar, toVar)) {
        asyncCondition /\= (f <==> t)
      }
      E \/= (!(fromVar <==> f) /\ !(fromVar <==> toVar) /\ asyncCondition)
    }
    // then colorVars = [c], and only allow !c as we only have 1 color
    if (validColors == TRUE) E = E /\ !colorVars.head

    val edgeFunctions = completeUpdateFunctions.map((v, f) => (v, !(f <==> v)))

    val C = validColors

    //    val start = System.currentTimeMillis()
    //    val sccs = Heuristic(V, edgeFunctions, C)
    //    val end = System.currentTimeMillis()
    //    printSccsCount(C, sccs)
    //    println(s"Found SCCs (${sccs.length} different BDDs) in ${end - start} ms.")
    //    println(s"|E| = ${E.satCount((fromVars ++ toVars ++ colorVars).reduce(and)).toInt} (${E.nodeCount()} BDD nodes), |C| = ${approxColorSatCount(C)}")
    (V, E, edgeFunctions, C)
  }

  /**
   * Symbolic BDD post operation of S based on edge functions rather than complete edge relation
   * Edge functions is instead of (v, f) from update functions we have (v, !(f <==> v))
   *
   * @param S             BDD to do post operation for
   * @param edgeFunctions edge functions for the BN
   * @return BDD of nodes reachable in one post operation
   */
  def post(S: BDD, edgeFunctions: Map[BDDVar, BDD]): BDD = {
    val acc = FALSE
    edgeFunctions.foreach((v, e) => acc \/= compose(S /\ e, !v, v))
    acc
  }

  /**
   * Symbolic BDD pre operation of S based on edge functions rather than complete edge relation
   * Edge functions is instead of (v, f) from update functions we have (v, !(f <==> v))
   *
   * @param S             BDD to do pre operation for
   * @param edgeFunctions edge functions for the BN
   * @return BDD of nodes reachable in one pre operation
   */
  def pre(S: BDD, edgeFunctions: Map[BDDVar, BDD]): BDD = {
    val acc = FALSE
    edgeFunctions.foreach((v, e) => acc \/= (compose(S, !v, v) /\ e))
    acc
  }

  /**
   * Calculate the number of satisfying assignment for each update function's BDD
   *
   * @param completeUpdateFunctions mapping for each BN var to its update function
   * @return mapping for each BN var to the number of satisfying assignments for its update function
   */
  def calculateSatisfyingColorCounts(completeUpdateFunctions: Map[BDDVar, BDD]): Map[BDDVar, Int] = {
    completeUpdateFunctions.foldLeft(Map[BDDVar, Int]()) { case (acc, (v, f)) =>
      var count = 0
      var remaining = f
      val varsToPickFrom = colorVars.filter(exist(f, _) != f)

      // If only one var to pick from, then there are 2 possibilities; true or false
      if (varsToPickFrom.filterNot(_ == FALSE).length == 1) count = 2
      else while (remaining != FALSE) {
        remaining = remaining \ pick(remaining, varsToPickFrom)
        count += 1
      }
      acc + (v -> count)
    }
  }

  /**
   * Make BDDVar corresponding to the possible outcomes in the truth table for each uninterpreted
   * function appearing in updateFunctions
   *
   * @param varsToBdd       vars in the BN
   * @param updateFunctions update functions in the BN
   * @return mapping from each uninterpreted function to its BDDVars for the truth table
   */
  def makeFunctionTruthVars(updateFunctions: Map[String, String])(using varsToBdd: Map[String, BDDVar]): Map[String, List[BDDVar]] = {
    updateFunctions.foldLeft(Map[String, List[BDDVar]]()) { case (acc, (_, f)) =>
      uninterpretedFunctionCalls(f).foldLeft(acc) { case (acc, f) =>
        val funName = f.substring(0, f.indexWhere(_ == '('))
        val argsCount = if (f.indexWhere(_ == '(') + 1 == f.indexWhere(_ == ')')) 0 else f.count(c => c == ',') + 1
        val bddVars = (0 until math.pow(2, argsCount).toInt).map(i => createVar()).toList
        if (!acc.contains(funName)) acc + (funName -> bddVars)
        else if (bddVars.length != acc(funName).length) throw RuntimeException(s"$funName used with incompatible arity")
        else acc
      }
    }
  }

  /**
   * @param i      number to convert
   * @param length length of binary string
   * @return binary representation of 'i', possibly padded with 0s
   */
  def binaryString(i: Int, length: Int): String =
    (i.toBinaryString.length until length).foldLeft(i.toBinaryString)((acc, _) => "0" + acc)

  /**
   * Find uninterpreted function calls of update function f with regards to BN variables vars
   *
   * @param f         update function
   * @param varsToBdd BN variables
   * @return List of uninterpreted function calls
   */
  def uninterpretedFunctionCalls(f: String)(using varsToBdd: Map[String, BDDVar]): List[String] = {
    val terms = f.split("[|&^]|=>|<=>")
    terms.filterNot(w => varsToBdd.keySet.contains(w.filter(isIdentifier)) || w.filter(isIdentifier) == "false" || w.filter(isIdentifier) == "true").map(w => w.substring(w.indexWhere(isIdentifier), w.indexWhere(_ == ')') + 1)).toList
  }

  /**
   * Make BDD that is true when update function f is true.
   *
   * If f contains uninterpreted functions, these will be 'eta' expanded.
   * That is, f(x) will be replaced with '(x /\ f(1)) \/ (!x /\ f(0))'
   * where f(1), f(0) are the corresponding BDDVars in the truth table
   *
   * @param f          update function
   * @param varsToBdd  mapping from variables of the BN to their BDDVar
   * @param funsToBdds mapping from uninterpreted functions to their 'truthtable BDDVars'
   * @return BDD which has a satisfying assignment for each input (vars and funs) making f true
   */
  def makeFunctionTrue(f: String)(using varsToBdd: Map[String, BDDVar], funsToBdds: Map[String, List[BDDVar]]): BDD = {
    AeonParser(f)
  }

  /**
   * Expand function call f(x) to '(x /\ f(1)) \/ (!x /\ f(0))'
   * where f(1), f(0) are the corresponding BDDVars in the truth table
   *
   * @param f          function call
   * @param varsToBdd  mapping from BN vars to corresponding BDDVar
   * @param funsToBdds mapping from uninterpreted functions to their 'truthtable BDDVars'
   * @return BDD of expanded function call
   */
  def expandFunctionCall(f: String)(using varsToBdd: Map[String, BDDVar], funsToBdds: Map[String, List[BDDVar]]): BDD = {
    val funName = f.substring(0, f.indexWhere(_ == '('))
    val args = f.substring(f.indexWhere(_ == '(') + 1, f.indexWhere(_ == ')')).split(',').map(_.trim).filterNot(_ == "").toList

    def encode(i: Int): BDD = {
      binaryString(i, args.length).zipWithIndex.foldLeft(TRUE) { case (res, (e, i)) =>
        if (e.asDigit == 0) res /\ !varsToBdd(args(i))
        else res /\ varsToBdd(args(i))
      } /\ funsToBdds(funName)(i)
    }

    if (args.isEmpty) funsToBdds(funName).head
    else (0 until math.pow(2, args.length).toInt).foldLeft(FALSE) { (acc, i) => acc \/ encode(i) }
  }

  /**
   * @param c char to check
   * @return if char 'c' can be part of an identifier name
   */
  def isIdentifier(c: Char): Boolean = ('A' <= c && c <= 'z') || ('0' <= c && c <= '9')

  /**
   * Read aeon and find vars, regulations and update functions
   *
   * Ensure that update functions only use regulating variables as arguments
   *
   * @param aeon String describing a BN in aeon format
   * @return (vars, regulations, updateFunctions)
   */
  def parseLines(aeon: String): (List[String], List[(String, String, String)], Map[String, String]) = {
    val lines = aeon.split('\n').map(_.trim).filterNot(_.startsWith("#")).filterNot(_.trim == "")

    val regulations: Array[(String, String, String)] = lines.filterNot(_.startsWith("$")).map(l =>
      val (regulator, temp) = l.splitAt(l.indexOf(' '))
      val (reg, target) = temp.trim.splitAt(temp.trim.indexOf(' '))
      (regulator, reg, target.trim)
    )

    val vars = regulations.foldLeft(List[String]()) { case (l, (regulator, _, target)) =>
      val l$ = if (!l.contains(regulator)) l :+ regulator else l
      if (!l$.contains(target)) l$ :+ target else l$
    }

    val varsWithUpdateFunctions = lines.filter(_.startsWith("$")).map(_.substring(1))
    val varsWithoutUpdateFunctions = vars.toSet -- varsWithUpdateFunctions.map(l => l.substring(0, l.indexWhere(_ == ':')).trim)
    val updateFunctions = varsWithUpdateFunctions.map { l =>
      val (v, f) = l.splitAt(l.indexOf(':'))
      v -> f.substring(1).trim
    }.toMap ++ varsWithoutUpdateFunctions.map { v =>
      val regs = regulations.filter((_, _, target) => target == v).map((regulator, _, _) => regulator)
      v -> s"f_$v${regs.mkString("(", ", ", ")")}"
    }

    for ((v, f) <- updateFunctions) {

      val regulatingVars = f.split("[|&!,()^]|=>|<=>").filter(s => vars.contains(s.filter(isIdentifier))).map(_.trim)
      for (u <- regulatingVars) {
        if (!regulations.exists((r, _, t) => r == u && t == v)) throw RuntimeException(s"Update function F_$v = $f invalid. $u does not regulate $v")
      }

      val calls = uninterpretedFunctionCalls(f)(using vars.map(v => v -> FALSE).toMap)
      for (c <- calls) {
        val argVars = c.substring(c.indexWhere(_ == '(') + 1, c.indexWhere(_ == ')')).split(',').map(_.trim).filterNot(_.isEmpty)
        for (u <- argVars) {
          if (!regulations.exists((r, _, t) => r == u && t == v)) throw RuntimeException(s"Update function F_$v = $f invalid. $u does not regulate $v")
        }
      }

    }

    (vars, regulations.toList, updateFunctions)
  }
}
