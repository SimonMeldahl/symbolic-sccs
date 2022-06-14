package symbolic.colored

import symbolic.*
import symbolic.BDD.{post as _, pre as _, *}
import symbolic.Main.{post, pre}

object Skeleton extends ((BDD, BDD | Map[BDDVar, BDD], BDD) => List[BDD]) {
  override def toString(): String = "Skeleton"

  override def apply(V: BDD, E: BDD | Map[BDDVar, BDD], C: BDD): List[BDD] = sccFind(V /\ C, FALSE, FALSE)(using E)

  private def sccFind(W: BDD, S: BDD, v: BDD)(using E: BDD | Map[BDDVar, BDD]): List[BDD] = {
    if (W == FALSE) return List()
    v \/= pivots(W \ colors(v))
    val (fw, s$, v$) = forwardIndexed(v)
    var scc, scc$ = copy(v)
    while ((scc \/= (pre(scc) /\ fw)) != scc$) scc$ = update(scc$, copy(scc))
    // output scc

    val Wfw = W \ fw
    val WfwS = S \ scc
    val Wfwv = (pre(scc /\ S) \ scc) /\ S
    val WfwE = restrictEdges(E, Wfw)
    val fwscc = fw \ scc
    val fwsccS = s$ \ scc
    val fwsccv = v$ \ scc
    val fwsccE = restrictEdges(E, fwscc)
    freeBdds(E, W, S, s$, v, v$, fw, scc, scc$)

    if (Wfw.satCount((fromVars ++ colorVars).reduce(and)) <= fwscc.satCount((fromVars ++ colorVars).reduce(and)))
      sccFind(Wfw, WfwS, Wfwv)(using WfwE)
      sccFind(fwscc, fwsccS, fwsccv)(using fwsccE)
    else
      sccFind(fwscc, fwsccS, fwsccv)(using fwsccE)
      sccFind(Wfw, WfwS, Wfwv)(using WfwE)
  }

  private def forwardIndexed(v: BDD)(using E: BDD | Map[BDDVar, BDD]): (BDD, BDD, BDD) = {
    val fw, ifw = FALSE
    var level = v
    var i = 0
    while (level != FALSE) {
      fw \/= copy(level)
      ifw \/= (encodeNode(i, indexVars) /\ level)
      i += 1
      level = post(level) \ fw
    }
    val s$, v$ = FALSE
    while (i >= 1) {
      i -= 1
      level = update(level, relProd(ifw, encodeNode(i, indexVars), indexVars.reduce(and)))
        v$ \/= pivots (level \ colors(v$))
        s$ \/=(pivots(pre(s$) /\ level) \/ v$)
    }
    freeBdds(ifw, level)
    (fw, s$, v$)
  }

  private def forward(v: BDD)(using E: BDD | Map[BDDVar, BDD]): (BDD, BDD, BDD) = {
    val fw = FALSE
    var i = 0
    var levels = List(v)
    while (levels(i) != FALSE) {
      fw \/= copy(levels(i))
      i += 1
      levels = levels :+ post(levels(i - 1)) \ fw
    }
    val v$, s$ = FALSE
    var level = levels(i)
    while (i >= 1) {
      i -= 1
      level = update(level, levels(i))
        v$ \/= pivots (level \ colors(v$))
        s$ \/=(pivots(pre(s$) /\ level) \/ v$)
    }
    (fw, s$, v$)
  }
}
