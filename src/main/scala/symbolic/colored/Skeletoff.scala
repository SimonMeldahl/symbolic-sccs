package symbolic.colored

import symbolic.*
import symbolic.BDD.{post as _, pre as _, *}
import symbolic.Main.{post, pre}

object Skeletoff extends ((BDD, BDD | Map[BDDVar, BDD], BDD) => List[BDD]) {
  override def toString(): String = "Skeletoff"

  override def apply(V: BDD, E: BDD | Map[BDDVar, BDD], C: BDD): List[BDD] = skeletoff(V /\ C, FALSE)(using E)

  private def skeletoff(W: BDD, V: BDD)(using E: BDD | Map[BDDVar, BDD]): List[BDD] = {
    if (W == FALSE) return List()
    val v = pivots(V \/ (W \ colors(V)))
    val fw = FALSE
    var V$ = FALSE
    var l = copy(v)
    while (l != FALSE) {
      fw \/= copy(l)
      V$ = update(V$, l \/ (V$ \ colors(l)))
      l = update(l, post(l) \ fw)
    }
    var scc, scc$ = copy(v)
    while ((scc \/= (pre(scc) /\ fw)) != scc$) scc$ = update(scc$, copy(scc))
    // output scc

    val WfwW = W \ fw
    val WfwV = pivots(pre(scc) \ fw)
    val WfwE = restrictEdges(E, WfwW)
    val fwsccW = fw \ scc
    val fwsccV = pivots(V$ \ scc)
    val fwsccE = restrictEdges(E, fwsccW)
    freeBdds(W, E, V, fw, scc, scc$, v, l, V$)

    if (WfwW.satCount((fromVars ++ colorVars).reduce(and)) <= fwsccW.satCount((fromVars ++ colorVars).reduce(and)))
      skeletoff(WfwW, WfwV)(using WfwE)
      skeletoff(fwsccW, fwsccV)(using fwsccE)
    else
      skeletoff(fwsccW, fwsccV)(using fwsccE)
      skeletoff(WfwW, WfwV)(using WfwE)
  }
}
