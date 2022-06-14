package symbolic.noncolored

import symbolic.*
import symbolic.BDD.{post as _, pre as _, *}
import symbolic.Main.{post, pre}

object Heuristic extends ((BDD, BDD | Map[BDDVar, BDD]) => List[BDD]) {
  override def toString(): String = "Heuristic"

  override def apply(V: BDD, E: BDD | Map[BDDVar, BDD]): List[BDD] = heuristic(V, FALSE)(using E)

  private def heuristic(W: BDD, v$: BDD)(using E: BDD | Map[BDDVar, BDD]): List[BDD] = {
    if (W == FALSE) return List()
    var v = v$
    if (v == FALSE) {
      v = pick(W, fromVars)
    } else {
      v = pick(v, fromVars)
    }
    val fw = FALSE
    var l = v
    var lprev = FALSE
    while (l != FALSE) {
      fw \/= copy(l)
      lprev = l
      l = post(l) \ fw
    }
    var scc, scc$ = copy(v)
    while ((scc \/= (pre(scc) /\ fw)) != scc$) scc$ = update(scc$, copy(scc))
    // output scc

    val WfwW = W \ fw
    val Wfwv = pre(scc) \ fw
    val WfwE = restrictEdges(E, WfwW)
    val fwsccW = fw \ scc
    val fwsccv = lprev \ scc
    val fwsccE = restrictEdges(E, fwsccW)

    if (WfwW.satCount(fromVars.reduce(and)) <= fwsccW.satCount(fromVars.reduce(and)))
      scc :: heuristic(WfwW, Wfwv)(using WfwE) ++ heuristic(fwsccW, fwsccv)(using fwsccE)
    else
      scc :: heuristic(fwsccW, fwsccv)(using fwsccE) ++ heuristic(WfwW, Wfwv)(using WfwE)
  }
}
