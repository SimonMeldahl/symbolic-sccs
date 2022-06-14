package symbolic.noncolored

import symbolic.*
import symbolic.BDD.{post as _, pre as _, *}
import symbolic.Main.{post, pre}

object Skeletoff extends ((BDD, BDD | Map[BDDVar, BDD]) => List[BDD]) {
  override def toString(): String = "Skeletoff"

  override def apply(V: BDD, E: BDD | Map[BDDVar, BDD]): List[BDD] = skeletoff(V, FALSE)(using E)

  private def skeletoff(W: BDD, v$: BDD)(using E: BDD | Map[BDDVar, BDD]): List[BDD] = {
    if (W == FALSE) return List()
    var v = v$
    if (v == FALSE) {
      v = pick(W, fromVars)
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
    val Wfwv = pick(pre(scc) \ fw, fromVars)
    val WfwE = restrictEdges(E, WfwW)
    val fwsccW = fw \ scc
    val fwsccv = pick(lprev \ scc, fromVars)
    val fwsccE = restrictEdges(E, fwsccW)

    if (WfwW.satCount(fromVars.reduce(and)) <= fwsccW.satCount(fromVars.reduce(and)))
      scc :: skeletoff(WfwW, Wfwv)(using WfwE) ++ skeletoff(fwsccW, fwsccv)(using fwsccE)
    else
      scc :: skeletoff(fwsccW, fwsccv)(using fwsccE) ++ skeletoff(WfwW, Wfwv)(using WfwE)
  }
}
