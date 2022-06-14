package symbolic.noncolored

import symbolic.*
import symbolic.BDD.{post as _, pre as _, *}
import symbolic.Main.{post, pre}

object Simple extends ((BDD, BDD | Map[BDDVar, BDD]) => List[BDD]) {
  override def toString(): String = "Simple"

  override def apply(V: BDD, E: BDD | Map[BDDVar, BDD]): List[BDD] = sccs(V)(using E)

  private def sccs(S: BDD)(using E: BDD | Map[BDDVar, BDD]): List[BDD] = {
    if (S == FALSE) return List()
    val v = pick(S, fromVars)

    var fw, fw$ = copy(v)
    while ((fw \/= post(fw)) != fw$) fw$ = update(fw$, copy(fw))

    var bw, bw$ = copy(v)
    while ((bw \/= pre(bw)) != bw$) bw$ = update(bw$, copy(bw))

    val scc = bw /\ fw
    // output scc

    val Sfw = S \ fw
    val SfwE = restrictEdges(E, Sfw)
    val fwbw = fw \ bw
    val fwbwE = restrictEdges(E, fwbw)
    if (Sfw.satCount(fromVars.reduce(and)) <= fwbw.satCount(fromVars.reduce(and)))
      scc :: sccs(Sfw)(using SfwE) ++ sccs(fwbw)(using fwbwE)
    else
      scc :: sccs(fwbw)(using fwbwE) ++ sccs(Sfw)(using SfwE)
  }
}
