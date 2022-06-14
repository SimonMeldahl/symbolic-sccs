package symbolic.colored

import symbolic.*
import symbolic.BDD.{post as _, pre as _, *}
import symbolic.Main.{post, pre}

object Simple extends ((BDD, BDD | Map[BDDVar, BDD], BDD) => List[BDD]) {
  override def toString(): String = "Simple"

  override def apply(V: BDD, E: BDD | Map[BDDVar, BDD], C: BDD): List[BDD] = sccs(V /\ C)(using E)

  private def sccs(S: BDD)(using E: BDD | Map[BDDVar, BDD]): List[BDD] = {
    if (S == FALSE) return List()
    val v = pivots(S)

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
    freeBdds(S, E, fw, fw$, bw, bw$, v, scc)
    if (Sfw.satCount((fromVars ++ colorVars).reduce(and)) <= fwbw.satCount((fromVars ++ colorVars).reduce(and)))
      sccs(Sfw)(using SfwE)
      sccs(fwbw)(using fwbwE)
    else
      sccs(fwbw)(using fwbwE)
      sccs(Sfw)(using SfwE)
  }
}
