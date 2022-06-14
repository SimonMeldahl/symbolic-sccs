package symbolic.noncolored

import symbolic.*
import symbolic.BDD.{pre as _, post as _, _}
import symbolic.Main.{post, pre}

object LockStep extends ((BDD, BDD | Map[BDDVar, BDD]) => List[BDD]) {
  override def toString(): String = "LockStep"

  override def apply(V: BDD, E: BDD | Map[BDDVar, BDD]): List[BDD] = decomposition(V)(using E)

  private def decomposition(W: BDD)(using E: BDD | Map[BDDVar, BDD]): List[BDD] =
    if (W == FALSE) return List()
    var F, B, Fopen, Bopen = pick(W, fromVars)
    while (Fopen != FALSE && Bopen != FALSE) {
      Fopen = update(Fopen, (post(Fopen) /\ W) \ F)
      Bopen = update(Bopen, (pre(Bopen) /\ W) \ B)
      F \/= copy(Fopen)
      B \/= copy(Bopen)
    }
    val Con = if (Fopen == FALSE) F else B
    while ((Fopen /\ B) != FALSE || (Bopen /\ F) != FALSE) {
      Fopen = update(Fopen, (post(Fopen) /\ W) \ F)
      Bopen = update(Bopen, (pre(Bopen) /\ W) \ B)
      F \/= copy(Fopen)
      B \/= copy(Bopen)
    }
    val scc = F /\ B
    // output scc

    val WCon = W \ Con
    val ConScc = Con \ scc

    if (WCon.satCount(fromVars.reduce(and)) <= ConScc.satCount(fromVars.reduce(and)))
      scc :: decomposition(WCon) ++ decomposition(ConScc)
    else
      scc :: decomposition(ConScc) ++ decomposition(WCon)
}
