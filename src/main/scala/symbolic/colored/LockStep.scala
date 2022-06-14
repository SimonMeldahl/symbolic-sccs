package symbolic.colored

import symbolic.*
import symbolic.BDD.{post as _, pre as _, *}
import symbolic.Main.{post, pre}

object LockStep extends ((BDD, BDD | Map[BDDVar, BDD], BDD) => List[BDD]) {
  override def toString(): String = "LockStep"

  override def apply(V: BDD, E: BDD | Map[BDDVar, BDD], C: BDD): List[BDD] = decomposition(V /\ C)(using E)

  private def decomposition(W: BDD)(using E: BDD | Map[BDDVar, BDD]): List[BDD] =
    if (W == FALSE) return List()
    var pivot, F, B, Fopen, Bopen = pivots(W)
    val Fpaused, Bpaused, Flock, Block = FALSE
    val c = colors(W)
    while ((Flock \/ Block) != c) {
      Fopen = update(Fopen, (post(Fopen) /\ W) \ F)
      Bopen = update(Bopen, (pre(Bopen) /\ W) \ B)
      Flock \/= (c \ colors(Fopen) \ Block)
      Block \/= (c \ colors(Bopen) \ Flock)
      Fpaused \/= (Fopen /\ Block)
      Bpaused \/= (Bopen /\ Flock)
      Fopen \= copy(Block)
      Bopen \= copy(Flock)
      F \/= copy(Fopen)
      B \/= copy(Bopen)
    }
    val Con = (F /\ Flock) \/ (B /\ Block)
    Fopen = Fpaused /\ Con
    Bopen = Bpaused /\ Con
    while (Fopen != FALSE || Bopen != FALSE) {
      F \/= copy(Fopen)
      B \/= copy(Bopen)
      Fopen = update(Fopen, (post(Fopen) /\ Con) \ F)
      Bopen = update(Bopen, (pre(Bopen) /\ Con) \ B)
    }
    val scc = F /\ B
    // output scc

    val WCon = W \ Con
    val ConScc = Con \ scc
    freeBdds(F, B, Con, scc, pivot, W, Fopen, Bopen, Fpaused, Bpaused, Flock, Block, c)

    if (WCon.satCount((fromVars ++ colorVars).reduce(and)) <= ConScc.satCount((fromVars ++ colorVars).reduce(and)))
      decomposition(WCon)
      decomposition(ConScc)
    else
      decomposition(ConScc)
      decomposition(WCon)
}
