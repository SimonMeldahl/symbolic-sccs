package symbolic.noncolored

import symbolic.*
import symbolic.BDD.{post as _, pre as _, *}
import symbolic.Main.{pre, post}

object DFS extends ((BDD, BDD | Map[BDDVar, BDD]) => List[BDD]) {
  override def toString(): String = "DFS"

  /**
   * DFS algorithm implementation.
   * We represent triples (x1, x2, x3) using encodings in fromVars, toVars and indexVars, respectively.
   */
  override def apply(V: BDD, E: BDD | Map[BDDVar, BDD]): List[BDD] = dfs(V)(using E)

  def push(S: BDD, t: BDD, X: BDD): (BDD, BDD) = {
    val v = pick(X, fromVars)
    var t$ = compose(t, indexVars, toVars)
    if (S == FALSE) t$ = compose(v, indexVars, fromVars)
    val (resS, resV) = (S \/ (X /\ compose(v, toVars, fromVars) /\ t$), compose(v, toVars, fromVars))
    (resS, resV)
  }

  def pop(S: BDD, t: BDD, X: BDD): (BDD, BDD) = {
    val p = exist(S /\ t, (fromVars ++ toVars).reduce(and))
    val S$ = S \ (X /\ t /\ p)
    val (resS, resV) = if ((S$ /\ (t /\ p)) == FALSE) (S$, compose(p, toVars, indexVars)) else (S$, t)
    (resS, resV)
  }

  def top(S: BDD, t: BDD): BDD = {
    val T = exist(S /\ t, (toVars ++ indexVars).reduce(and))
    val t$ = compose(t, fromVars, toVars)
    val res = if (t$ /\ T != FALSE) t$ else T
    res
  }

  def clean(S$: BDD, t$: BDD, W: BDD, A$: BDD, a$: BDD): (BDD, BDD, BDD, BDD) = {
    var S = S$
    var t = t$
    var A = A$
    var a = a$
    while (S != FALSE) {
      val T = top(S, t) /\ W \ exist(A, (toVars ++ indexVars).reduce(and))
      if (T != FALSE) {
        val v = pick(T, fromVars)
        {
          val Aa = push(A, a, v); A = Aa._1; a = Aa._2
        }
        {
          val St = pop(S, t, v); S = St._1; t = St._2
        }
      } else if (top(S, t) \ W == FALSE) {
        {
          val St = pop(S, t, top(S, t)); S = St._1; t = St._2
        }
      } else {
        return (S, t, A, a)
      }
    }
    (S, t, A, a)
  }

  def dfs(V: BDD)(using E: BDD | Map[BDDVar, BDD]): List[BDD] = {
    var sccs = List[BDD]()
    var (fS, ft) = (FALSE, FALSE)
    var W = FALSE
    while (W != V) {
      var v = pick(V \ W, fromVars)
      var (S@_, t) = push(FALSE, compose(v, toVars, fromVars), v)
      while (S != FALSE) {
        v = pick(top(S, t) \ W, fromVars)
        W = W \/ v
        val T = post(v) \ W
        if (T != FALSE) {
          val St = push(S, t, T); S = St._1; t = St._2
        }
        {
          val St = clean(S, t, W, fS, ft); S = St._1; t = St._2; fS = St._3; ft = St._4
        }
      }
    }

    W = FALSE
    while (W != V) {
      var scc = FALSE
      var topfSt = top(fS, ft)
      while (topfSt \ W == FALSE) {
        {
          val fSt = pop(fS, ft, topfSt); fS = fSt._1; ft = fSt._2
        }
        topfSt = top(fS, ft)
      }
      var v = pick(topfSt \ W, fromVars)
      var (S@_, t) = push(FALSE, compose(v, toVars, fromVars), v)
      while (S != FALSE) {
        v = pick(top(S, t) \ W, fromVars)
        scc = scc \/ v
        W = W \/ v
        val T = pre(v) \ W
        if (T != FALSE) {
          val St = push(S, t, T); S = St._1; t = St._2
        }
        {
          val St = clean(S, t, W, TRUE, TRUE); S = St._1; t = St._2
        }
      }
      sccs = scc :: sccs
    }

    sccs
  }
}
