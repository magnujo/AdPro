package adpro

import Digit._
import Reduce.reduce

sealed trait FingerTree[+A] {

  // Exercise 14 (first part)

  def ▷ [B >: A] (b: B): FingerTree[B] = ???



  // Delegations for convenience

  def addL[B >: A] (b: B): FingerTree[B] =
    FingerTree.addL (b, this)

  def addR[B >: A] (b: B): FingerTree[B] =
    FingerTree.addR (this, b)

  def toList: List[A] =
    FingerTree.reduceTree.toList (this)

  def headL: A =
    FingerTree.headL (this)

  def tailL: FingerTree[A] =
    FingerTree.tailL (this)

  def headR: A =
    FingerTree.headR (this)

  def tailR: FingerTree[A] =
    FingerTree.tailR (this)

  // A quick size

  def size: Int = this.toList.size

  // We implement empty/nonEmpty without using views or patterns, thanks to
  // dynamic dispatch in Scala (match simpler, IMHO).  Haskell has no dynamic
  // dispatch, because it has no inheritance.

  def empty = false
  def nonEmpty = true
}

case class Empty () extends FingerTree[Nothing] {

  override def empty = true
  override def nonEmpty = false
}

case class Single[A] (data: A) extends FingerTree[A]

// pr - prefix, m - middle, sf - suffix
case class Deep[A] (

  pr: Digit[A],
  m: FingerTree[Node[A]],
  sf: Digit[A]

) extends FingerTree[A]



object FingerTree {

  // Exercise 11

  implicit def reduceTree: Reduce[FingerTree] = ???



  // Exercise 14  (second part)

  implicit class LeftFingerTreeOps[A]  (a: A) {
    def ◁ [B >: A] (t: FingerTree[B]): FingerTree[B] = ???
  }



  // Exercise 12 (page 5)

  def addL[A] (a: A, t: FingerTree[A]): FingerTree[A] = ???

  def addR[A] (t: FingerTree[A], a: A): FingerTree[A] = ???



  // Exercise 15

  def toTree[F[_]: Reduce, A] (fa:  F[A]): FingerTree[A] = ???



  // To be implemented as part of Exercise 19 (page 6)

  def deepL[A] (pr: Digit[A], m: FingerTree[Node[A]], sf: Digit[A])
    : FingerTree[A] = ???

  // To be implemented as part of Exercise 19

  def deepR[A] (pr: Digit[A], m: FingerTree[Node[A]], sf: Digit[A])
    : FingerTree[A] = ???



  // Easy to use convenience wrappers around matchers

  def headL[A] (t: FingerTree[A]): A =
    t match {
      case ConsL(h,_) => h
    }

  def tailL[A] (t: FingerTree[A]): FingerTree[A] =
    t match {
      case ConsL(_,t) => t
    }

  def headR[A] (t: FingerTree[A]): A =
    t match {
      case ConsR(_,h) => h
    }

  def tailR[A] (t: FingerTree[A]): FingerTree[A] =
    t match {
      case ConsR(t,_) => t
    }

}


// Exercise 18

// In the paper views are generic in the type of tree used. Here I make them
// fixed for FingerTrees.

// sealed trait ViewL[+A]
// case object Nil extends ViewL[Nothing]
// case class ConsL[A] (hd: A, tl: FingerTree[A]) extends ViewL[A]




// Exercise 19 (page 6, using Scala extractors)

object Nil {
  def unapply[A] (t: FingerTree[A]): Boolean = ???
}

object ConsL {
  def unapply[A] (t: FingerTree[A]): Option[(A, FingerTree[A])] = ???
}

object ConsR {
  def unapply[A] (t: FingerTree[A]): Option[(FingerTree[A],A)] = ???
}

