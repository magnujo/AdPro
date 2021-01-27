// Advanced Programming
// Andrzej Wąsowski, IT University of Copenhagen
//
// meant to be compiled, for example: fsc Stream.scala

package adpro


sealed trait Stream[+A] {
  import Stream._


  def headOption: Option[A] =
    this match {
      case Empty => None
      case Cons(h,t) => Some(h())
    }

  def tail: Stream[A] = this match {
      case Empty => Empty
      case Cons(h,t) => t()
  }

  def foldRight[B] (z : =>B) (f: (A, =>B) => B): B = this match {
      case Empty => z
      case Cons (h,t) => f (h(), t().foldRight (z) (f))
      // Note 1. f can return without forcing the tail
      // Note 2. this is not tail recursive (stack-safe) It uses a lot of stack
      // if f requires to go deeply into the stream. So folds sometimes may be
      // less useful than in the strict case
    }

  // Note. foldLeft is eager; cannot be used to work with infinite streams. So
  // foldRight is more useful with streams (somewhat opposite to strict lists)
  def foldLeft[B] (z : =>B) (f :(A, =>B) =>B) :B = this match {
      case Empty => z
      case Cons (h,t) => t().foldLeft (f (h(),z)) (f)
      // Note 2. even if f does not force z, foldLeft will continue to recurse
    }

  def exists (p : A => Boolean) :Boolean = this match {
      case Empty => false
      case Cons (h,t) => p(h()) || t().exists (p)
      // Note 1. lazy; tail is never forced if satisfying element found this is
      // because || is non-strict
      // Note 2. this is also tail recursive (because of the special semantics
      // of ||)
    }


  // Exercise 2

  def toList: List[A] =
    this.foldRight(List[A]())((a, b) => a :: b)

  // Exercise 3

  def take (n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(x, xs) => if (n==0) Empty else cons[A](x(), xs().take(n-1))
  }


  def drop (n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(x, xs) => if (n==0) cons(x(), xs()) else xs().drop(n-1)
  }

  // Exercise 4

  def takeWhile (p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t() takeWhile p)
    case _ => Empty
  }

  //Exercise 5
  
  def forAll (p: A => Boolean): Boolean =
    this.foldRight(true)((a, b) => p(a))


  //Exercise 6
  
  def takeWhile2 (p: A => Boolean): Stream[A] =
    this.foldRight[Stream[A]](Empty)((a, b) => if (p(a)) Cons(()=>a, ()=> b) else b)
  //Exercise 7

  def headOption2: Option[A] =
    this.foldRight[Option[A]](None)((a, b) => Some(a))

  //Exercise 8 The types of these functions are omitted as they are a part of the exercises

  def map[B] (f: A => B ): Stream[B] =
    //foldRight[Stream[B]](Empty)((a ,b) => Cons(()=>f(a), ()=>b))
      foldRight[Stream[B]](Empty)((a ,b) => cons(f(a), b))

  def filter (p: A => Boolean): Stream[A] =
    foldRight[Stream[A]](Stream[A]())((a, b) => if (p(a)) cons(a, b) else b)


  def append[B>:A] (a1: => Stream[B]): Stream[B] = //B is a supertype of A
    this.foldRight(a1) ((x,xs) => cons(x, xs))

  def flatMap[B] (f: A => Stream[B]): Stream[B] = {
    this.foldRight(Stream[B]())((x,xs) => f(x).append(xs))
  }

  //Exercise 09
  //Put your answer here:
  // Because on a list it would filter the whole list before it outputtet the head


  //Exercise 10
  //Put your answer here:
  def fib: Stream[Int] = {
    def go(a: Int, b: Int): Stream[Int] = {
      cons(a, go(b, a+b))
    }
    go(0, 1)
  }

  // Exercise 13

  def map_[B](f: A => B) = unfold(0)((x: Int) => Some(f(this.drop(x).headOption.get), x+1))
  def take_(n: Int) = unfold(0)(x => Some(this.drop(x).headOption.get, x+1)).take(n)
  def takeWhile_(p: A => Boolean) = unfold(0)(x => {
    if (p(this.drop(x).headOption.get)) Some(this.drop(x).headOption.get, x+1)
    else Some(this.drop(x+1).headOption.get, x+2)
  })

  def zipWith_ = ???

}


case object Empty extends Stream[Nothing]
case class Cons[+A] (h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def empty[A]: Stream[A] = Empty

  def cons[A] (hd: => A, tl: => Stream[A]) :Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def apply[A] (as: A*) :Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
    // Note 1: ":_*" tells Scala to treat a list as multiple params
    // Note 2: pattern matching with :: does not seem to work with Seq, so we
    //         use a generic function API of Seq


  // Exercise 1

  def from (n: Int): Stream[Int] = cons(n, from(n+1))

  def to (n: Int): Stream[Int] = cons(n, to(n-1))



  val naturals: Stream[Int] = from(1)

  //Exercise 11

  def unfold [A, S] (z: S) (f: S => Option[(A, S)]): Stream[A] = {
      cons(f(z).get._1, unfold(f(z).get._2)(f))
  }

  // Exercise 12

  def fib2  = unfold((0, 1))(x => Some(x._1, (x._2, x._1+x._2)))
  def from2(n: Int) = unfold(n)(x => Some(n, n+1))

}

object Tester extends App {
  import Stream._
  //import List._

  val s = cons(1, cons(2, Empty))

 // print(s.take(2).toList)
 // print(naturals.take(10).drop(9).toList)
 // print(naturals.take(10000000).drop(41).take(10).toList)
 // print(naturals.takeWhile(x => x < 10000).drop(100).take(50).toList)
  //print(naturals.take(10).forAll(_ < 0))
  print(naturals.map_(_*2).drop(30).take(50).toList)
  //print(naturals.drop(42).filter( x => x%2 ==0).take(30).toList)
  //println(naturals.flatMap(to _).take(100).toList)
  //println(naturals.flatMap(x => from(x)).take(100).toList)
  //print(unfold(1)(x => Some((x, x+1))).take(10).toList)
  //print(fib2.take(10).toList)

}

