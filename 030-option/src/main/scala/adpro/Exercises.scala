// Advanced Programming, A. Wąsowski, IT University of Copenhagen
//
// Group number: _____
//
// AUTHOR1: __________
// TIME1: _____ <- how much time have you used on solving this exercise set
// (excluding reading the book, fetching pizza, and going out for a smoke)
//
// AUTHOR2: __________
// TIME2: _____ <- how much time have you used on solving this exercise set
// (excluding reading the book, fetching pizza, and going out for a smoke)
//
// You should work with the file by following the associated exercise sheet
// (available in PDF from the course website).
//
// The file shall always compile and run after you are done with each exercise
// (if you do them in order).  Please compile and test frequently. Of course,
// some tests will be failing until you finish. Only hand in a solution that
// compiles and where tests pass for all parts that you finished.  The tests
// will fail for unfnished parts.  Comment such out.

package adpro

// Exercise  1

trait OrderedPoint extends scala.math.Ordered[java.awt.Point] {

  this: java.awt.Point =>

  override def compare (that: java.awt.Point): Int =  {
    if(this.x < that.x) 1
    else if (this.x == that.x && this.y < that.y) 1
    else 0
  }

}

// Try the following (and similar) tests in the repl (sbt console):
// val p = new java.awt.Point(0,1) with OrderedPoint
// val q = new java.awt.Point(0,2) with OrderedPoint
// assert(p < q)

sealed trait Tree[+A]
case class Leaf[A] (value: A) extends Tree[A]
case class Branch[A] (left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  // Exercise 2

  def size[A] (t :Tree[A]): Int = t match {
    case Leaf(x) => 1
    case Branch(x, y) => 1 + size(x) + size(y)
  }



  // Exercise 3

  def maximum (t: Tree[Int]): Int = t match {
    case Leaf(x) => x
    case Branch(l, r) =>  maximum(l).max(maximum(r))
  }

  // Exercise 4

  def map[A,B] (t: Tree[A]) (f: A => B): Tree[B] = t match {
    case Leaf(x) => Leaf(f(x))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  // Exercise 5

  def fold[A,B] (t: Tree[A]) (f: (B,B) => B) (g: A => B): B = t match {
    case Leaf(x) => g(x)
    case Branch(l, r) => f(fold(l)(f)(g), fold(r)(f)(g))
  }

  def size1[A] (t: Tree[A]): Int =
    fold(t)((a: Int, b: Int) => 1 + a + b)(_ => 1)

  def maximum1 (t: Tree[Int]): Int =
    fold(t)((a: Int, b: Int) => a.max(b))(a => a)

  def map1[A,B] (t: Tree[A]) (f: A=>B): Tree[B] =
    fold[A, Tree[B]](t)((a, b) => Branch(a, b))(x => Leaf(f(x)))

}


sealed trait Option[+A] {

  // Exercise 6

  def map[B] (f: A=>B): Option[B] = this match {
    case None => None
    case Some(x) => Some(f(x))
  }

  /**
   * Ignore the arrow (=>) in default's type below for now.
   * It prevents the argument "default" from being evaluated until it is needed.
   * So it is not evaluated if we process a Some object (this is 'call-by-name' 
   * and we should talk about this soon). 
   */

  def getOrElse[B >: A] (default: => B): B = this match {
    case None => default
    case Some(x) => x
  }

  def flatMap[B] (f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(x) => f(x)
  }

  def filter (p: A => Boolean): Option[A] = this match {
    case None => None
    case Some(x) => if(p(x)) Some(x) else None
  }

}

case class Some[+A] (get: A) extends Option[A]
case object None extends Option[Nothing]

object ExercisesOption extends App{
  import Tree._

  val b = Branch(Leaf(1), Leaf(2))

  print( size1(b))
  // mean is implemented in Chapter 4 of the text book

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  // Exercise 7

  def variance (xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
  }


  // Exercise 8

  def map2[A,B,C] (ao: Option[A], bo: Option[B]) (f: (A,B) => C): Option[C] =
    ao.flatMap(x => bo.flatMap(y => Some(f(x, y))))

  // Exercise 9

  def sequence[A] (aos: List[Option[A]]): Option[List[A]] =
    aos.foldRight[Option[List[A]]](Some(List[A]()))((x, xs) => map2(x, xs)((x, y) => x::y))

  // Exercise 10
  def traverse[A,B] (as: List[A]) (f :A => Option[B]): Option[List[B]] =
    sequence(as.map(f))
}
