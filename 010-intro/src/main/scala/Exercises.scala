// Advanced Programming, Exercises by A. Wąsowski, IT University of Copenhagen
//
// Work on this file by following the associated exercise sheet
// (available in PDF in the same directory).
//
// The file is meant to be compiled inside sbt, using the 'compile' command.
// To run the compiled file use the 'run' or 'runMain'.
// To load the file int the REPL use the 'console' command.
//
// Continue solving exercises in the order presented in the PDF file. The file
// shall always compile, run, and pass tests (for the solved exercises), 
// after you are done with each exercise (if you do them in order).  
// Compile and test frequently. Best continously.

// The extension of App allows writing the main method statements at the top
// level (the so called default constructor). For App objects they will be
// executed as if they were placed in the main method in Java.

package fpinscala

object Exercises extends App with ExercisesInterface {

  import fpinscala.List._

  // Exercise 3

  def fib (n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, a: Int, b: Int): Int =  //0+1 =1 1+1 = 2 2+2 = 4
      if (n <= 1) a
      else go(n-1, b, a+b)
    go(n, 0, 1)
  }// Exercise 4

  print(fib(3))

  def isSorted[A] (as: Array[A], ordered: (A,A) =>  Boolean): Boolean = {
    def go(n: Int): Boolean = {
      if(n == as.length-1) true
      else if (ordered(as(n), as(n+1)) == true) go(n+1)
      else false
    }
    go(0)
  }

  // Exercise 5

  def curry[A,B,C] (f: (A,B)=>C): A => (B => C) =
    (a: A) => (b: B) => f(a,b)

  // Exercise 6

  def uncurry[A,B,C] (f: A => B => C): (A,B) => C =
    (a: A, b: B) => f(a)(b)

  // Exercise 7

  def compose[A,B,C] (f: B => C, g: A => B) : A => C =
    (a: A) => f(g(a))

}
