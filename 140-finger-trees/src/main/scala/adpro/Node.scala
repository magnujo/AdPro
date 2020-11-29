package adpro

import Reduce.reduce

sealed trait Node[+A] {

  import Node.reduceNode

  // convenience delegations

  def toList: List[A] =
    reduce[Node].toList[A] (this)
}

case class Node2[A] (l: A, r: A) extends Node[A]
case class Node3[A] (l: A, m: A, r: A) extends Node[A]

object Node {

  // Exercise 10

  implicit lazy val reduceNode: Reduce[Node] = ???
}
