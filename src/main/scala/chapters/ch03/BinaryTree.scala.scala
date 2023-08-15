package training.ch03

sealed trait BinaryTree[+A]
case class Leaf[A](value: A) extends BinaryTree[A]
case class Branch[A](left: BinaryTree[A], right: BinaryTree[A]) extends BinaryTree[A]

object BinaryTree {
  // ex.25
  def size[A](t: BinaryTree[A]): Int =
    t match {
        case Leaf(_) => 1
        case Branch(l, r) => 1 + size(l) + size(r)
    }

  // ex.26
  def maximum(t: BinaryTree[Int]): Int =
    t match {
        case Leaf(v) => v
        case Branch(l, r) => maximum(l) max maximum(r)
    }

  // ex.27
  def depth[A](t: BinaryTree[A]): Int =
    t match {
        case Leaf(_) => 1
        case Branch(l, r) => (1+ depth(l)) max (1 + depth(r))
    }

  // ex.28
  def map[A,B](t: BinaryTree[A])(f: A => B): BinaryTree[B] = 
    t match {
        case Leaf(value) => Leaf(f(value))
        case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }

  // ex.29
  // f -> operation performed on each leaf (map)
  // g -> operation performed between all the leafs (fold)
  def fold[A,B](t: BinaryTree[A])(f: A => B)(g: (B, B) => B): B =
    t match {
        case Leaf(v) => f(v)
        case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }

  def sizeThroughFold[A](t: BinaryTree[A]): Int =
    fold(t)(_ => 1)((l, r) => 1 + l + r)

  // the following function works for scala 3
  // def maximumThroughFold[A](t: BinaryTree[A]): A =
  //   fold(t)(v => v)(_ max _)

  def depthThroughFold[A](t: BinaryTree[A]): Int =
    fold(t)(_ => 1)((l, r) => (1 + l) max (1 + r))

  // the following function works for scala 3
  // def mapThroughFold[A, B](t: BinaryTree[A])(f: A => B): BinaryTree[B] =
  //   fold(t)(f)(Branch(_, _))

}