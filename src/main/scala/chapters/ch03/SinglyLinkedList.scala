package training.ch03

sealed trait SinglyLinkedList[+A]
case object Nil extends SinglyLinkedList[Nothing]
case class Cons[+A](head: A, tail: SinglyLinkedList[A]) extends SinglyLinkedList[A]

object SinglyLinkedList {
  def sum(ints: SinglyLinkedList[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: SinglyLinkedList[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): SinglyLinkedList[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // ex.02
  def tail[A](l: SinglyLinkedList[A]) = l match {
    case Nil => Nil
    case Cons(x, xs) => xs
  }
  
  // ex.03
  def setHead[A](l: SinglyLinkedList[A], h: A) = l match {
    case Nil => Cons(h, Nil)
    case Cons(x, xs) => Cons(h, xs)
  }

  // ex.04
  def drop[A](l: SinglyLinkedList[A], n: Int): SinglyLinkedList[A] = {
    if (n == 0) {
        l
    }
    else l match {
        case Nil => Nil
        case Cons(x, xs) => drop(xs, n-1)
    }
  }

  // ex.05
  def dropWhile[A](l: SinglyLinkedList[A], f: A => Boolean): SinglyLinkedList[A] = {
    l match {
        case Cons(x, xs) if f(x) => dropWhile(xs, f)
        case _ => l
    }
  }

  // Improving type inference
  def dropWhileImproved[A](l: SinglyLinkedList[A])(f: A => Boolean): SinglyLinkedList[A] = {
    l match {
        case Cons(x, xs) if f(x) => dropWhileImproved(xs)(f)
        case _ => l
    }
  }

  def append[A](l1: SinglyLinkedList[A], l2: SinglyLinkedList[A]): SinglyLinkedList[A] =
    l1 match {
        case Nil => l2
        case Cons(h, t) => Cons(h, append(t, l2))
    }
  
  // ex.06
  def init[A](l: SinglyLinkedList[A]): SinglyLinkedList[A] = {
    l match {
        case Nil => Nil
        case Cons(h, Nil) => Nil
        case Cons(h, t) => Cons(h, init(t))
    }
  }

  def foldRight[A,B](as: SinglyLinkedList[A], z: B)(f: (A, B) => B): B =
    as match {
        case Nil => z
        case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: SinglyLinkedList[Int]) =
    foldRight(ns, 0)(_ + _)

  def product2(ns: SinglyLinkedList[Double]) = 
    foldRight(ns, 1.0)(_ * _)

  // ex.07
  // It is impossible to implement early termination in the 'foldRight'
  // approach, since we are traversing the whole list before evaluating 
  // the passed function itself

  // ex.08
  // scala> SinglyLinkedList.foldRight(SinglyLinkedList(1,2,3), Nil: SinglyLinkedList[Int])(Cons(_, _))               
  // res10: SinglyLinkedList[Int] = Cons(1,Cons(2,Cons(3,Nil)))
  // The foldRight returned the original list
  // That makes sense since, as mentioned before, foldRight will be replaced with the Cons(),
  // which effectively makes us construct that list from left to right, i.e. produces the same list

  // ex.09
  def length[A](l: SinglyLinkedList[A]): Int = {
    foldRight(l, 0)((_, len) => 1 + len)
  }

  // ex.10
  @annotation.tailrec
  def foldLeft[A,B](l: SinglyLinkedList[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  // ex.11
  def sumLeft(l: SinglyLinkedList[Int]) = 
    foldLeft(l, 0)(_ + _)

  def productLeft(l: SinglyLinkedList[Double]) = 
    foldLeft(l, 1.0)(_ * _)

  def lengthLeft[A](l: SinglyLinkedList[A]) : Int =
    foldLeft(l, 0)((h, _) => h + 1)

  // ex.12
  def reverse[A](l: SinglyLinkedList[A]): SinglyLinkedList[A] =
    foldLeft(l, Nil: SinglyLinkedList[A])((a, b) => Cons(b, a))

  // ex.13
  def foldRightThroughLeft[A,B](l: SinglyLinkedList[A], z: B)(f: (A, B) => B): B = 
    foldLeft(reverse(l), z)((b, a) => f(a, b))

  // ex.14
  def appendThroughLeft[A](l1: SinglyLinkedList[A], l2: SinglyLinkedList[A]): SinglyLinkedList[A] = 
    foldLeft(reverse(l1), l2)((b, a) => Cons(a, b))
  
  def appendThroughRight[A](l1: SinglyLinkedList[A], l2: SinglyLinkedList[A]): SinglyLinkedList[A] = 
    foldRight(l1, l2)(Cons(_, _))

  // ex.15
  def concatLists[A](l1: SinglyLinkedList[SinglyLinkedList[A]]): SinglyLinkedList[A] =
    foldRight(l1, Nil: SinglyLinkedList[A])(append)

  // ex.16
  def add1toEachElement(l: SinglyLinkedList[Int]): SinglyLinkedList[Int] =
    foldLeft(reverse(l), Nil: SinglyLinkedList[Int])((h, t) => Cons(t+1, h))

  // ex.17
  def convertDouble2String(l: SinglyLinkedList[Double]): SinglyLinkedList[String] =
    foldLeft(reverse(l), Nil: SinglyLinkedList[String])((h, t) => Cons(t.toString, h))

  // ex.18
  def map[A,B](l: SinglyLinkedList[A])(f: A => B): SinglyLinkedList[B] = 
    foldLeft(reverse(l), Nil: SinglyLinkedList[B])((h, t) => Cons(f(t), h))

  // ex.19
  def filter[A](l: SinglyLinkedList[A])(f: A => Boolean): SinglyLinkedList[A] =
    foldRightThroughLeft(l, Nil: SinglyLinkedList[A])((h, t) => if (f(h)) Cons(h, t) else t)

  // ex.20
  def flatMap[A,B](l: SinglyLinkedList[A])(f: A => SinglyLinkedList[B]): SinglyLinkedList[B] =
    concatLists(map(l)(f))

  // ex.21
  def filterThroughFlatMap[A](l: SinglyLinkedList[A])(f: A => Boolean): SinglyLinkedList[A] =
    flatMap(l)(e => if (f(e)) SinglyLinkedList(e) else Nil)

  // ex.22
  def add2Lists(l1: SinglyLinkedList[Int], l2: SinglyLinkedList[Int]): SinglyLinkedList[Int] =
    (l1, l2) match {
      case (_, Nil) => Nil
      case (Nil, _) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1+h2, add2Lists(t1, t2))
    }
  
  // ex.23
  def zipWith[A,B,C](l1: SinglyLinkedList[A], l2: SinglyLinkedList[B])(f: (A, B) => C): SinglyLinkedList[C] =
    (l1, l2) match {
      case (_, Nil) => Nil
      case (Nil, _) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    }

  // ex.24
  def startsWith[A](sup: SinglyLinkedList[A], sub: SinglyLinkedList[A]): Boolean =
    (sup, sub) match {
      case (_, Nil) => true
      case (Cons(h1, t1), Cons(h2, t2)) => if (h1 == h2) startsWith(t1,t2) else false
      case (Nil, Cons(_,_)) => false
    }

  def hasSubsequence[A](sup: SinglyLinkedList[A], sub: SinglyLinkedList[A]): Boolean =
    (sup, sub) match {
      case (_, Nil) => true
      case (Nil, Cons(_, _)) => false
      case (Cons(h1, t1), Cons(h2, t2)) => if (h1==h2) startsWith(t1, t2) else hasSubsequence(t1, sub) 
    }
}