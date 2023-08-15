package fp.part01.ch05

sealed trait Stream[+A] {
    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
        case Cons(h, t) => f(h(), t().foldRight(z)(f))
        case _          => z
    }
    
    def exists(p: A => Boolean): Boolean = this match {
        case Cons(h, t) => p(h()) || t().exists(p)
        case _          => false
    }
    
    def existsViaFold(p: A => Boolean): Boolean = 
        foldRight(false)((a, b) => p(a) || b)   
    
    // ex.04
    def forAll(p: A => Boolean): Boolean = 
        foldRight(true)((a, b) => p(a) && b)

    def headOption: Option[A] = this match {
        case Empty      => None
        case Cons(h, t) => Some(h())
    }
    
    // ex.06
    def headOptionViaFold: Option[A] = 
        foldRight(None: Option[A])((a, _) => Some(a))

    // ed.07
    def map[B](f: A => B): Stream[B] =
        foldRight(Stream.empty[B])((a, b) => Stream.cons(f(a), b))

    def filter(f: A => Boolean): Stream[A] =
        foldRight(Stream.empty[A])((a, b) => if (f(a)) Stream.cons(a, b) else b)

    def append[AA >: A](s : => Stream[AA]): Stream[AA] =
        foldRight(s)((a, b) => Stream.cons(a, b))
    
    def flatMap[B](f: A => Stream[B]): Stream[B] =
        foldRight(Stream.empty[B])((a,b) => f(a).append(b))

    def find(p: A => Boolean): Option[A] =
        filter(p).headOption

}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
    
    // smart constructor
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
        lazy val head = hd
        lazy val tail = tl
        Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
        if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
    
    // ex.01
    def toList[A](s: Stream[A]): List[A] = s match {
        case Empty      => Nil
        case Cons(h, t) => h() :: (toList(t()))
    }

    // ex.02
    def drop[A](s: Stream[A], n: Int): Stream[A] = s match {
        case Cons(h, t) if n == 0   => s
        case Cons(h, t) if n > 0    => t().drop(n-1)
        case _                      => empty
    }

    def take[A](s: Stream[A], n: Int): Stream[A] = s match {
        case Cons(h, t) if n > 0    => cons(h(), t().take(n-1))
        case _                      => empty
    }

    // ex.03
    def takeWhile[A](s: Stream[A], b: A => Boolean): Stream[A] = s match {
        case Cons(h, t) if b(h())   => cons(h(), t().takeWhile(b))
        case _                      => empty
    }
    
    // ex.05
    def takeWhileViaFold[A](s: Stream[A], p: A => Boolean): Stream[A] = 
        s.foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else empty[A])
    
    val ones: Stream[Int] = cons(1, ones)

    // ex.08
    def constant[A](a: A): Stream[A] = cons(a, constant(a))

    // ex.09
    def from(n: Int): Stream[Int] = cons(n, from(n+1))

    // ex.10
    val fibs: Stream[Int] = {
        def go(f: Int, s: Int): Stream[Int] = cons(f, go(s, f+s))
        go(1, 1)
    }
    
    // ex.11
    def unfold[A, S](s: S)(f: S => Option[(A, S)]): Stream[A] =
        f(s) match {
            case Some((h, t))   => cons(h, unfold(t)(f))
            case None           => empty
        }
    
    // ex.12
    val fibsViaUnfold: Stream[Int] =
        unfold((1, 1)){
            case (f, s) => Some(f, (s, f+s))
        }
    
    def fromViaUnfold(n: Int): Stream[Int] =
        unfold(n){
            case n: Int => Some((n, n+1))
        }
    
    def constantViaUnfold[A](a: A): Stream[A] =
        unfold(a){
            case _ => Some(a, a)
        }
    
    def onesViaUnfold: Stream[Int] =
        unfold(1){
            case _: Int => Some(1, 1)
        }

    // ex.13
    def mapViaUnfold[A, B](s: Stream[A])(f: A => B): Stream[B] =
        unfold(s){
            case Cons(h, t) => Some(f(h()), t())
            case _          => None
        }
    
    def takeViaUnfold[A](s: Stream[A])(n: Int): Stream[A] = 
        unfold((s, n)){
            case (Cons(h, t), n) if n > 0   => Some(h(), (t(), n-1))
            case _                          => None
        }
    
    def takeWhileViaUnfold[A](s: Stream[A])(b: A => Boolean): Stream[A] =
        unfold(s){
            case Cons(h, t) if b(h())   => Some(h(), t())
            case _                      => None
        }

    def zipWithViaUnfold[A,B,C](s1: Stream[A], s2: Stream[B])(f: (A, B) => C): Stream[C] =
        unfold((s1, s2)){
            case (_, Empty)                     => None
            case (Empty, _)                     => None
            case (Cons(h1, t1), Cons(h2, t2))   => Some(f(h1(), h2()), (t1(), t2()))
        }

    def zipAllViaUnfold[A,B](s1: Stream[A], s2: Stream[B]): Stream[(Option[A], Option[B])] = 
        unfold((s1, s2)){
            case (Empty, Empty)                 => None
            case (Cons(h1, t1), Empty)          => Some(((Some(h1()), None), (t1(), Empty)))
            case (Empty, Cons(h2, t2))          => Some(((None, Some(h2())), (Empty, t2())))
            case (Cons(h1, t1), Cons(h2, t2))   => Some(((Some(h1()), Some(h2())), (t1(), t2())))
        }

    // ex.14
    def startsWith[A](s1: Stream[A], s2: Stream[A]): Boolean =
        zipAllViaUnfold(s1, s2).foldRight(false){
            case ((Some(a1), Some(a2)), tail) if a1 == a2 && tail   => true
            case ((Some(_), None), _)                               => true
            case _                                                  => false
        }

    // ex.15
    def tails[A](s: Stream[A]): Stream[Stream[A]] =
        unfold(s){
            case Cons(h, t) => Some(Cons(h, t), t())
            case _          => None
        }

    // ex.16
    def scanRight[A,B](s: Stream[A])(state: B)(f: (A, => B) => B): Stream[B] = 
        s.foldRight((state, Stream(state))){
            case (s, (h, t)) => {
                val temp = f(s, h)
                (temp, cons(temp, t))
            }
        }._2

    implicit class Streams[A](s: Stream[A]) {
        def toList: List[A] = Stream.toList(s)
        def drop(n: Int) = Stream.drop(s, n)
        def take(n: Int) = Stream.take(s, n)
        def takeWhile(b: A => Boolean) = Stream.takeWhile(s, b)
    }
}