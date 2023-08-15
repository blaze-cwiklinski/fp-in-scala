package fp.part01.ch04

import scala.{Option => _, None => _, Some => _}

sealed trait Option[+A] {
    def map[B](f: A => B): Option[B]
    def flatMap[B](f: A => Option[B]): Option[B]
    def getOrElse[B >: A](default: => B): B
    def orElse[B >: A](ob: => Option[B]): Option[B]
    def filter(f: A => Boolean): Option[A]
}

// ex.01
case class Some[+A](get: A) extends Option[A] {
    override def map[B](f: A => B): Option[B] = Some(f(get))
    override def flatMap[B](f: A => Option[B]): Option[B] = f(get)
    override def getOrElse[B >: A](default: => B): B = get
    override def orElse[B >: A](ob: => Option[B]): Option[B] = this
    override def filter(f: A => Boolean): Option[A] = if(f(get)) this else None
}

case object None extends Option[Nothing] {
    override def map[B](f: Nothing => B): Option[B] = None
    override def flatMap[B](f: Nothing => Option[B]): Option[B] = None
    override def getOrElse[B >: Nothing](default: => B): B = default
    override def orElse[B >: Nothing](ob: => Option[B]): Option[B] = ob
    override def filter(f: Nothing => Boolean): Option[Nothing] = None
}

object Option {
    def mean(xs: Seq[Double]): Option[Double] = 
        if (xs.isEmpty) None
        else Some(xs.sum / xs.length)

    // ex.02
    def variance(xs: Seq[Double]): Option[Double] = 
        mean(xs) match {
            case None => None
            case Some(m) => mean(xs.map(x => math.pow(x-m, 2)))
        }

    def variance_alternative(xs: Seq[Double]): Option[Double] =
        mean(xs).flatMap(m => mean(xs.map(x => math.pow(x-m, 2))))

    def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f

    val absOpt: Option[Double] => Option[Double] = lift(math.abs)

    // ex.03
    def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
        a.flatMap(ax => b.map(bx => f(ax, bx)))
    
    def Try[A](a: => A): Option[A] = 
        try Some(a)
        catch {case e: Exception => None}

    def map2_alternative[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
        for {
            ax <- a
            bx <- b
        } yield f(ax, bx)

    def parseInts(a: List[String]): Option[List[Int]] = 
        sequence(a map (i => Try(i.toInt)))
    
    // ex.04
    def sequence[A](as: List[Option[A]]): Option[List[A]] =
        as match {
            case Nil => Some(Nil)
            case h :: t => h.flatMap(hx => sequence(t).map(hx :: _))
        }

    // ex.05
    def traverse[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] =
        a match {
            case Nil => Some(Nil)
            case h :: t => map2_alternative(f(h), traverse(t)(f))(_ :: _)
        }
    
    def sequenceViaTraverse[A](as: List[Option[A]]): Option[List[A]] =
        traverse(as)(a => a)
}