package fp.part01.ch04

sealed trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B]
    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B]
    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B]
    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C]
}

// ex.06
case class Left[+E](value: E) extends Either[E, Nothing] {
    override def map[B](f: Nothing => B): Either[E, B] = this
    override def flatMap[EE >: E, B](f: Nothing => Either[EE, B]): Either[EE, B] = this
    override def orElse[EE >: E, B >: Nothing](b: => Either[EE, B]): Either[EE, B] = b
    override def map2[EE >: E, B, C](b: Either[EE, B])(f: (Nothing, B) => C): Either[EE, C] = this
}

case class Right[+A](value: A) extends Either[Nothing, A] {
    override def map[B](f: A => B): Either[Nothing, B] = Right(f(value))
    override def flatMap[EE >: Nothing, B](f: A => Either[EE, B]): Either[EE, B] = f(value)
    override def orElse[EE >: Nothing, B >: A](b: => Either[EE, B]): Either[EE, B] = this
    override def map2[EE >: Nothing, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = 
        b.flatMap(bx => Right(f(value, bx)))
}

object Either {
    def mean(xs: List[Double]): Either[String, Double] =
        if(xs.isEmpty)  Left("mean of empty list")
        else            Right(xs.sum / xs.length)

    def safeDiv(x: Double, y: Double): Either[Exception, Double] =
        try     Right(x / y)
        catch   { case e: Exception => Left(e) }

    def Try[A](a: => A): Either[Exception, A] = 
        try     Right(a)
        catch   { case e: Exception => Left(e)}

    // ex.07
    def traverse[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
        es match {
            case Nil => Right(Nil)
            case h :: t => f(h).map2(traverse(t)(f))(_ :: _)
        }
    
    def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = 
        traverse(es)(e => e)

}

sealed class Name(value: String)
sealed class Age(value: Int)
case class Person(name: Name, age: Age)

object Person {
    def mkName(name: String): Either[String, Name] =
        if (name == "" || name == null) Left("invalid string")
        else Right(new Name(name))

    def mkAge(age: Int): Either[String, Age] =
        if (age < 0) Left("age has to be non-negative")
        else Right(new Age(age))

    def mkPerson(name: String, age: Int): Either[String, Person] =
        mkName(name).map2(mkAge(age))(new Person(_, _))

    // ex.08
    // The implementation could be based on the pattern matching,
    // so that we could have distinct cases for: 
    // [E, E], [E, A], [A, E] and [A, A] - though the implementation
    // could quickly become cumbersome, if we were to e.g. 
    // add mode fields in the Person class
}
