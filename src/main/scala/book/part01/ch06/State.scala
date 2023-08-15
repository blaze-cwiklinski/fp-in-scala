package fp.part01.ch06

case class State[S,+A](run: S => (A, S)){
    // needed to see the signature from the official repo
    def flatMap[B](f: A => State[S, B]): State[S, B] = 
        State(
            s => {
                val (a, newState) = run(s)
                f(a).run(newState)
            }
        )

    def map[B](f: A => B): State[S, B] = 
        flatMap(
            a =>
                State(
                    s => (f(a), s)
                )
        )
    
    def map2[B,C](interState: State[S, B])(f: (A, B) => C): State[S, C] =
        for {
            a <- this
            b <- interState
        } yield f(a, b)
}

object State {

    type Rand[A] = State[RNG,A]

    def unit[S,A](a: A): State[S, A] = State((a, _))

    def get[S]: State[S, S] = State(s => (s, s))

    def set[S](s: S): State[S, Unit] = State(_ => ((), s))

    def modify[S](f: S => S): State[S, Unit] = 
        for {
            s <- get
            _ <- set(f(s))
        } yield ()

    def sequence[S,A](ss: List[State[S, A]]): State[S, List[A]] =
        ss.foldRight(unit[S, List[A]](Nil))((s, a) => s.map2(a)(_ :: _))

    def traverse[S,A,B](ss: List[A])(f: A => State[S,B]): State[S, List[B]] =
        ss.foldRight(unit[S, List[B]](Nil))((s, a) => f(s).map2(a)(_ :: _))

}