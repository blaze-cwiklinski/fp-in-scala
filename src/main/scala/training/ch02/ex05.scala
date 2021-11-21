package training

import ex03._

object ex05 {

    def compose[A,B,C](f: B => C, g: A => B): A => C = {
        a => f(g(a))
    }

    def increment = curry(sum)(1)

    def main(agrs: Array[String]): Unit = {
        println(compose(increment, increment)(0))
    }
}