package fp.part01.ch02

import fp.part01.ch02.ex03._

object ex05 {

    def compose[A,B,C](f: A => B, g: B => C): A => C = {
        x => g(f(x))
    }

    def increment = curry(sum)(1)

    def main(agrs: Array[String]): Unit = {
        println(compose(increment, increment)(0))
    }
}