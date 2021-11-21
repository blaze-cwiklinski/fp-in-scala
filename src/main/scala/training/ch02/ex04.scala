package training

import ex03._

object ex04 {

    def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
        (a, b) => f(a)(b)
    }

    def main(args: Array[String]): Unit = {
        println(sum(5, 5))
        println(curry(sum)(5)(5))
        println(uncurry(curry(sum))(5, 5))
    }
}