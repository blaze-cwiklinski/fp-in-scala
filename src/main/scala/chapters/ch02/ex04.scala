package training.ch02

import training.ch02.ex03._

object ex04 {

    // we're taking a function that is curring,
    // and returning a function that combines 
    // the argument lists into one list
    def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
        (a, b) => f(a)(b)
    }

    def main(args: Array[String]): Unit = {
        println(sum(5, 5))
        println(curry(sum)(5)(5))
        println(uncurry(curry(sum))(5, 5))
    }
}