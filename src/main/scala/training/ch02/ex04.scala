package training.ch02

import training.ch02.ex03._

object ex04 {

    // pobieramy funkcję, która oczekuje carried
    // (w kolejnych listach argumentów) wartości a i b
    // i zwraca nam funkcję przyjmującą a i b jednocześnie
    def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
        (a, b) => f(a)(b)
    }

    def main(args: Array[String]): Unit = {
        println(sum(5, 5))
        println(curry(sum)(5)(5))
        println(uncurry(curry(sum))(5, 5))
    }
}