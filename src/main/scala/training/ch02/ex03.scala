package training.ch02

object ex03 {

    val moreThan = new Function2[Int, Int, Boolean] {
        def apply(a: Int, b: Int) = a > b
    }

    def partial1[A,B,C](a: A, f: (A, B) => C): B => C = {
        (b: B) => f(a, b)
    }

    def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
        a => b => f(a, b)
    }

    def sum(a: Int, b: Int) = a + b

    def main(args: Array[String]): Unit = {
        println(moreThan.apply(100, 10))
        println(moreThan.apply(10, 100))

        println(curry(sum)(5)(5))
    }
}