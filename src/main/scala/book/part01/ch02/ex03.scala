package fp.part01.ch02

object ex03 {

    val moreThan = new Function2[Int, Int, Boolean] {
        def apply(a: Int, b: Int) = a > b
    }

    // we're taking an argument 'a', and a function 'f'
    // we're returning a structure, that expects another argument: 'b'
    // when we'll apply that 'b', we'll get 'c'
    def partial1[A,B,C](a: A, f: (A, B) => C): B => C = {
        (b: B) => f(a, b)
    }

    // we're taking a function that expects 2 arguments: 'a' and 'b'
    // in the following argument lists, i.e. parentheses
    def curry[A,B,C](f: (A, B) => C): A => B => C = {
        a => b => f(a, b)
    }

    def sum(a: Int, b: Int) = a + b

    def main(args: Array[String]): Unit = {
        println(partial1[Int, Int, Int](13, _ + _)(10))
        println(curry[Int, Int, Int](_ + _)(1)(1))

        println(moreThan.apply(100, 10))
        println(moreThan.apply(10, 100))

        println(curry(sum)(5)(5))
    }
}