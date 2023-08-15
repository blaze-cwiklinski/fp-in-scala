package fp.part01.ch06

import scala.annotation.tailrec

trait RNG {
    def nextInt: (Int, RNG)
}

object RNG {

    type Rand[+A] = RNG => (A, RNG)

    val int: Rand[Int] = _.nextInt

    def unit[A](a: A): Rand[A] =
        rng => (a, rng)

    def map[A,B](s: Rand[A])(f: A => B): Rand[B] = 
        rng => {
            val (a, newRng) = s(rng)
            (f(a), newRng)
        }

    case class SimpleRNG(seed: Long) extends RNG {
        def nextInt: (Int, RNG) = {
            val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
            val nextRNG = SimpleRNG(newSeed)
            val n = (newSeed >>> 16).toInt
            (n, nextRNG)
        }
    }

    // ex.01
    def nonNegativeInt(rng: RNG): (Int, RNG) = {
        val (int, newRng) = rng.nextInt
        val nonNeg = if (int < 0) -(int + 1) else int
        (nonNeg, newRng)
    }

    def nonNegativeEven: Rand[Int] =
        map(nonNegativeInt)(i => i - i%2)

    // ex.02
    def double(rng: RNG): (Double, RNG) = {
        val (int, newRng) = nonNegativeInt(rng)
        val double = int.toDouble / Int.MaxValue
        (double, newRng)
    }

    // ex.03
    def intDouble(rng: RNG): ((Int, Double), RNG) = {
        val (int, newRng1) = rng.nextInt
        val (double, newRng2) = newRng1.double
        ((int, double), newRng2)
    }

    def doubleInt(rng: RNG): ((Double, Int), RNG) = {
        val (double, newRng1) = rng.double
        val (int, newRng2) = newRng1.nextInt
        ((double, int), newRng2)
    }
    
    def double3(rng: RNG): ((Double, Double, Double), RNG) = {
        val (double1, newRng1) = rng.double
        val (double2, newRng2) = newRng1.double
        val (double3, newRng3) = newRng2.double
        ((double1, double2, double3), newRng3)
    }

    // ex.04
    def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
        @tailrec
        def go(i: Int, l: List[Int], r: RNG): (List[Int], RNG) = {
            if (i > 0) {
                val (int, newRng) = r.nextInt
                go(i - 1, int :: l, newRng)
            }
            else (l, r)
        }
        go(count, Nil, rng)
    }

    // ex.05
    def doubleViaMap: Rand[Double] =
        map(nonNegativeInt)(_.toDouble / Int.MaxValue)

    // ex.06
    def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] = 
        rng => {
            val (a, newRng1) = ra(rng)
            val (b, newRng2) = rb(newRng1)
            (f(a, b), newRng2)
        }

    def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
        map2(ra, rb)((_, _))
    
    def intDoubleViaMap(ra: Rand[Int], rb: Rand[Double]): Rand[(Int, Double)] =
        both(int, double)
    
    def doubleIntViaMap(ra: Rand[Double], rb: Rand[Int]): Rand[(Double, Int)] =
        both(double, int)

    // ex.07
    // inspired by the offcial repo
    def sequence[A](rs: List[Rand[A]]): Rand[List[A]] =
        rs.foldRight(unit(Nil: List[A]))((x, r) => map2(x, r)(_ :: _))

    // ex.08
    def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
        rng => {
            val (a, newRng) = f(rng)
            g(a)(newRng)
        }

    def nonNegativeLessThan(mod: Int): Rand[Int] =
        flatMap(nonNegativeInt){
            int => {
                if (mod < 0) {
                    unit(0)
                }
                else unit(int % mod)
            }
        }

    def mapViaFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] =
        flatMap(s){
            // here we pass the 'g' function:
            a => unit(f(a))
        }

    def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
        flatMap(ra){
            a => map(rb){
                b => f(a, b)
            }
        }

    def dieRoll: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)
    
    implicit class RNGs(s: RNG) {
        def nonNegativeInt: (Int, RNG) = RNG.nonNegativeInt(s)
        def double: (Double, RNG) = RNG.double(s)
        def intDouble: ((Int, Double), RNG) = RNG.intDouble(s)
        def doubleInt: ((Double, Int), RNG) = RNG.doubleInt(s)
        def double3: ((Double, Double, Double), RNG) = RNG.double3(s)
        def ints(count: Int): (List[Int], RNG) = RNG.ints(count)(s)
    }
}