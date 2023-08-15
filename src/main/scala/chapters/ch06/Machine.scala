package training.ch06

import javax.swing.InputMap

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Simulation {

    def evaluateInput(m: Machine, i: Input) =
        (m, i) match {
            // if we insert a "coin", the machine unlocks:
            case (Machine(true, candies, coins), Coin) if candies > 0 => Machine(false, candies, coins + 1)
            // if we perform a "turn" on an unlocked machine, it will reduce the number of candies:
            case (Machine(false, candies, coins), Turn) => Machine(true, candies - 1, coins)
            // no other action should be able to modify the state:
            case (_, _) => m
        }

    def simulateMachine(input: List[Input]): State[Machine, (Int, Int)] =
        for {
            _ <- State.traverse(input)(i => State.modify(evaluateInput(i, _)))
            s <- State.get
        } yield (s.candies, s.coins)
}