package projetal2020.test

import projetal2020.classes._
import projetal2020.enums._
import Orientation._
import Instruction._
import org.scalatest.funsuite.AnyFunSuite

class MowerTest extends AnyFunSuite {

  private val grid = Grid(5, 5)

  test("Should execute instructions") {
    val initialState = MowerState(1, 2, North)
    val mower = Mower(
      initialState,
      List(Left, Forward, Left, Forward, Left, Forward, Left, Forward, Forward)
    )
    assert(mower.execute(grid) == MowerState(1, 3, North))
  }

  test("Should change orientation when turning") {
    val initialState = MowerState(4, 4, North)
    val mower = Mower(initialState, List(Left))
    assert(mower.execute(grid) == MowerState(4, 4, West))
  }

  test("Should change state when mower does not go outside the grid") {
    val initialState = MowerState(4, 4, North)
    val mower = Mower(initialState, List(Forward))
    assert(mower.execute(grid) == MowerState(4, 5, North))
  }

  test("Should not change move if mower tries to go outside the grid") {
    val initialState = MowerState(5, 5, North)
    val mower = Mower(initialState, List(Forward))
    assert(mower.execute(grid) == initialState)
  }
}
