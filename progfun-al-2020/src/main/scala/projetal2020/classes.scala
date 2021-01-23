package projetal2020.classes

import annotation.tailrec
import projetal2020.enums._
import Orientation._
import Instruction._

final case class Grid(x: Int, y: Int)

final case class MowerState(x: Int, y: Int, orientation: Orientation)

final case class MowerLifecycle(
    initialState: MowerState,
    instructions: List[Instruction],
    finalState: MowerState
)

final case class Config(grid: Grid, mowers: List[Mower])

final case class Mower(state: MowerState, instructions: List[Instruction]) {

  def execute(grid: Grid): MowerState = {

    def left(state: MowerState): MowerState =
      MowerState(state.x, state.y, state.orientation match {
        case North => West
        case East  => North
        case South => East
        case West  => South
      })

    def right(state: MowerState): MowerState =
      MowerState(state.x, state.y, state.orientation match {
        case North => East
        case East  => South
        case South => West
        case West  => North
      })

    def forward(state: MowerState): MowerState = {

      val movement: (Int, Int) = state.orientation match {
        case North => (0, 1)
        case East  => (1, 0)
        case South => (0, -1)
        case West  => (-1, 0)
      }

      val newState =
        MowerState(
          state.x + movement._1,
          state.y + movement._2,
          state.orientation
        )

      if (newState.x < 0 || newState.x > grid.x || newState.y < 0 || newState.y > grid.y) {
        state
      } else {
        newState
      }
    }

    @tailrec
    def go(state: MowerState, instructions: List[Instruction]): MowerState =
      instructions match {
        case instruction :: rest =>
          go(instruction match {
            case Left    => left(state)
            case Right   => right(state)
            case Forward => forward(state)
          }, rest)
        case Nil => state
      }
    go(this.state, this.instructions)
  }
}
