package streams

import common._

/**
 * This component implements the solver for the Bloxorz game
 */
trait Solver extends GameDef {

  /**
    * Make this frequently used type more wieldy.
    */
  type Path = (Block, List[Move])

  /**
   * Returns `true` if the block `b` is at the final position.
   */
  def done(path: Path): Boolean = path match {
    case (Block(b1, b2), _) => b1 == goal && b2 == goal
  }


  /**
   * This function takes two arguments: the current block `b` and
   * a list of moves `history` that was required to reach the
   * position of `b`.
   *
   * The `head` element of the `history` list is the latest move
   * that was executed, i.e. the last move that was performed for
   * the block to end up at position `b`.
   *
   * The function returns a stream of pairs: the first element of
   * the each pair is a neighboring block, and the second element
   * is the augmented history of moves required to reach this block.
   *
   * It should only return valid neighbors, i.e. block positions
   * that are inside the terrain.
   */
  def neighborsWithHistory(block: Block, history: List[Move]): Stream[Path] =
    block.legalNeighbors map {
      case (neighbor, move) => (neighbor, move :: history)
    } toStream

  /**
    * Convenience method which is easier to use in conjunction with
    * the map function.
    */
  def neighbors(path: Path): Stream[Path] =
    neighborsWithHistory _ tupled path

  /**
   * This function returns the list of neighbors without the block
   * positions that have already been explored. We will use it to
   * make sure that we don't explore circular paths.
   */
  def prune(neighbors: Stream[Path], visited: Set[Block]): Stream[Path] =
    neighbors filterNot (visited contains _._1)

  /**
   * The function `from` returns the stream of all possible paths
   * that can be followed, starting at the `head` of the `initial`
   * stream.
   *
   * The blocks in the stream `initial` are sorted by ascending path
   * length: the block positions with the shortest paths (length of
   * move list) are at the head of the stream.
   *
   * The parameter `explored` is a set of block positions that have
   * been visited before, on the path to any of the blocks in the
   * stream `initial`. When search reaches a block that has already
   * been explored before, that position should not be included a
   * second time to avoid cycles.
   *
   * The resulting stream should be sorted by ascending path length,
   * i.e. the block positions that can be reached with the fewest
   * amount of moves should appear first in the stream.
   *
   * Note: the solution should not look at or compare the lengths
   * of different paths - the implementation should naturally
   * construct the correctly sorted stream.
   *
   * Warning: the solution is in reverse order since it is more
   * efficient to build the list of moves. invert the obtained
   * list for the correct list of moves.
   */

  def from(initial: Stream[Path], visited: Set[Block]): Stream[Path] =
    if(initial.isEmpty) Stream.empty
    else {
      val newInitial = prune(initial flatMap neighbors, visited)
      val newVisited = visited ++ (newInitial map (_._1))
      initial #::: from(newInitial, newVisited)
    }

  /**
   * The stream of all paths that begin at the starting block.
   */
  lazy val pathsFromStart: Stream[Path] = {
    val initial = (startBlock, List.empty) #:: Stream.empty
    val visited = Set(startBlock)
    from(initial, visited)
  }

  /**
   * Returns a stream of all possible pairs of the goal block along
   * with the history how it was reached.
   */
  lazy val pathsToGoal: Stream[Path] =
    pathsFromStart filter done

  /**
   * The (or one of the) shortest sequence(s) of moves to reach the
   * goal. If the goal cannot be reached, the empty list is returned.
   *
   * Note: the `head` element of the returned list should represent
   * the first move that the player should perform from the starting
   * position.
   */
  lazy val solution: List[Move] = pathsToGoal match {
    case (block, moves) #:: _ => moves.reverse
    case _ => List.empty
  }
}
