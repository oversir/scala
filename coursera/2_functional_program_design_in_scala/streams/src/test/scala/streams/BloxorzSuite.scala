package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Bloxorz._

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) => move match {
        case Left => block.left
        case Right => block.right
        case Up => block.up
        case Down => block.down
      }
    }
  }

  trait LevelUnsolvable extends SolutionChecker {
    /* terrain for level unsolvable*/

    val level =
      """oo--ooo
        |oSoooTo
        |ooo-ooo""".stripMargin
  }

  trait Level1 extends SolutionChecker {
      /* terrain for level 1*/

    val level =
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    val block = Block(Pos(1,1),Pos(1,1))
    val history = List(Left,Up)
    val setNeighborsWithHistory = Set((Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
                                      (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up)))
    val explored = Set(Block(Pos(1,2),Pos(1,3)), Block(Pos(1,1),Pos(1,1)))
    val setNewNeighborsOnly = Set((Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up)))

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }

  trait Level3 extends SolutionChecker {
    /* terrain for level 3*/

    val level =
      """------ooooooo--
        |oooo--ooo--oo--
        |ooooooooo--oooo
        |oSoo-------ooTo
        |oooo-------oooo
        |------------ooo""".stripMargin

    val optsolution = List(Right, Up, Right, Right, Right, Up, Left, Down, Right, Up,
                           Up, Right, Right, Right, Down, Down, Down, Right, Up)
  }

  trait Level6 extends SolutionChecker {
    /* terrain for level 6*/

    val level =
      """-----oooooo----
        |-----o--ooo----
        |-----o--ooooo--
        |Sooooo-----oooo
        |----ooo----ooTo
        |----ooo-----ooo
        |------o--oo----
        |------ooooo----
        |------ooooo----
        |-------ooo-----""".stripMargin

    val optsolution = List(Right, Right, Right, Down, Right, Down, Down, Right, Down, Down, Right,
                           Up, Left, Left, Left, Up, Up, Left, Up, Up, Up, Right, Right, Right,
                           Down, Down, Left, Up, Right, Right, Down, Right, Down, Down, Right)  }

	test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0,0)), "0,0")
      assert(terrain(Pos(1,1)), "1,1") // start
      assert(terrain(Pos(4,7)), "4,7") // goal
      assert(terrain(Pos(5,8)), "5,8")
      assert(!terrain(Pos(5,9)), "5,9")
      assert(terrain(Pos(4,9)), "4,9")
      assert(!terrain(Pos(6,8)), "6,8")
      assert(!terrain(Pos(4,11)), "4,11")
      assert(!terrain(Pos(-1,0)), "-1,0")
      assert(!terrain(Pos(0,-1)), "0,-1")
    }
  }

	test("findChar level 1") {
    new Level1 {
      assert(startPos == Pos(1,1))
    }
  }

  test("neighbors") {
    new Level1 {
      assert(neighborsWithHistory(block, history).toSet == setNeighborsWithHistory)
      assert(neighbors((block, history)).toSet == setNeighborsWithHistory)
    }
  }

  test("prune") {
    new Level1 {
      assert(prune(neighborsWithHistory(block, history), explored).toSet == setNewNeighborsOnly)
    }
  }

  test("no solution for level unsolvable") {
    new LevelUnsolvable {
      assert(solution.isEmpty)
    }
  }

	test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length == optsolution.length)
    }
  }

  test("optimal solution for level 3") {
    new Level3 {
      assert(solve(solution) == Block(goal, goal))
    }
  }

  test("optimal solution length for level 3") {
    new Level3 {
      assert(solution.length == optsolution.length)
    }
  }

  test("optimal solution for level 6") {
    new Level6 {
      assert(solve(solution) == Block(goal, goal))
    }
  }

  test("optimal solution length for level 6") {
    new Level6 {
      assert(solution.length == optsolution.length)
    }
  }

}
