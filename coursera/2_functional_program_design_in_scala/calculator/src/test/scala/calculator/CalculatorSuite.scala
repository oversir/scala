package calculator

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalatest._

import TweetLength.MaxTweetLength

@RunWith(classOf[JUnitRunner])
class CalculatorSuite extends FunSuite with ShouldMatchers {

  /******************
   ** TWEET LENGTH **
   ******************/

  def tweetLength(text: String): Int =
    text.codePointCount(0, text.length)

  test("tweetRemainingCharsCount with a constant signal") {
    val result = TweetLength.tweetRemainingCharsCount(Var("hello world"))
    assert(result() == MaxTweetLength - tweetLength("hello world"))

    val tooLong = "foo" * 200
    val result2 = TweetLength.tweetRemainingCharsCount(Var(tooLong))
    assert(result2() == MaxTweetLength - tweetLength(tooLong))
  }

  test("tweetRemainingCharsCount with a supplementary char") {
    val result = TweetLength.tweetRemainingCharsCount(Var("foo blabla \uD83D\uDCA9 bar"))
    assert(result() == MaxTweetLength - tweetLength("foo blabla \uD83D\uDCA9 bar"))
  }


  test("colorForRemainingCharsCount with a constant signal") {
    val resultGreen1 = TweetLength.colorForRemainingCharsCount(Var(52))
    assert(resultGreen1() == "green")
    val resultGreen2 = TweetLength.colorForRemainingCharsCount(Var(15))
    assert(resultGreen2() == "green")

    val resultOrange1 = TweetLength.colorForRemainingCharsCount(Var(12))
    assert(resultOrange1() == "orange")
    val resultOrange2 = TweetLength.colorForRemainingCharsCount(Var(0))
    assert(resultOrange2() == "orange")

    val resultRed1 = TweetLength.colorForRemainingCharsCount(Var(-1))
    assert(resultRed1() == "red")
    val resultRed2 = TweetLength.colorForRemainingCharsCount(Var(-5))
    assert(resultRed2() == "red")
  }

  test("calculator should eval literal") {
    val map: Calculator.ExprMap = Map()
    assert(Calculator.eval(Literal(7.0))(map) == 7.0)
  }

  test("calculator should eval reference") {
    val map: Calculator.ExprMap = Map("a" -> Var(Literal(3)))
    assert(Calculator.eval(Ref("a"))(map) == 3)
  }

  test("calculator should eval addition") {
    val map: Calculator.ExprMap = Map()
    assert(Calculator.eval(Plus(Literal(11), Literal(3)))(map) == 14)
  }

  test("calculator should eval subtraction") {
    val map: Calculator.ExprMap = Map()
    assert(Calculator.eval(Minus(Literal(11), Literal(3)))(map) == 8)
  }

  test("calculator should eval multiplication") {
    val map: Calculator.ExprMap = Map()
    assert(Calculator.eval(Times(Literal(11), Literal(3)))(map) == 33)
  }

  test("calculator should eval division") {
    val map: Calculator.ExprMap = Map()
    assert(Calculator.eval(Divide(Literal(11), Literal(3)))(map) == 3.6666666666666665)
  }

  test("calculator should intercept infinte loop") {
    val a = Plus(Ref("b"), Literal(1))
    val b = Plus(Ref("a"), Literal(2))
    val map: Calculator.ExprMap = Map("a" -> Var(a), "b" -> Var(b))
    assert(Calculator.eval(Ref("a"))(map).isNaN)
  }

  test("calculator should compute value") {
    val map: Calculator.ExprMap = Map("a" -> Var(Literal(5)))
    val value = Calculator.computeValues(map)
    assert(value("a")() == 5)
  }

  test("calculator should compute values") {
    val map: Calculator.ExprMap = Map(
      "a" -> Var(Literal(5)),
      "b" -> Var(Ref("a")),
      "c" -> Var(Plus(Ref("a"), Literal(2)))
    )
    val value = Calculator.computeValues(map)
    assert(value("a")() == 5)
    assert(value("b")() == 5)
    assert(value("c")() == 7)
  }

  test("calculator should update values") {
    val variable: Var[Expr] = Var(Literal(9))
    val map: Calculator.ExprMap = Map(
      "a" -> variable,
      "b" -> Var(Plus(Ref("a"), Literal(4))),
      "c" -> Var(Literal(3)),
      "d" -> Var(Times(Ref("b"), Ref("c")))
    )
    val value = Calculator.computeValues(map)
    assert(value("d")() == 39)
    variable() = Literal(10)
    assert(value("d")() == 42)
  }

  test("calculator should sort values") {
    val map: Calculator.ExprMap = Map(
      "a" -> Var(Literal(9)),
      "b" -> Var(Plus(Ref("a"), Literal(4))),
      "c" -> Var(Literal(1)),
      "d" -> Var(Times(Ref("b"), Ref("c")))
    )
    val value = Calculator.computeValues(map)
    val minimum = value.values.min
    assert(minimum() == 1)
  }

}
