
trait Generator[+T] {
  self =>

  def generate: T

  def map[S](f: T => S): Generator[S] = new Generator[S] {
    override def generate = f(self.generate)
  }

  def flatMap[S](f: T => Generator[S]): Generator[S] = new Generator[S] {
    override def generate = f(self.generate).generate
  }


}

val integers = new Generator[Int] {
  val rand = new java.util.Random
  def generate = rand.nextInt()
}

/*
val booleans = new Generator[Boolean] {
  def generate = integers.generate > 0
}
*/
val booleans = for(x <- integers) yield x > 0
booleans.generate
def pairs[T, U](t: Generator[T], u: Generator[U]) = new Generator[(T, U)] {
  def generate = (t.generate, u.generate)
}
def single[T](x: T) = new Generator[T] {
  def generate = x
}
def choose(lo: Int, hi: Int) =
  for (x <- integers) yield lo + x % (hi - lo)
def oneOf[T](xs: T*) =
  for(idx <- choose(0, xs.length)) yield xs(idx)
def lists = for {
  isEmpty <- booleans
  list <- if(isEmpty) emptyLists else nonEmptyLists
} yield list
def emptyLists = single(Nil)
def nonEmptyLists: Generator[List[Int]] = for {
  head <- integers
  tail <- lists
} yield head :: tail
//val lst = lists.generate
trait Tree
case class Inner(left: Tree, right: Tree) extends Tree
case class Leaf(x: Int) extends Tree
def isLeaf = booleans.generate
def leaf = single(Leaf(integers.generate))
def tree: Generator[Tree] = for {
  left <- if(isLeaf) leaf else tree
  right <- if(isLeaf) leaf else tree
} yield Inner(left, right)

//tree.generate

abstract class Writer {
  def write(msg: String)
}

class StringWriter extends Writer {
  var target = new StringBuilder
  override def write(msg: String) = {
    target.append(msg)
  }

  override def toString = target.toString
}

trait UpperCaseConverter extends Writer {
  abstract override def write(msg: String) = {
    super.write(msg.toUpperCase)
  }
}

trait ProfanityFilter extends Writer {
  abstract override def write(msg: String) = {
    super.write(msg.replace("stupid", "s*****"))
  }
}

def writeStuff(writer: Writer) = {
  writer.write("This is stupid")
  println(writer)
}
writeStuff(new StringWriter with UpperCaseConverter
                            with ProfanityFilter)
writeStuff(new StringWriter with ProfanityFilter
                            with UpperCaseConverter)
