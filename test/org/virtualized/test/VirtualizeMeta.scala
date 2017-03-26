package org.virtualized.test

import org.virtualized._
import org.scalatest._

class VirtualizeMeta extends FlatSpec with Matchers{

  "virtualizeIfThenElse" should "be virtualized" in {

    //Careful, these tests depend on the line numbers they are written on!!
    Prog.test1() should be(3)
  }

  "virtualizeSimpleAssign" should "be virtualized" in {
    Prog.test2() should be(3)
  }

  "virtualizeOverride" should "be virtualized" in {
    Prog.test3() should be(3)
  }

  "virtualizeOverrideDef" should "be virtualized" in {
    Prog2.main() should be(3)
  }
}


@virtualize
object Prog extends EmbeddedControls{

  def test1(): Int = {
    val foo = 5
    var b = 3
    if (true)
      b
    else
      foo
  }

  def test2(): Int = {
    var x = 5
    x = 3
    x
  }

  def __ifThenElse[T](cond: Int, thenBr: T, elseBr: T): T = thenBr

  def test3(): Int = {
      if (2) 3 else 4
  }
}



object Prog2 extends EmbeddedControls{

  def __ifThenElse[T](cond: Int, thenBr: T, elseBr: T): T = thenBr

  @virtualize
  def main() = {
    val x = if (2) 3 else 4
    x
  }
}

trait StageAny[A]
trait Staged[A]

@stageany
object Prog3 {
  def test[A:StageAny]: Unit = ???
}



