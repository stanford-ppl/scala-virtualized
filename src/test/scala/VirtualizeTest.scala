package org.scala_lang.virtualized
package annotation

import org.scala_lang.virtualized.SourceContext
import org.scalatest.{ FlatSpec, ShouldMatchers }


class VirtualizeSpec extends FlatSpec with ShouldMatchers with EmbeddedControls {

  def __ifThenElse[T](cs: Seq[Boolean], tb: => T, eb: => T): T = {
    if (cs forall (_ == true)) tb else eb
  }

  def infix_+[T](x1: List[T], x2: Any): String = {
    x1.toString+"+"+x2.toString
  }

  def infix_==[T](x1: List[T], x2: List[T]): Boolean = {
    (x1 zip x2) forall (p => p._1 == p._2)
  }

  "virtualizeSourceContext" should "be virtualized" in {
    implicit class OpsCls(lhs: Boolean){
      def op(rhs: Boolean)(implicit pos: SourceContext) = pos.toString + " " + pos.methodName
    }

    def virtualizeContext() = true op false

    //Careful, these tests depend on the line numbers they are written on!!
    virtualizeContext() should be("VirtualizeTest.scala:27:36 virtualizeContext")
  }

  "virtualizeSourceContextNested" should "be virtualized" in {

    def a()(implicit pos: SourceContext) = b()
    def b()(implicit pos: SourceContext) = c()
    def c()(implicit pos: SourceContext) = pos.toString + " " + pos.methodName

    //SourceContext macro should only be applied at the highest level
    //Afterwards the implicit parameter should be passed down the forwarding calls
    def virtualizeContext() = a()
    virtualizeContext() should be("VirtualizeTest.scala:41:32 virtualizeContext")
  }

  def infix_+(x1: String, x2: Boolean): String = "trans"

  "StringConcat" should "be virtualized" in {

    @virtualize
    def virtualizeIfTest() = "wefjbh" + true + "" + 6

    virtualizeIfTest() should be("trans6")
  }

  "StringCaseClassConcat" should "be virtualized" in {

    @virtualize
    def m = {
      case class C(i:Int) {def x = "wefjbh" + true}
      C(6).x
    }
    def virtualizeIfTest() = m

    virtualizeIfTest() should be("trans")
  }

  "method virtualizeIfTest" should "be virtualized" in {

    @virtualize
    def virtualizeIfTest(cs: Boolean*) = if (cs) "yep" else "nope"

    virtualizeIfTest(true, false) should be("nope")
    virtualizeIfTest(true, true) should be("yep")
  }

  "object VirtualizeIfTest" should "be virtualized" in {

    @virtualize
    object VirtualizeIfTest {
      def apply(cs: Boolean*) = if (cs) "yep" else "nope"
    }

    VirtualizeIfTest(true, false) should be("nope")
    VirtualizeIfTest(true, true) should be("yep")
  }

  "VirtualizeIfTraitTest" should "be virtualized" in {

    @virtualize
    trait VirtualizeIfTrait {
      def apply(cs: Boolean*) = if (cs) "yep" else "nope"
    }

    object VirtualizeIfTraitTest extends VirtualizeIfTrait

    VirtualizeIfTraitTest(true, false) should be("nope")
    VirtualizeIfTraitTest(true, true) should be("yep")
  }

  "VirtualizeIfTrait2Test" should "be virtualized" in {

    trait IfListInt {
      def __ifThenElse[T](cs: Seq[Int], tb: => T, eb: => T): T = {
        if (cs forall (_ != 0)) tb else eb
      }
    }

    @virtualize
    trait VirtualizeIfTrait2 { this: IfListInt =>
      def apply(cs: Int*) = if (cs) "yep" else "nope"
    }

    object VirtualizeIfTrait2Test extends VirtualizeIfTrait2 with IfListInt

    VirtualizeIfTrait2Test(1, 0) should be("nope")
    VirtualizeIfTrait2Test(1, 1) should be("yep")
  }

  // Should use default `__ifThenElse` from EmbeddedControls.
  "defaultIfTest" should "be virtualized" in {

    @virtualize
    def defaultIfTest(c: Boolean) = if (c) "yep" else {
      var x = "no"
      x + "pe"
    }

    defaultIfTest(false) should be("nope")
    defaultIfTest(true) should be("yep")
  }

  // Should use inner virtualized `__ifThenElse`
  "virtualizeInnerIfTest" should "be virtualized" in {

    // This overrides the `__ifThenElse` in `EmbeddedControls`
    def __ifThenElse[T](c: Boolean, thenBr: => T, elseBr: => T): T =
      if (!c) thenBr else elseBr

    @virtualize
    def virtualizeInnerIfTest(c: Boolean) = if (c) "yep" else "nope"

    virtualizeInnerIfTest(false) should be("yep")
    virtualizeInnerIfTest(true) should be("nope")
  }

  "virtualizeWhileDo" should "be virtualized" in {
    def __whileDo(cond: Seq[Boolean], body: => String): String = if (cond forall (_ == true)) body else "nope"

    @virtualize
    def virtualizeWhileTest(c: Boolean*) = while (c) { "yep" }

    virtualizeWhileTest(false, true) shouldBe "nope"
    virtualizeWhileTest(true, true) shouldBe "yep"
  }

  "virtualizeVariables" should "be virtualized" in {
    case class Var[T](var x: T)
    def __newVar(init: Int): Var[Int] = Var(init + 1)
    def __assign(lhs: Var[Int], rhs: Int): Unit = lhs.x = lhs.x + rhs
    def __readVar(lhs: Var[Int]): Int = lhs.x

    @virtualize
    def virtualizeVariablesTest() = {
      var x = 5 // x = 6
      x = 3 // x = 9
      println(x.toString)
      x // inject readVar
    }

    virtualizeVariablesTest() shouldBe 9
  }

  "virtualizeAnyMethods" should "be virtualized" in {
    def infix_==(x1: List[Int], x2: List[Int]) = Nil
    def infix_!=(x1: List[Int], x2: List[Int]) = Nil
    def infix_##(x: List[Int]) = Nil
    def infix_equals(x1: List[Int], x2: List[Int]) = Nil
    def infix_hashCode(x: List[Int]) = Nil
    def infix_asInstanceOf[T](x: List[Int]) = List(1)
    def infix_isInstanceOf[T](x: List[Int]) = Nil
    def infix_toString(x: List[Int]) = Nil
    def infix_getClass(x: List[Int]) = Nil
  
    @virtualize
    def virtualizeAnyTest(x: List[Int], y: List[Int]) = {
      (x == y) ++
      (x != y) ++
      x.## ++ x.##() ++
      (x equals y) ++ x.equals(y) ++
      x.hashCode ++ x.hashCode() ++
      x.isInstanceOf[Int] ++
      x.asInstanceOf[Int] ++
      x.toString ++ x.toString() ++
      x.getClass ++ x.getClass()
    }

    virtualizeAnyTest(List(1,2,3), List(4,5,6)) shouldBe List(1)
  }

  // note: you can't define overloaded methods inside the test block below
  def infix_wait(x: List[Int]): List[Int] = Nil
  def infix_wait(x: List[Int], timeout: Long): List[Int] = Nil
  def infix_wait(x: List[Int], timeout: Long, nanos: Int): List[Int] = Nil

  "virtualizeAnyRefMethods" should "be virtualized" in {
    def infix_eq(x1: List[Int], x2: List[Int]) = Nil
    def infix_ne(x1: List[Int], x2: List[Int]) = Nil
    def infix_notify(x: List[Int]) = Nil
    def infix_notifyAll(x: List[Int]) = List(1)
    def infix_synchronized[T](x: List[Int], body: => T) = Nil
    def infix_clone(x: List[Int]) = Nil
    def infix_finalize(x: List[Int]) = Nil

    @virtualize
    def virtualizeAnyRefTest(x: List[Int], y: List[Int]) = {
      (x eq y) ++ x.eq(y) ++
      (x ne y) ++ x.ne(y) ++
      x.notify ++ x.notify() ++
      x.notifyAll ++ x.notifyAll() ++
      x.synchronized("hello") ++
      x.wait ++ x.wait() ++ 
      x.wait(10) ++
      x.wait(10, 10) ++
      x.clone ++ x.clone() ++
      x.finalize ++ x.finalize()
    }

    virtualizeAnyRefTest(List(1,2,3), List(4,5,6)) shouldBe List(1,1)
  }

  "numericPlusTest" should "not be virtualized" in {
    def numericPlusTest(a: Int, b: Int): Int = a+b
    numericPlusTest(1, 2) should be(3)
  }

  "virtualizePlusTest" should "be virtualized" in {
    def infix_+(a1: Any, a2: Any) = a2.toString + a1 //+ on Any is not virtualized!
    @virtualize
    def virtualizePlusTest(a: String, b: List[Boolean]) = a + b //only "StringLiteral"+b will be virtualized!
    virtualizePlusTest("you", List(false)) should be("youList(false)")
  }

  "virtualizeAnyPlusTest" should "not be virtualized" in {
    @virtualize
    def virtualizePlusTest(a: Any, b: List[Boolean]) = a.toString + b //only "literal"+b will be virtualized!
    virtualizePlusTest("you", List(false)) should be("youList(false)")
  }

  "virtualizePlusTestStringLiteral" should "be virtualized" in {
    def infix_+(s: String, a: Any) = a.toString + s //no need to overwrite?
    @virtualize
    def virtualizePlusTest(a: Any) = "test" + a
    virtualizePlusTest(List(false)) should be("List(false)test") //check that call is actually intercepted
  }

  "method virtualizeEqualsTest" should "be virtualized" in {

    @virtualize
    def virtualizeEqualsTest(a: List[Boolean], b: List[Boolean]) = a == b

    virtualizeEqualsTest(List(true, true), List(true, false)) should be(false)
    virtualizeEqualsTest(List(true, true), List(true, true, false)) should be(true)
    (List(true, true) == List(true, true, false)) should be(false)
  }

  "object VirtualizeEqualsTest" should "be virtualized" in {

    @virtualize
    object VirtualizeEqualsTest {
      def apply(a: List[Boolean], b: List[Boolean]) = a == b
    }

    VirtualizeEqualsTest(List(true, true), List(true, false)) should be(false)
    VirtualizeEqualsTest(List(true, true), List(true, true, false)) should be(true)
    (List(true, true) == List(true, true, false)) should be(false)
  }

  // Should use default `Any.==` method from EmbeddedControls.
  "defaultEqualsTest" should "be virtualized" in {

    @virtualize
    def defaultEqualsTest(a: Boolean, b: Boolean) = a == b

    defaultEqualsTest(false, true) should be(false)
    defaultEqualsTest(true, true) should be(true)
  }

  "guardEqualsTest" should "be virtualized" in {
    def guardEqualsSanityTest(xs: List[Boolean], ys: List[Boolean]) = (xs,ys) match {
      case (x::xs, y::ys) if infix_==(xs,ys) => true
      case _ => false
    }

    @virtualize
    def guardEqualsTest(xs: List[Boolean], ys: List[Boolean]) = (xs,ys) match {
      case (x::xs, y::ys) if xs==ys => true
      case _ => false
    }
    guardEqualsSanityTest(List(false, true, false), List(true, true)) should be(true)
    guardEqualsTest(List(false, true, false), List(true, true)) should be(true)
  }

  "parameter of virtualizeParamTest" should "not be virtualized" in {

    val c = false
    def virtualizeParamTest(
      @virtualize s: String = if (c) "yep" else "nope") = s

    virtualizeParamTest() should be("nope")
  }

  "type parameter of virtualizeTParamTest" should "not be virtualized" in {

    def virtualizeTParamTest[@virtualize T](s: T) = s

    virtualizeTParamTest("nope") should be("nope")
  }

  "try expression in virtualizeTryTest" should "not be virtualized" in {

    @virtualize
    def virtualizeTryTest[T](s: => T) = try s catch { case _:Exception => "hello" }

    virtualizeTryTest("nope") should be("nope")
  }

  "throw expression in virtualizeThrowTest" should "not be virtualized" in {

    case class MyException(msg: String) extends Exception

    @virtualize
    def virtualizeThrowTest(e: String) = throw MyException(e)

    try {
      virtualizeThrowTest("nope")
    } catch {
      case MyException(e) => e should be("nope")
    }
  }

  "isInstanceOf and asInstanceOf" should "not be virtualized" in {
    @virtualize
    def virtualizeInstanceOf(o: Object) = if (o.isInstanceOf[String]) o.asInstanceOf[String] else null
    virtualizeInstanceOf("hello") should be("hello")
    virtualizeInstanceOf(Nil) should be(null)
  }

  "Scopes" should "be generated" in {
    case class MyCls[T](i:T)
    trait Ops {
      def m(i:Int):MyCls[Int]
      def apply: Any
    }
    trait OpsExp[R] extends Ops{
      def m(i:R) = MyCls(i)
//      def apply = ??? //(i:Int):MyCls = m(i)
    }

    //new scope needs to be inside an object for this to work!
    //none should be needed!
    @virtualize
    class OptiQL {
      def MyDSL[R](b: => R) = new Scope[Ops, OpsExp[Int], Int](b)
      val result = MyDSL {
        println("hello")
        m(5).i
      }
      result should be (5)
    }
  }

  // "withTpe" should "crash in this case" in {
  //   //have to make an explicit call to 'execute' side effects
  //    //values could not be annotated...
  //   @virtualize
  //   {
  //     val magic = withTpee(Community){ println("with state") }
  //   }
  // }

}
