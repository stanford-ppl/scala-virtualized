package org.scala_lang.virtualized
package annotation

import org.scala_lang.virtualized.SourceContext
import org.scalatest.{ FlatSpec, ShouldMatchers }

class VirtualizeSpec extends FlatSpec with ShouldMatchers with EmbeddedControls {

  def __ifThenElse[T](cs: List[Boolean], tb: => T, eb: => T): T = {
    if (cs forall (_ == true)) tb else eb
  }

  def infix_+[T](x1: List[T], x2: Any): String = {
    x1.toString+"+"+x2.toString
  }

  def infix_==[T](x1: List[T], x2: List[T]): Boolean = {
    (x1 zip x2) forall (p => p._1 == p._2)
  }

  "virtualizeSourceContext" should "be virtualized" in {
    implicit def conv(b:Boolean):OpsCls = OpsCls(b)
    case class OpsCls(tis: Boolean){
      def someOp(tat: Boolean)(implicit pos: SourceContext) = pos.toString
    }

    //@virtualize
    def virtualizeContext()(implicit pos: SourceContext) = true.someOp(false)

    //Carefull, these tests depend on the line numbers they are writen on!!
    //Char offset not available?
    virtualizeContext() should be("VirtualizeTest.scala:32:21")
  }

  "virtualizeSourceContextNested" should "be virtualized" in {

    def a()(implicit pos: SourceContext) = b()
    def b()(implicit pos: SourceContext) = c()
    def c()(implicit pos: SourceContext) = pos.toString()

    //SourceContext macro should only be applied at the highest level
    //Afterwards the implicit parameter should be passed down the forwarding calls
    a() should be("VirtualizeTest.scala:43:5")
  }

  "virtualizeIfTest" should "be virtualized" in {

    @virtualize
    def virtualizeIfTest(cs: List[Boolean]) = if (cs) "yep" else "nope"

    virtualizeIfTest(List(true, false)) should be("nope")
    virtualizeIfTest(List(true, true)) should be("yep")
  }

  "VirtualizeIfTest" should "be virtualized" in {

    @virtualize
    object VirtualizeIfTest {
      def apply(cs: List[Boolean]) = if (cs) "yep" else "nope"
    }

    VirtualizeIfTest(List(true, false)) should be("nope")
    VirtualizeIfTest(List(true, true)) should be("yep")
  }

  "VirtualizeIfTraitTest" should "be virtualized" in {

    @virtualize
    trait VirtualizeIfTrait {
      def apply(cs: List[Boolean]) = if (cs) "yep" else "nope"
    }

    object VirtualizeIfTraitTest extends VirtualizeIfTrait

    VirtualizeIfTraitTest(List(true, false)) should be("nope")
    VirtualizeIfTraitTest(List(true, true)) should be("yep")
  }

  "VirtualizeIfTrait2Test" should "be virtualized" in {

    trait IfListInt {
      def __ifThenElse[T](cs: List[Int], tb: => T, eb: => T): T = {
        if (cs forall (_ != 0)) tb else eb
      }
    }

    @virtualize
    trait VirtualizeIfTrait2 { this: IfListInt =>
      def apply(cs: List[Int]) = if (cs) "yep" else "nope"
    }

    object VirtualizeIfTrait2Test extends VirtualizeIfTrait2 with IfListInt

    VirtualizeIfTrait2Test(List(1, 0)) should be("nope")
    VirtualizeIfTrait2Test(List(1, 1)) should be("yep")
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

  "virtualizeEqualsTest" should "be virtualized" in {

    @virtualize
    def virtualizeEqualsTest(a: List[Boolean], b: List[Boolean]) = a == b

    virtualizeEqualsTest(List(true, true), List(true, false)) should be(false)
    virtualizeEqualsTest(List(true, true), List(true, true, false)) should be(true)
    (List(true, true) == List(true, true, false)) should be(false)
  }

  "VirtualizeEqualsTest" should "be virtualized" in {

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
    def virtualizeTryTest[T](s: => T) = try s

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
    case class MyCls(i:Int)
    trait Ops {
      def m(i:Int):MyCls
      def apply: Any
    }
    trait OpsExp extends Ops{
      def m(i:Int) = MyCls(i)
//      def apply = ??? //(i:Int):MyCls = m(i)
    }

    //new scope needs to be inside an object for this to work!
    //none should be needed!
    @virtualize
    class OptiQL {
      def apply(b: Int) = {
        new Scope[Ops, OpsExp, Int](m(b))
        //new Scope[Ops, OpsExp, Int]{m(b)} //different version of scopes, more involved but less dangerous
      }
      def x = {
        new OpsExp { def apply = 7} //other anonymous classes should not be deleted!
      }
    }


    println(new OptiQL()(9))
  }
}
