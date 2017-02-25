package org.virtualized

import language.experimental.macros
import scala.reflect.macros.whitebox.Context

/**
  * Default implementation of org.virtualized Scala control structures.
  *
  * This trait is adapted from the `EmbeddedControls` trait in
  * Scala org.virtualized.  See also
  * [[https://raw.github.com/namin/scala/topic-virt/src/library/scala/EmbeddedControls.scala]]
  *
  * The `EmbeddedControls` trait provides method definitions where
  * calls to the methods are treated by the compiler in a special way.
  * The reason to express these calls as methods is to give embedded
  * DSLs a chance to provide their own definitions and thereby override
  * the standard interpretation of the compiler.
  *
  * Example: When faced with an `if` construct, the `@org.virtualized`
  * macro annotation will generate a method call:
  * `__ifThenElse(cond, thenp, elsep)`
  *
  * This method call will be bound to an implementation based on normal
  * rules of scoping.  If it binds to the standard one in this trait,
  * the corresponding macro will replace it by an `If` tree node. If
  * not, the call will be left as it is and a staging or interpreting
  * DSL can take over.
  *
  * @note This is feature experimental.
  * @note None of the above will happen unless you annotate your code with `@virtualize`.
  */
trait EmbeddedControls {

  import EmbeddedControls._

  // NOTE: Some of the signatures below have "by-val" arguments where
  // one would expect "by-name" arguments.  However, since these are
  // all macros the difference is irrelevant.  Furthermore, there's
  // currently a bug precluding the use of "by-name" parameters in
  // macros (See [[https://issues.scala-lang.org/browse/SI-5778
  // SI-5778]]).
  trait Cell[T] {
    def read(): T
    def write(x: T): Unit
  }

  case class SCell[T](var x:T) extends Cell[T] {
    def read(): T = x
    def write(_x: T): Unit = {
      x = _x
    }
  }

  implicit def readCell[T](x:Cell[T]): T = x.read()

  // Control structures
  def __ifThenElse[T](cond: Boolean, thenBr: T, elseBr: T): T = macro ifThenElseImpl[T]
  def __return(expr: Any): Nothing = macro returnImpl
  def __assign[T](lhs: T, rhs: T): Unit = macro assignImpl[T]
  def __assign[T](lhs: Cell[T], rhs: T): Unit = lhs.write(rhs)
  def __whileDo(cond: Boolean, body: Unit): Unit = macro whileDoImpl
  def __doWhile(body: Unit, cond: Boolean): Unit = macro doWhileImpl
  def __newVar[T](init: T): Cell[T] = SCell(init)


  def infix_+=[T](lhs: T, rhs: T): Unit = macro plusEqualsImpl[T]
  def infix_-=[T](lhs: T, rhs: T): Unit = macro minusEqualsImpl[T]
  def infix_*=[T](lhs: T, rhs: T): Unit = macro timesEqualsImpl[T]
  def infix_/=[T](lhs: T, rhs: T): Unit = macro divEqualsImpl[T]



  // Poor man's infix methods for `Any` methods
  def infix_+(x1: String, x2: Any): String = macro string_+
  def infix_+(x1: Any, x2: Any): Any = macro any_+ // don't know the return type => should actually never be produced by LanguageVirtualization
  def infix_==(x1: Any, x2: Any): Boolean = macro any_==
  def infix_!=(x1: Any, x2: Any): Boolean = macro any_!=

  def infix_toString(x: Any): String = macro any_toString


}


/**
  * EmbeddedControls companion object containing macro implementations.
  */
private object EmbeddedControls {

  // Control structures

  def ifThenElseImpl[T](c: Context)(
    cond: c.Expr[Boolean], thenBr: c.Expr[T], elseBr: c.Expr[T]): c.Expr[T] = {

    import c.universe._
    c.Expr(q"if ($cond) $thenBr else $elseBr")
  }

  def returnImpl(c: Context)(expr: c.Expr[Any]): c.Expr[Nothing] = {
    import c.universe._
    c.Expr(q"return $expr")
  }

  def assignImpl[T](c: Context)(lhs: c.Expr[T], rhs: c.Expr[T]): c.Expr[Unit] = {
    import c.universe._
    c.Expr(q"$lhs = $rhs")
  }

  def whileDoImpl(c: Context)(
    cond: c.Expr[Boolean], body: c.Expr[Unit]): c.Expr[Unit] = {

    import c.universe._
    c.Expr(q"while ($cond) $body")
  }

  def doWhileImpl(c: Context)(
    body: c.Expr[Unit], cond: c.Expr[Boolean]): c.Expr[Unit] = {

    import c.universe._
    c.Expr(q"do $body while ($cond)")
  }

  def string_+(c: Context)(
    x1: c.Expr[String], x2: c.Expr[Any]): c.Expr[String] = {

    import c.universe._
    c.Expr(q"$x1.+($x2)")
  }

  def plusEqualsImpl[T](c: Context)(lhs: c.Expr[T], rhs: c.Expr[T]): c.Expr[Unit] = {
    import c.universe._
    c.Expr(q"$lhs += $rhs")
  }

  def minusEqualsImpl[T](c: Context)(lhs: c.Expr[T], rhs: c.Expr[T]): c.Expr[Unit] = {
    import c.universe._
    c.Expr(q"$lhs -= $rhs")
  }

  def timesEqualsImpl[T](c: Context)(lhs: c.Expr[T], rhs: c.Expr[T]): c.Expr[Unit] = {
    import c.universe._
    c.Expr(q"$lhs *= $rhs")
  }

  def divEqualsImpl[T](c: Context)(lhs: c.Expr[T], rhs: c.Expr[T]): c.Expr[Unit] = {
    import c.universe._
    c.Expr(q"$lhs /= $rhs")
  }

  def any_+(c: Context)(
    x1: c.Expr[Any], x2: c.Expr[Any]): c.Expr[Any] = {

    import c.universe._
    c.Expr(q"$x1.+($x2)")
  }

  def any_==(c: Context)(
    x1: c.Expr[Any], x2: c.Expr[Any]): c.Expr[Boolean] = {

    import c.universe._
    c.Expr(q"$x1.==($x2)")
  }

  def any_!=(c: Context)(
    x1: c.Expr[Any], x2: c.Expr[Any]): c.Expr[Boolean] = {

    import c.universe._
    c.Expr(q"$x1.!=($x2)")
  }

  def any_toString(c: Context)(x: c.Expr[Any]): c.Expr[String] = {

    import c.universe._
    c.Expr(q"$x.toString()")
  }


}