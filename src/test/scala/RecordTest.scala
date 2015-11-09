package org.scala_lang.virtualized
package annotation

import org.scala_lang.virtualized.SourceContext
import org.scala_lang.virtualized.record
import org.scalatest.{ FlatSpec, ShouldMatchers }

class RecordTest extends FlatSpec with ShouldMatchers with EmbeddedControls {

  case class Exp[T](t:T)
  type Rep[T] = Exp[T]
  implicit def rep[T](t:T):Rep[T] = Exp(t)
  implicit def unrep[T](r:Rep[T]):T = r.t
  trait Record {// for use as Record {}
    def list:Seq[Any]
  }
  object Record { // for use as Record
    def apply(a:Any*) = () //FixMe: return type
  }

  "virtualizeRecord" should "be virtualized" in {
    val universe: scala.reflect.runtime.universe.type = scala.reflect.runtime.universe
    import universe._

    @record
    case class RTest0()

    @record
    case class RTest1()
    import O_RTest1._
    val o1 = RTest1()
    println(o1.getClass())

    @record
    case class RTest2(key: Int, name: String, comment: String)
    import O_RTest2._
    val o2 = RTest2(4, "Hello", "World")
    println(o1.getClass())


  }
}
