package org.virtualized

import scala.annotation.StaticAnnotation
import scala.meta._

/**
  * Converts Scala features that can not be overridden to method calls that can be given
  * arbitrary semantics.
  *
  * ==Features covered are==
  * {{{
  *   var x = e              =>       val x = __newVar(e)
  *   if(c) t else e         =>       __ifThenElse(c, t, e)
  *   return t               =>       __return(t)
  *   x = t                  =>       __assign(x, t)
  *   while(c) b             =>       __whileDo(c, b)
  *   do b while c           =>       __doWhile(c, b)
  * }}}
  *
  *
  * ===Poor man's infix methods for `Any` methods===
  * {{{
  *   t == t1                =>       infix_==(t, t1)
  *   t != t1                =>       infix_!=(t, t1)
  *   t *= t1                =>       infix_*=(t, t1)
  *   t += t1                =>       infix_+=(t, t1)
  *   t -= t1                =>       infix_-=(t, t1)
  *   t /= t1                =>       infix_/=(t, t1)
  * }}}
  */

class virtualize extends StaticAnnotation {

  inline def apply(defn: Any): Any = meta {

    val treeTransform: PartialFunction[Tree, Tree] = {
      case q"if ($b) $t else $f" =>
        q"__ifThenElse($b, $t, $f)"

      case q"return $e" =>
        q"__return($e)"

      case q"$ref = $e" =>
        q"__assign($ref, $e)"

      case q"while ($b) $e" =>
        q"__whileDo($b, $e)"

      case q"do $e while($b)" =>
        q"__doWhile($b, $e)"

      case Defn.Var(mods, pats, dcltpe, rhs) =>
        val tpe = dcltpe.map(tp => t"Cell[$tp]")
        val dcl = rhs.getOrElse(null)
        val newVar = q"__newVar($dcl)"
        Defn.Val(mods, pats, tpe, newVar)

      case q"$e1 == $e2" =>
        q"infix_==($e1, $e2)"

      case q"$e1 != $e2" =>
        q"infix_!=($e1, $e2)"

      case q"$e1 += $e2" =>
        q"infix_+=($e1, $e2)"

      case q"$e1 -= $e2" =>
        q"infix_-=($e1, $e2)"

      case q"$e1 *= $e2" =>
        q"infix_*=($e1, $e2)"

      case q"$e1 /= $e2" =>
        q"infix_/=($e1, $e2)"

      case q"$t.toString" =>
        q"infix_toString($t)"
    }

    defn match {
      case t: Tree =>
        t.transform(treeTransform).asInstanceOf[Stat]
      case _ =>
        abort("The virtualization is not applied to a Tree ? (this should be impossible)")
    }
  }

}