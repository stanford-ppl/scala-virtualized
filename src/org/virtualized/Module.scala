package org.virtualized

import language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.reflect.macros.blackbox

/** Annotation class for @module macro annotation. */
final class module extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro module.impl
}

/** Companion object implementing @module macro annotation. */
private object module {
  def impl(c: blackbox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    // Call the virtualize macro first
    virtualize.impl(c)(annottees:_*).tree match {
      case Block(inputs,ret) =>
        val (annottee, rest) = inputs match {
          case (a: DefDef) :: bs =>
            (Some(a), bs)

          case _ =>
            c.warning(c.enclosingPosition, "@module can only be used on a function definition.")
            (None, inputs)
        }
        val expandees = annottee match {
          case Some(a) => objectize(c)(a) :: rest
          case None    => rest
        }

        // c.info(c.enclosingPosition, showCode(expandees.head), true)
        c.Expr(Block(expandees, ret))

      case _ => throw new Exception("@virtualize didn't return a block?")
    }
  }

  def objectize(c: blackbox.Context)(d: c.Tree): c.Tree = {
    import c.universe._
    d match {
      case DefDef(mods,name,Nil,argss,retTp,body) =>
        val TermName(funcName) = name

        val privDefName = TermName("__"+funcName)
        val privDef = DefDef(Modifiers(Flag.PRIVATE),privDefName,Nil,argss,retTp,body)

        val argNamesAndTypes = argss.map{args =>
          args.map{case ValDef(_,n,tpt,_) => (n,tpt) }
        }
        val argTypesCurr = argNamesAndTypes.map(_.map(_._2))
        val argTypesFlat = argTypesCurr.flatten

        val argNamesCurr = argNamesAndTypes.map(_.map(_._1))
        val argNamesFlat = argNamesCurr.flatten
        val unwrapArgNames = argNamesFlat.map{case TermName(n) => TermName("u"+n) }
        val unwrapArgs = argNamesFlat.zip(unwrapArgNames).map{case (nam,unam) => q"val $unam = unwrap($nam)" }
        val wrapArgs   = argNamesFlat.zip(unwrapArgNames).map{case (nam,unam) => q"val $nam = wrap($unam)" }
        val freshes    = unwrapArgNames.zip(argTypesFlat).map{case (unam,tp)  => q"val $unam = fresh[$tp]" }

        val funcVal =
          q"""
             private val func = {
               ..$freshes;
               ..$wrapArgs;
               val b = () => unwrap{ $privDefName(...$argNamesCurr) }
               val l = List(..$unwrapArgNames)
               val f = __decl(l, b)
               __valDef(f, ${Literal(Constant(funcName))})
               //registerFunction(f)
               f
             }
           """

        val applyDef =
          q"""def apply(...$argss): $retTp = {
               ..$unwrapArgs;
               val l: List[Exp[_]] = List.apply(..$unwrapArgNames);
               wrap{ __call(func, l) }
           }"""

        val obj = q"""object $name {
             $privDef;
             $funcVal;
             $applyDef;
         }"""

        c.info(c.enclosingPosition, showCode(obj), true)
        obj

      case _:DefDef =>
        c.error(c.enclosingPosition, "@module does not yet support type arguments.")
        EmptyTree
    }
  }
}
