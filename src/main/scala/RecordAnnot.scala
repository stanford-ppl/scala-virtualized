package org.scala_lang.virtualized

import language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.reflect.macros.blackbox.Context

/**
  * Created by cedricbastin on 04/11/15.
  * taking inspiration from the @virtualize macro
  */

/**
  * These helper annotations should provide help to generate the type and RefinedManifest of a record when being used
  *
  * INPUT: when given:
  *
  * @record
  * case class Region(key: Int, name: String, comment: String)
  *
  * OUTPUT: it should generate:
  *
  * type Region = Record {
  *   val r_regionkey: Int
  *   val r_name: String
  *   val r_comment: String
  * }
  *
  * def Region(key: Rep[Int], name: Rep[String], comment: Rep[String]): Rep[Region] = Record (
  *   r_regionkey = key,
  *   r_name = name,
  *   r_comment = comment
  * )
  */

/** Annotation class for @record macro annotation. */
final class record extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro record.impl
}

//for console testing:
//object records {
//  def apply(annottees: Any*): Any = macro record.impl
//}

/** Companion object implementing @record macro annotation. */
private object record {

  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    val list = annottees.map(_.tree).toList
    //assert(list.size == 1) //we can only use @record on a single case class
    val tree = list.head
    val x = tree match {
      case ClassDef(mods, className: TypeName, tparams, impl@Template(parents, selfType, bodyList)) if (mods.hasFlag(Flag.CASE)) =>
        val fields = bodyList.take(bodyList.size - 1)
        assert(fields.forall { case _: ValDef => true }) //all except the last parameter should be field definitions
        assert(bodyList.last match { case _: DefDef => true }) //the constructor
        val cc =
        q"""
        type $className = Record {
        ${
          fields.map {
            case ValDef(_, termName, typeIdent, rhs) => "val r_" + termName + ": " + typeIdent
          }.mkString("; ")
        }
        }
        def ${className.toTermName}(
        ${
          fields.map {
            case ValDef(_, termName, typeIdent, rhs) => "" + termName + ": Rep[" + typeIdent + "]"
          }.mkString(", ")
        }
        ) = Record (
        ${
          fields.map {
            case ValDef(_, termName, typeIdent, rhs) => "r_" + termName + " = " + termName
          }.mkString(", ")
        }
        )"""
        //        ClassDef(
        //        modifiers,
        //        className,
        //        tparams,
        //        impl @ Template(
        //          parents,
        //          selfType,
        //          body @ List(
        //            ValDef(Modifiers(CASEACCESSOR | PARAMACCESSOR), TermName("i"), Ident(TypeName("Int")), EmptyTree),
        //            ValDef(Modifiers(CASEACCESSOR | PARAMACCESSOR), TermName("s"), Ident(TypeName("String")), EmptyTree),
        //            constructor = DefDef(
        //              Modifiers(),
        //              termNames.CONSTRUCTOR,
        //              List(),
        //              List(
        //                List(
        //                  ValDef(Modifiers(PARAM | PARAMACCESSOR),
        //                  TermName("i"),
        //                  Ident(TypeName("Int")),
        //                  EmptyTree),
        //                  ValDef(Modifiers(PARAM | PARAMACCESSOR), TermName("s"), Ident(TypeName("String")), EmptyTree)
        //                )
        //              ),
        //              TypeTree(),
        //              Block(List(pendingSuperCall), Literal(Constant(())))
        //            )
        //          )
        //        )
        //      )
        c.warning(tree.pos, showCode(cc))
        c.warning(tree.pos, showRaw(cc))
        println(showCode(cc))
        //c.Expr(Block(expandees, Literal(Constant(()))))
        c.Expr(cc)
      case _ =>
        c.warning(tree.pos, "Only case classes can be transformed using the @record annotation!\r\n CODE: "+showCode(tree)+"\r\n RAW: "+showRaw(tree))
        annottees.head //FIXME
    }
    x
  }
}
