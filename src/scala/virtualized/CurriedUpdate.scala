package scala.virtualized

import language.experimental.macros
import scala.reflect.macros.blackbox.Context
import scala.annotation.StaticAnnotation

// TODO: Having a weird issue with adding SourceContext to updates
// found: scala.virtualized.SourceContext
// expected: virtualized.SourceContext.type
class CurriedUpdate extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro CurriedUpdate.curry
}

object CurriedUpdate {

  private val updateRenamed = "update$r"

  def curry(c: Context)(annottees: c.Tree*): c.Tree = {
    import c.universe._

    annottees.head match {
      case DefDef(mods, TermName(name), tparams, vparamss, tpt, rhs) =>
        if (name.toString != "update")
          c.abort(c.enclosingPosition, "CurriedUpdate can only be applied to the update method")
        if (vparamss.size != 2)
          c.abort(c.enclosingPosition, "Curried update must have two argument list (no SourceContext)")
        if (vparamss.head.isEmpty)
          c.abort(c.enclosingPosition, "The first argument list must not be empty")
        if (vparamss(1).size != 1)
          c.abort(c.enclosingPosition, "The second argument list must have only one element")

        val imprt   = q"import scala.language.experimental.macros"
        val implic: Modifiers = Modifiers(Flag.IMPLICIT | Flag.PARAM)
        //val ctx = List(List(ValDef(implic, TermName("ctx"), q"_root_.scala.virtualized.SourceContext", EmptyTree)))
        //val params = vparamss ++ ctx
        val updateR = DefDef(mods, TermName(updateRenamed), tparams, vparamss, tpt, rhs)
        val updateC = if (mods.hasFlag(Flag.OVERRIDE)) {
          q"override def update(values: Any*): Unit = macro CurriedUpdate.updateMacroDispatcher"
        }
        else {
          q"def update(values: Any*): Unit = macro CurriedUpdate.updateMacroDispatcher"
        }

        val result = q"$imprt ; $updateR ; $updateC"

        //c.info(c.enclosingPosition, showCode(result), true)
        result

      case _ =>
        c.abort(c.enclosingPosition, "CurriedUpdate can only be applied on method")
    }
  }

  def updateMacroDispatcher(c: Context)(values: c.Tree*)/*(ctx:c.Tree)*/: c.Tree = {
    import c.universe._

    val inds = values.take(values.size - 1)
    val value = values.last
    val self = c.prefix.tree
    q"$self.${TermName(updateRenamed)}(..$inds)($value)" //($ctx)"
  }
}