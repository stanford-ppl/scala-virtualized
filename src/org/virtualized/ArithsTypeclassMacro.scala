package org.virtualized

import scala.reflect.macros.blackbox

private[virtualized] object ArithsTypeclassMacro extends TypeclassMacro {

  override def generateLookup(c: blackbox.Context)(name: c.TypeName): Option[c.Tree] = {
    None
  }

  override def generateImplementation(c: blackbox.Context)(tree: c.Tree) = {
    import c.universe._
    tree match {
      case ClassDef(_, className: TypeName, tparams, impl@Template(parents, selfType, bodyList)) =>
        val (fields, methods) = bodyList.partition { case _: ValDef => true case _ => false }
        val classTerm = className.toTermName
        val fieldNames = fields.map { case ValDef(_, name, _, _) => name }
        val fieldTypes = fields.map { case ValDef(_, _, typ, _) => typ }

        val distinctChildren = fieldTypes.distinct
        val typeMapping = fieldTypes.map { x => distinctChildren.indexWhere { y => x == y } }
        val distinctTypes = distinctChildren

        val arithEvidence = List.tabulate(distinctTypes.length) { i => TermName("arith" + i) }
        val stgEvidence = List.tabulate(distinctTypes.length) { i => TermName("tp" + i) }
        val evidences = arithEvidence ++ stgEvidence
        val arithEvidenceParams = distinctTypes.zip(arithEvidence).map { case (tp, term) =>
          ValDef(Modifiers(),term,AppliedTypeTree(Ident(TypeName("Arith")), List(tp)),EmptyTree)
          //q"$term: Arith[$tp]"
        }
        val stgEvidenceParams = distinctTypes.zip(stgEvidence).map { case (tp, term) =>
          ValDef(Modifiers(),term,AppliedTypeTree(Ident(TypeName("Type")), List(tp)),EmptyTree)
          //q"$term: Type[$tp]"
        }
        val implicits = arithEvidenceParams ++ stgEvidenceParams
        val fieldAriths = typeMapping.map { i => arithEvidence(i) }

        val aFields = List.tabulate(fields.length){i => TermName("fieldA" + i) }
        val bFields = List.tabulate(fields.length){i => TermName("fieldB" + i) }

        val aFieldsGet = fieldNames.zip(aFields).map{case (field,term) => q"val $term = a.$field(ctx,state)" }
        val bFieldsGet = fieldNames.zip(bFields).map{case (field,term) => q"val $term = b.$field(ctx,state)" }

        val neg = aFields.zip(fieldAriths).map{case (term,arith) => q"$arith.negate($term)(ctx,state)" }
        val plus = (aFields, bFields, fieldAriths).zipped.map{case (a, b, arith) => q"$arith.plus($a,$b)(ctx,state)" }
        val minus = (aFields, bFields, fieldAriths).zipped.map{case (a, b, arith) => q"$arith.minus($a,$b)(ctx,state)" }
        val times = (aFields, bFields, fieldAriths).zipped.map{case (a, b, arith) => q"$arith.times($a,$b)(ctx,state)" }
        val divide = (aFields, bFields, fieldAriths).zipped.map{case (a, b, arith) => q"$arith.divide($a,$b)(ctx,state)" }

        /**
          * Type class instance
          */
        val cls =
          q"""
            class ${TypeName(className.toString + "Arith")}()(implicit ..$implicits) extends Arith[$className] {
              override def negate(a: $className)(implicit ctx: org.virtualized.SourceContext, state: argon.State): $className = {
                ..$aFieldsGet
                $classTerm ( ..$neg )(ctx,state)
              }
              override def plus(a: $className, b: $className)(implicit ctx: org.virtualized.SourceContext, state: argon.State): $className = {
                ..$aFieldsGet
                ..$bFieldsGet
                $classTerm ( ..$plus )(ctx,state)
              }
              override def minus(a: $className, b: $className)(implicit ctx: org.virtualized.SourceContext, state: argon.State): $className = {
                ..$aFieldsGet
                ..$bFieldsGet
                $classTerm ( ..$minus )(ctx,state)
              }
              override def times(a: $className, b: $className)(implicit ctx: org.virtualized.SourceContext, state: argon.State): $className = {
                ..$aFieldsGet
                ..$bFieldsGet
                $classTerm ( ..$times )(ctx,state)
              }
              override def divide(a: $className, b: $className)(implicit ctx: org.virtualized.SourceContext, state: argon.State): $className = {
                ..$aFieldsGet
                ..$bFieldsGet
                $classTerm ( ..$divide )(ctx,state)
              }

            }
          """
        /**
          * Implicit type class evidence
          * (implicit chaining :( )
          */
        val imp =
          q"""
              implicit def ${TermName(classTerm.toString + "MayArith")}(implicit ..$implicits): Arith[$className] = new ${TypeName(className.toString + "Arith")}()(..$evidences)
            """
        /**
          * Type class "lookup"
          * hack to get type class evidence from Type[T]
          */
        // Not needed for now

       List(cls, imp)
    }
  }
}
