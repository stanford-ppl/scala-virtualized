package org.virtualized


import scala.annotation.StaticAnnotation
import scala.meta._

/**
  stage macro to generate a simple api

  @trait Type {
        this: Mixs* =>
        def unary_umethod(x:Type): B =
        def method[A]([args:A]*):B = tuples(args) match {
          case a => b
          ...
        }
        ...
  }

  =>

  trait Types {
         this: Mixs* =>
         type Type = TypeOps with StagedTop[Type]

        trait TypeOps {
          def unary_umethod: B
          def method[A]([args:A]*):B
         }

  }

  trait TypesExp

  trait TypeExp extends Staging with BitsExp { this: TextExp =>
    /** Type classes **/
    // --- Staged
    implicit object TypeType extends Staged[Type] {
      override def wrapped(x: Exp[Type]): Type = Type(x)
      override def typeArguments = Nil
      override def stagedClass = classOf[Type]
      override def isPrimitive = true
    }

    /** Infix methods **/
    case class Type(s: Exp[Type]) {
      def exp = s
      def unary_!(implicit ctx: SrcCtx): Type = Type(Type_not(this.s)(ctx))
      def &&(that: Type)(implicit ctx: SrcCtx): Type = Type(Type_and(this.s, that.s)(ctx))

      def +[A](rhs: A)(implicit ctx: SrcCtx, lft: Lift[A,Text]): Text = textify(this) + lft.lift(rhs)
    }

    /** Virtualized methods **/
    def infix_==(x: Type, y: Type)(implicit ctx: SrcCtx): Type = Type(Type_xnor(x.s, y.s)(ctx))
    def infix_!=(x: Type, y: Type)(implicit ctx: SrcCtx): Type = Type(Type_xor(x.s, y.s)(ctx))
    def infix_toString(x: Type)(implicit ctx: SrcCtx): Text = textify(x)


    // --- Bits
    implicit object TypeBits extends Bits[Type] {
      override def zero(implicit ctx: SrcCtx): Type = Typeean2Type(false)
      override def one(implicit ctx: SrcCtx): Type = Typeean2Type(true)
      override def random(max: Option[Type])(implicit ctx: SrcCtx): Type = Type(Type_random(max.map(_.s)))
      override def length = 1
    }
    override protected def bitsUnapply[T](tp: Staged[T]): Option[Bits[T]] = tp match {
      case TypeType => Some(TypeBits.asInstanceOf[Bits[T]])
      case _ => super.bitsUnapply(tp)
    }

    // --- Lifts
    implicit object Typeean2Type extends Lift[Typeean,Type] { val staged = TypeType }


    /** Constant lifting **/
    implicit def Typeean2Type(x: Typeean)(implicit ctx: SrcCtx): Type = lift(x)
    def Type(x: Typeean)(implicit ctx: SrcCtx): Const[Type] = constant[Type](x)


    /** IR Nodes **/
    case class Not(a: Exp[Type]) extends Op[Type] { def mirror(f:Tx) = Type_not(f(a)) }
    case class And(a: Exp[Type], b: Exp[Type]) extends Op[Type]  { def mirror(f:Tx) = Type_and(f(a), f(b)) }


    /** Constructors **/
    def Type_not(x: Exp[Type])(implicit ctx: SrcCtx): Exp[Type] = x match {
      case Const(c: Typeean) => Type(!c)    // Constant propagation
      case Op(Not(a)) => a                  // Typeean simplification: !(!a) => a
      case _ => stage(Not(x))(ctx)          // Default constructor
    }
    def Type_and(x: Exp[Type], y: Exp[Type])(implicit ctx: SrcCtx): Exp[Type] = (x,y) match {
      case (Const(a:Typeean), Const(b:Typeean)) => Type(a && b)     // Constant propagation
      case (Const(false), _)                    => Type(false)      // Short circuit evaluation: false && _ => false
      case (_, Const(false))                    => Type(false)      // Short circuit evaluation: _ && false => false
      case (Const(true), b)                     => b                // Typeean simplification: true && b => b
      case (a, Const(true))                     => a                // Typeean simplification: a && true => a
      case _ => stage( And(x,y) )(ctx)                              // Default constructor

  }

  */

/*
class staged extends StaticAnnotation {

  inline def apply(defn: Any): Any = meta {
      defn match {
        case Defn.t
        case _ =>
          abort("ABORT STAGING!")
      }
  }

}
*/