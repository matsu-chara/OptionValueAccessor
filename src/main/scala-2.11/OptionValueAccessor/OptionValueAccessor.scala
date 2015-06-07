package OptionValueAccessor

import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

object OptionValueAccessor {

  implicit class OptValue[A, B](val self: Option[A])(implicit ev: A <:< {def value: B}) {
    def ? : Option[B] = macro Impl.optValImpl[A, B]
  }

  implicit class OptOpt[A, B](val self: Option[Option[A]])(implicit ev: A <:< {def value: B}) {
    def ? : Option[B] = macro Impl.optOptImpl[A, B]
  }

  object Impl {
    def optValImpl[A: c.WeakTypeTag, B: c.WeakTypeTag](c: Context): c.Expr[Option[B]] = {
      import c.universe._
      c.Expr[Option[B]](q"${c.prefix}.self.map {_.value}")
    }

    def optOptImpl[A: c.WeakTypeTag, B: c.WeakTypeTag](c: Context): c.Expr[Option[B]] = {
      import c.universe._
      c.Expr[Option[B]](q"${c.prefix}.self.flatten.map {_.value}")
    }
  }
}