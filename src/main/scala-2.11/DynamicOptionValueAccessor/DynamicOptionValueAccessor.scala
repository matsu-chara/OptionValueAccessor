package DynamicOptionValueAccessor

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

object DynamicOptionValueAccessor {

  implicit class OptValue[A, B](val self: Option[A])(implicit ev: A <:< {def value: B}) extends Dynamic {
    def selectDynamic(name: String): Option[B] = macro Impl.Opt.selectDynamicImpl[B]
  }

  implicit class OptOpt[A, B](val self: Option[Option[A]])(implicit ev: A <:< {def value: B}) {
    def selectDynamic(name: String): Option[B] = macro Impl.OptOpt.selectDynamicImpl[B]
  }

  object Impl {
    object Opt {
      def selectDynamicImpl[B: c.WeakTypeTag](c: whitebox.Context)(name: c.Expr[String]): c.Expr[Option[B]] = {
        import c.universe._

        val nameStr: String = name.tree match {
          case pq"${n: String}" if n.startsWith("_") => n.drop(1)
          case _                                     => c.abort(c.enclosingPosition, s"#$name not found.")
        }
        c.Expr[Option[B]](q"${c.prefix}.self.map {_.${TermName(nameStr)}}")
      }
    }

    object OptOpt {
      def selectDynamicImpl[B: c.WeakTypeTag](c: whitebox.Context)(name: c.Expr[String]): c.Expr[Option[B]] = {
        import c.universe._
        val nameStr: String = name.tree match {
          case pq"${n: String}" if n.startsWith("_") => n.drop(1)
          case _                                     => c.abort(c.enclosingPosition, s"#$name not found.")
        }
        c.Expr[Option[B]](q"${c.prefix}.self.flatten.map {_.${TermName(nameStr)}}")
      }
    }
  }
}