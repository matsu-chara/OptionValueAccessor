package TypeDynamic

import scala.language.experimental.macros
import scala.reflect.macros.whitebox
import scala.reflect.macros.whitebox.Context

object TypeDynamic {
  class Example() extends Dynamic {
    val value: Int = 2
    def selectDynamic(name: String): Int = macro selectDynamicImpl
  }

  def selectDynamicImpl(c: whitebox.Context)(name: c.Expr[String]): c.Expr[Int] = {
    import c.universe._

    val nameStr: String = name.tree match {
      case pq"${n: String}" if n.startsWith("_") => n.drop(1)
      case _                                     => c.abort(c.enclosingPosition, s"#$name not found.")
    }

    c.Expr[Int](q"${c.prefix}.${TermName(nameStr)}")
  }
}