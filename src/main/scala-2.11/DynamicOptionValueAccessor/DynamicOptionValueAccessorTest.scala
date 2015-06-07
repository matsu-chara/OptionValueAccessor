package DynamicOptionValueAccessor

import DynamicOptionValueAccessor._

object DynamicOptionValueAccessorTest {

  case class A(value: Int)
  case class B(value: Option[A])
  case class C(value: Option[B])
  case class D(value: Option[C])
  case class E(value: D)

  def main(args: Array[String]): Unit = {
    val edcba: Option[E] = Some(E(D(Some(C(Some(B(Some(A(1)))))))))
    println(
      edcba
        .selectDynamic("_value")
        .selectDynamic("_value")
        .selectDynamic("_value")
        .selectDynamic("_value")
        .selectDynamic("_value")
    )

    val edcbnone = Some(E(D(None)))
    println(
      edcbnone
        .selectDynamic("_value")
        .selectDynamic("_value")
        .selectDynamic("_value")
        .selectDynamic("_value")
        .selectDynamic("_value")
    )
  }
}