package OptionValueAccessor

import OptionValueAccessor._

object OptionValueAccessorTest {

  case class A(value: Int)
  case class B(value: Option[A])
  case class C(value: Option[B])
  case class D(value: Option[C])
  case class E(value: D)

  def main(args: Array[String]): Unit = {
    val edcba = Some(E(D(Some(C(Some(B(Some(A(1)))))))))
    println(edcba.?.?.?.?.?) // Some(1)

    val ednon = Some(E(D(None)))
    println(ednon.?.?.?.?.?) // None
  }
}