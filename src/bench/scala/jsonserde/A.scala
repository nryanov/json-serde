package jsonserde

sealed trait A
final case class CC1(f1: String, f2: Long, f3: Map[String, String]) extends A
final case class CC2(f1: String, f2: CC1) extends A
final case class CC3(f1: Long, f2: Option[String], f3: CC2) extends A
