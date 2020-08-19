package jsonserde

import org.scalameter.api._
import org.scalameter.picklers.Implicits._
import Encoder._
import Decoder._

object Benchmark extends Bench.LocalTime {
  lazy val json: Json = JsonObj(
    List(
      ("f1", JsonNumber(1)),
      ("f2", JsonString("CC3-f2")),
      (
        "f3",
        JsonObj(
          List(
            ("f1", JsonString("CC2-f1")),
            (
              "f2",
              JsonObj(
                List(
                  ("f1", JsonString("CC1-f1")),
                  ("f2", JsonNumber(2)),
                  (
                    "f3",
                    JsonObj(
                      List(
                        ("a", JsonString("v1")),
                        ("b", JsonString("v2")),
                        ("c", JsonString("v3"))
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )

  lazy val instance: A = CC3(1, Some("2"), CC2("3", CC1("4", 5, Map("a" -> "v1", "b" -> "v2", "c" -> "v3"))))

  val sizes: Gen[Int] = Gen.single("size")(100_000)

  val range: Gen[Range] = for {
    size <- sizes
  } yield 0 until size

  val jsonserdeEncoder: Encoder[A] = Encoder[A]
  val jsonserdeDecoder: Decoder[A] = Decoder[A]
  @volatile
  var jsonserdeSerializeResult: Json = _
  @volatile
  var jsonserdeDeserializeResult: Either[Throwable, A] = _

  performance.of("jsonserde") in {
    measure.method("serialize") in {
      using(range) in { i =>
        i.foreach { _ =>
          jsonserdeSerializeResult = jsonserdeEncoder.encode(instance)
        }
      }
    }

    measure.method("deserialize") in {
      using(range) in { i =>
        i.foreach { _ =>
          jsonserdeDeserializeResult = jsonserdeDecoder.decode(json)
        }
      }
    }
  }
}
