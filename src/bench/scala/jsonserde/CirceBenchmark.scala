package jsonserde

import io.circe._
import io.circe.generic.auto._
import io.circe.generic.semiauto._
import io.circe.syntax._
import cats.syntax.functor._
import org.scalameter.api._
import org.scalameter.picklers.Implicits._

object CirceBenchmark extends Bench.LocalTime {
  lazy val circeJson: Json = Json.obj(
    ("f1", Json.fromLong(1)),
    ("f2", Json.fromString("CC3-f2")),
    (
      "f3",
      Json.obj(
        ("f1", Json.fromString("CC2-f1")),
        (
          "f2",
          Json.obj(
            ("f1", Json.fromString("CC1-f1")),
            ("f2", Json.fromLong(2)),
            (
              "f3",
              Json.obj(
                ("a", Json.fromString("v1")),
                ("b", Json.fromString("v2")),
                ("c", Json.fromString("v3"))
              )
            )
          )
        )
      )
    )
  )

  lazy val instance: A = CC3(1, Some("2"), CC2("3", CC1("4", 5, Map("a" -> "v1", "b" -> "v2", "c" -> "v3"))))

  implicit val encodeEvent: Encoder[A] = Encoder.instance {
    case foo: CC1 => foo.asJson
    case bar: CC2 => bar.asJson
    case baz: CC3 => baz.asJson
  }

  implicit val decodeEvent: Decoder[A] =
    List[Decoder[A]](
      Decoder[CC1].widen,
      Decoder[CC2].widen,
      Decoder[CC3].widen
    ).reduceLeft(_.or(_))

  val circeEncoder: Encoder[A] = deriveEncoder[A]
  val circeDecoder: Decoder[A] = deriveDecoder[A]

  val sizes: Gen[Int] = Gen.single("size")(100_000)

  val range: Gen[Range] = for {
    size <- sizes
  } yield 0 until size

  @volatile
  var circeSerializeResult: Json = _
  @volatile
  var circeDeserializeResult: Either[DecodingFailure, A] = _

  performance.of("circe") in {
    measure.method("serialize") in {
      using(range) in { i =>
        i.foreach { _ =>
          circeSerializeResult = circeEncoder(instance)
        }
      }
    }

    measure.method("deserialize") in {
      using(range) in { i =>
        i.foreach { _ =>
          circeDeserializeResult = circeDecoder.decodeJson(circeJson)
        }
      }
    }
  }
}
