package jsonserde.ops

import jsonserde.{Decoder, Json}

object DecoderOps {
  def decode[A](json: Json)(implicit decoder: Decoder[A]): Either[Throwable, A] = decoder.decode(json)
}
