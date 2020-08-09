package jsonserde

object DecoderOps {
  def decode[A](json: Json)(implicit decoder: Decoder[A]): Either[Throwable, A] = decoder.decode(json)
}
