package jsonserde

object EncoderOps {
  def encode[A](value: A)(implicit encoder: Encoder[A]): Json = encoder.encode(value)
}
