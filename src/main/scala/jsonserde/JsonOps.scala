package jsonserde

import jsonserde.antlr.{JSONLexer, JSONParser}
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}

import scala.util.Try

object JsonOps {
  private lazy val jsonVisitor: JsonVisitor = new JsonVisitor

  def parse(str: String): Either[Throwable, Json] = {
    val lexer = new JSONLexer(CharStreams.fromString(str))
    val tokenStream = new CommonTokenStream(lexer)
    val parser = new JSONParser(tokenStream)

    Try(jsonVisitor.visit(parser.json())).toEither
  }
}
