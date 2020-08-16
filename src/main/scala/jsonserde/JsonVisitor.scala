package jsonserde

import jsonserde.antlr.{JSONBaseVisitor, JSONParser}
import org.antlr.v4.runtime.tree.ErrorNode
import scala.jdk.CollectionConverters._

final class JsonVisitor extends JSONBaseVisitor[Json] {
  override def visitErrorNode(node: ErrorNode): Json = throw new RuntimeException("Parse error")

  override def visitJson(ctx: JSONParser.JsonContext): Json = visit(ctx.value())

  override def visitNotEmptyObject(ctx: JSONParser.NotEmptyObjectContext): Json = JsonObj(
    ctx.pair().asScala.map(pair => (removeQuotes(pair.key.getText), visit(pair.value()))).toList
  )

  override def visitEmptyObject(ctx: JSONParser.EmptyObjectContext): Json = JsonObj.EMPTY

  override def visitPair(ctx: JSONParser.PairContext): Json = JsonObj(List((removeQuotes(ctx.key.getText), visit(ctx.value()))))

  override def visitNotEmptyArray(ctx: JSONParser.NotEmptyArrayContext): Json = JsonArray(ctx.value().asScala.map(visit(_)).toVector)

  override def visitEmptyArray(ctx: JSONParser.EmptyArrayContext): Json = JsonArray.EMPTY

  override def visitStringValue(ctx: JSONParser.StringValueContext): Json = JsonString(removeQuotes(ctx.STRING().getSymbol.getText))

  override def visitNumberValue(ctx: JSONParser.NumberValueContext): Json = JsonNumber(BigDecimal(ctx.NUMBER().getSymbol.getText))

  override def visitObjectValue(ctx: JSONParser.ObjectValueContext): Json = visit(ctx.obj())

  override def visitArrayValue(ctx: JSONParser.ArrayValueContext): Json = visit(ctx.arr())

  override def visitTrue(ctx: JSONParser.TrueContext): Json = JsonBoolean(true)

  override def visitFalse(ctx: JSONParser.FalseContext): Json = JsonBoolean(false)

  override def visitNull(ctx: JSONParser.NullContext): Json = JsonNull

  private def removeQuotes(str: String): String =
    str.substring(1, str.length - 1)
}
