package jsonserde

import jsonserde.antlr.{JSONBaseVisitor, JSONParser}
import org.antlr.v4.runtime.tree.ErrorNode
import scala.jdk.CollectionConverters._

final class JsonVisitor extends JSONBaseVisitor[Json] {
  override def visitJson(ctx: JSONParser.JsonContext): Json =
    super.visitJson(ctx)

  override def visitObj(ctx: JSONParser.ObjContext): Json =
    JsonObj(
      ctx
        .pair()
        .asScala
        .map(pair => (removeQuotes(pair.key.getText), visit(pair.value())))
        .toMap
    )

  override def visitPair(ctx: JSONParser.PairContext): Json =
    JsonObj(Map(removeQuotes(ctx.key.getText) -> visit(ctx.value())))

  override def visitArr(ctx: JSONParser.ArrContext): Json =
    JsonArray(ctx.value().asScala.map(visit(_)).toVector)

  override def visitStringValue(ctx: JSONParser.StringValueContext): Json =
    JsonString(removeQuotes(ctx.STRING().getSymbol.getText))

  override def visitNumberValue(ctx: JSONParser.NumberValueContext): Json =
    JsonNumber(BigDecimal(ctx.NUMBER().getSymbol.getText))

  override def visitObjectValue(ctx: JSONParser.ObjectValueContext): Json =
    super.visitObjectValue(ctx)

  override def visitArrayValue(ctx: JSONParser.ArrayValueContext): Json =
    JsonArray(ctx.arr().value().asScala.map(visit(_)).toVector)

  override def visitTrue(ctx: JSONParser.TrueContext): Json = JsonBoolean(true)

  override def visitFalse(ctx: JSONParser.FalseContext): Json =
    JsonBoolean(false)

  override def visitNull(ctx: JSONParser.NullContext): Json = JsonNull

  override def visitErrorNode(node: ErrorNode): Json =
    throw new RuntimeException("Parse error")

  private def removeQuotes(str: String): String =
    str.substring(1, str.length - 1)
}
