package ergfi

import scala.io.Source
import io.circe.parser._
import io.circe.Json
import org.ergoplatform.appkit.ErgoValue

object Utils {

  def loadJsonFromFile(fileName: String): Json = {
    val source = Source.fromResource(fileName)
    val jsonContent = try source.mkString finally source.close()
    parse(jsonContent) match {
      case Right(json) => json
      case Left(error) => throw new RuntimeException(s"Failed to parse JSON from $fileName: ${error.getMessage}")
    }
  }

  def getTxHeight(tx: Json): Int = {
    tx.hcursor.downField("outputs").downArray.get[Int]("creationHeight").getOrElse(
      throw new RuntimeException("Missing creationHeight")
    )
  }

  def sigmaProp(a: Boolean): Boolean = a

  def PK(a: String): Boolean = false

  def min(a: Int, b: Int): Int = Math.min(a, b)
  def max(a: Int, b: Int): Int = Math.max(a, b)

  /**
   * Takes a register's hex string, returns the underlying object:
   *   - SInt => java.lang.Integer
   *   - SLong => java.lang.Long
   *   - SOption[SInt] => Some(Integer) or None
   *   - SOption[SLong] => Some(Long) or None
   *   etc.
   */
  def parseRegister(reg: String): Any = {
    ErgoValue.fromHex(reg).getValue
  }

  def extractToken(asset: Json): Option[(String, Long)] = {
    for {
      tokenId <- asset.hcursor.downField("tokenId").as[String].toOption
      amount  <- asset.hcursor.downField("amount").as[String].toOption.map(_.toLong)
    } yield (tokenId, amount)
  }

  def extractTokens(json: Json): Int => Option[Map[String, Any]] = { j =>
    val assets = json.hcursor.downField("assets").as[List[Json]].getOrElse(Nil).zipWithIndex.map {
      case (asset, idx) =>
        idx.toString -> extractToken(asset).map {
          case (tokenId, amount) =>
            Map("_1" -> tokenId, "_2" -> amount)
        }.getOrElse(Map.empty)
    }.toMap

    if (j == -1) Some(assets.view.mapValues(_.asInstanceOf[Any]).toMap)
    else assets.get(j.toString).map(_.asInstanceOf[Map[String, Any]])
  }

  def createInputs(tx: Json): Int => Option[Map[String, Any]] = {
    (i: Int) =>
      tx.hcursor.downField("inputs").as[List[Json]] match {
        case Right(inputs) if i >= 0 && i < inputs.length =>
          val input = inputs(i)
          Some(
            Map(
              "value" -> input.hcursor.downField("value").as[String].map(_.toLong).getOrElse(0L),
              "propositionBytes" -> input.hcursor.downField("ergoTree").as[String].getOrElse(""),
              "tokens" -> ((j: Int) => {
                val assets = input.hcursor.downField("assets").as[List[Json]].getOrElse(Nil).zipWithIndex.map {
                  case (asset, idx) =>
                    idx.toString -> Map(
                      "_1" -> asset.hcursor.downField("tokenId").as[String].getOrElse(""),
                      "_2" -> asset.hcursor.downField("amount").as[String].map(_.toLong).getOrElse(0L)
                    )
                }.toMap
                if (j == -1) Some(assets.view.mapValues(_.asInstanceOf[Any]).toMap)
                else assets.get(j.toString)
              })
            ) ++ input.hcursor.downField("additionalRegisters").as[Map[String, String]].getOrElse(Map.empty).view
              .mapValues(parseRegister).toMap
          )
        case _ => None
      }
  }

  def createOutputs(tx: Json): Int => Option[Map[String, Any]] = {
    (i: Int) =>
      tx.hcursor.downField("outputs").as[List[Json]] match {
        case Right(outputs) if i >= 0 && i < outputs.length =>
          val output = outputs(i)
          Some(
            Map(
              "value" -> output.hcursor.downField("value").as[String].map(_.toLong).getOrElse(0L),
              "propositionBytes" -> output.hcursor.downField("ergoTree").as[String].getOrElse(""),
              "tokens" -> ((j: Int) => {
                val assets = output.hcursor.downField("assets").as[List[Json]].getOrElse(Nil).zipWithIndex.map {
                  case (asset, idx) =>
                    idx.toString -> Map(
                      "_1" -> asset.hcursor.downField("tokenId").as[String].getOrElse(""),
                      "_2" -> asset.hcursor.downField("amount").as[String].map(_.toLong).getOrElse(0L)
                    )
                }.toMap
                if (j == -1) Some(assets.view.mapValues(_.asInstanceOf[Any]).toMap)
                else assets.get(j.toString)
              })
            ) ++ output.hcursor.downField("additionalRegisters").as[Map[String, String]].getOrElse(Map.empty).view
              .mapValues(parseRegister).toMap
          )
        case _ => None
      }
  }

  def createContext(tx: Json): Map[String, Int => Option[Map[String, Any]]] = {
    Map(
      "dataInputs" -> ((i: Int) =>
        tx.hcursor.downField("dataInputs").as[List[Json]] match {
          case Right(dataInputs) if i >= 0 && i < dataInputs.length =>
            val dataInput = dataInputs(i)
            Some(
              Map(
                "value" -> dataInput.hcursor.downField("value").as[String].map(_.toLong).getOrElse(0L),
                "propositionBytes" -> dataInput.hcursor.downField("ergoTree").as[String].getOrElse(""),
                "tokens" -> ((j: Option[Int]) => {
                  val assets = dataInput.hcursor.downField("assets").as[List[Json]].getOrElse(Nil).zipWithIndex.map {
                    case (asset, idx) =>
                      idx.toString -> Map(
                        "_1" -> asset.hcursor.downField("tokenId").as[String].getOrElse(""),
                        "_2" -> asset.hcursor.downField("amount").as[String].map(_.toLong).getOrElse(0L)
                      )
                  }.toMap
                  j.map(_.toString).flatMap(assets.get).getOrElse(assets)
                })
              ) ++ dataInput.hcursor.downField("additionalRegisters").as[Map[String, String]].getOrElse(Map.empty).view
                .mapValues(parseRegister).toMap
            )
          case _ => None
        }
        )
    )
  }
}
