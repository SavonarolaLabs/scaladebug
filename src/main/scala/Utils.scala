package ergfi

import scala.io.Source
import io.circe.parser._
import io.circe.Json

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
}
