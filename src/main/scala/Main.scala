package ergfi

import io.circe._
import io.circe.generic.auto._
import io.circe.parser._
import org.ergoplatform.appkit.ErgoValue
import scala.io.Source
import BoxHelpers._
import ergfi.Utils.loadJsonFromFile

case class Asset(tokenId: String, amount: Long)

case class Box(
                value: String,
                ergoTree: String,
                assets: List[Asset],
                additionalRegisters: Map[String, String],
                creationHeight: Option[Int]
              )

case class Tx(
               inputs: List[Box],
               dataInputs: List[Box],
               outputs: List[Box]
             )

object BoxHelpers {
  def parseRegister(hex: String): Any = {
    val ev = ErgoValue.fromHex(hex)
    ev.getValue
  }

  implicit class RichBox(val box: Box) extends AnyVal {
    def valueNum: Long =
      box.value.toLongOption.getOrElse(0L)

    def R4: Long = {
      box.additionalRegisters.get("R4") match {
        case Some(hex) =>
          parseRegister(hex) match {
            case n: java.lang.Number => n.longValue()
            case other => sys.error(s"R4 not numeric: $other")
          }
        case None => 0L
      }
    }

    def R5: Long = {
      box.additionalRegisters.get("R5") match {
        case Some(hex) =>
          parseRegister(hex) match {
            case n: java.lang.Number => n.longValue()
            case other => sys.error(s"R5 not numeric: $other")
          }
        case None => 0L
      }
    }

    def tokens(i: Int): (String, Long) = {
      val asset = box.assets.lift(i).getOrElse(
        sys.error(s"Asset index $i out of range; we have ${box.assets.size} tokens.")
      )
      (asset.tokenId, asset.amount.toLong)
    }

    def tokens(): List[(String, Long)] =
      box.assets.map(a => (a.tokenId, a.amount.toLong))
  }
}

object Freemint extends App {
  def sigmaProp(a: Boolean) = a
  def PK(addr: String) = false

  val rawJson = loadJsonFromFile("freemintTx.json")
  val tx: Tx = {
    val parsed = rawJson.as[Tx]
    parsed.getOrElse(sys.error("Could not decode freemintTx.json into Tx."))
  }

  val HEIGHT = tx.outputs.headOption.map(_.creationHeight.getOrElse(0)).getOrElse(0)
  val context = tx.dataInputs
  val inputs = tx.inputs
  val outputs = tx.outputs
  val SELF = inputs(0)

  var bankInIndex = 1
  var buybackInIndex = 2
  var selfOutIndex = 0
  var bankOutIndex = 1
  var buybackOutIndex = 2
  var oracleBoxIndex = 0
  var lpBoxIndex = 1

  var bankNFT = "a033c16089312f77d7724ae6fd22ff5f2524a7d684fdd2f6f3f94132bbb30784"
  var buybackNFT = "109dfaf60489985fc43fbbf3a49cc2f41eedc33f7b01370122c69cf4aeb58272"
  var oracleNFT = "e38048c74cb92bb2f908c2465106f7ab2f2632fbbbb72a26c372276263b2b011"
  var lpNFT = "323bf7f5cfcc33f3e4f1bd559113e46592139835b64bfe02aa810658980cb50c"

  val T_free = 360
  val T_buffer = 5

  val bankFeeNum = 3
  val buybackFeeNum = 2
  val feeDenom = 1000

  val oracleBox = context(oracleBoxIndex)
  val lpBox = context(lpBoxIndex)

  val bankBoxIn = inputs(bankInIndex)
  val buybackBoxIn = inputs(buybackInIndex)

  val successor = outputs(selfOutIndex)
  val bankBoxOut = outputs(bankOutIndex)
  val buybackOut = outputs(buybackOutIndex)

  val selfInR4 = SELF.R4.toInt
  val selfInR5 = SELF.R5
  val successorR4 = successor.R4.toInt
  val successorR5 = successor.R5

  val isCounterReset = HEIGHT > selfInR4

  val oracleRate = oracleBox.R4 / 1_000_000.0

  val lpReservesX = lpBox.valueNum
  val lpReservesY = lpBox.tokens(2)._2
  val lpRate = lpReservesX / lpReservesY

  val validRateFreeMint = lpRate * 100 > oracleRate * 98

  val dexyMinted =
    bankBoxIn.tokens(1)._2 -
      bankBoxOut.tokens(1)._2

  val ergsAdded =
    bankBoxOut.valueNum -
      bankBoxIn.valueNum

  val bankRate = math.floor(oracleRate * (bankFeeNum + feeDenom) / feeDenom).toLong
  val validBankDelta = ergsAdded >= dexyMinted * bankRate && ergsAdded > 0

  val buybackErgsAdded =
    buybackOut.valueNum -
      buybackBoxIn.valueNum

  val buybackRate = math.floor(oracleRate * buybackFeeNum / feeDenom).toLong
  val validBuybackDelta = buybackErgsAdded >= dexyMinted * buybackRate && buybackErgsAdded > 0
  val validDelta = validBankDelta && validBuybackDelta

  val maxAllowedIfReset = lpReservesY / 100
  val availableToMint = if (isCounterReset) maxAllowedIfReset else selfInR5
  val validAmount = dexyMinted <= availableToMint

  val validSuccessorR4 =
    if (!isCounterReset) {
      successorR4 == selfInR4
    } else {
      successorR4 >= HEIGHT + T_free &&
        successorR4 <= HEIGHT + T_free + T_buffer
    }

  val validSuccessorR5 = successorR5 == (availableToMint - dexyMinted)

  val validBankBoxInOut =
    bankBoxIn.tokens(0)._1 == bankNFT

  val validBuyBackIn =
    buybackBoxIn.tokens(0)._1 == buybackNFT

  val validLpBox =
    lpBox.tokens(0)._1 == lpNFT

  val validOracleBox =
    oracleBox.tokens(0)._1 == oracleNFT

  val validSuccessor =
    (successor.tokens().toString == SELF.tokens().toString) &&
      (successor.ergoTree == SELF.ergoTree) &&
      (successor.valueNum >= SELF.valueNum) &&
      validSuccessorR5 &&
      validSuccessorR4

  val contract =
    sigmaProp(
      validAmount &&
        validBankBoxInOut &&
        validLpBox &&
        validOracleBox &&
        validBuyBackIn &&
        validSuccessor &&
        validDelta &&
        validRateFreeMint
    ) || PK("9gJa6Mict6TVu9yipUX5aRUW87Yv8J62bbPEtkTje28sh5i3Lz8")

  println("Contract evaluation => " + contract)
}
