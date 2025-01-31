package ergfi

import Utils._

object Main {
  def main(args: Array[String]): Unit = {
    val tx = loadJsonFromFile("freemintTx.json")

    val HEIGHT = getTxHeight(tx)
    val CONTEXT = createContext(tx)
    val INPUTS = createInputs(tx)
    val OUTPUTS = createOutputs(tx)
    val SELF = INPUTS(0).get

    val bankInIndex = 1
    val buybackInIndex = 2

    val selfOutIndex = 0
    val bankOutIndex = 1
    val buybackOutIndex = 2

    val oracleBoxIndex = 0
    val lpBoxIndex = 1

    val bankNFT = "a033c16089312f77d7724ae6fd22ff5f2524a7d684fdd2f6f3f94132bbb30784"
    val buybackNft = "109dfaf60489985fc43fbbf3a49cc2f41eedc33f7b01370122c69cf4aeb58272"

    val oracleNFT = "e38048c74cb92bb2f908c2465106f7ab2f2632fbbbb72a26c372276263b2b011"
    val lpNFT = "323bf7f5cfcc33f3e4f1bd559113e46592139835b64bfe02aa810658980cb50c"

    val T_free = 360
    val T_buffer = 5

    val bankFeeNum = 3
    val buybackFeeNum = 2
    val feeDenom = 1000

    val oracleBox = CONTEXT("dataInputs")(oracleBoxIndex).get
    val lpBox = CONTEXT("dataInputs")(lpBoxIndex).get

    val bankBoxIn = INPUTS(bankInIndex).get
    val buybackBoxIn = INPUTS(buybackInIndex).get

    val successor = OUTPUTS(selfOutIndex).get
    val bankBoxOut = OUTPUTS(bankOutIndex).get
    val buybackOut = OUTPUTS(buybackOutIndex).get

    val selfInR4 = SELF("R4")
    val selfInR5 = SELF("R5")
    val successorR4 = successor("R4")
    val successorR5 = successor("R5")

    val isCounterReset = HEIGHT > selfInR4.asInstanceOf[Long]

    val oracleRate = oracleBox("R4").asInstanceOf[Long] / 1000000.0

    val lpReservesX = lpBox("value").asInstanceOf[Long]
    val lpReservesY = lpBox("tokens").asInstanceOf[Function1[Option[Int], Map[String, Any]]](Some(2))
      .get("_2").asInstanceOf[Long]
    val lpRate = lpReservesX.toDouble / lpReservesY.toDouble

    val validRateFreeMint = lpRate * 100 > oracleRate * 98

    val dexyMinted = bankBoxIn("tokens").asInstanceOf[Function1[Option[Int], Map[String, Any]]](Some(1))
      .get("_2").asInstanceOf[Long] -
      bankBoxOut("tokens").asInstanceOf[Function1[Option[Int], Map[String, Any]]](Some(1))
        .get("_2").asInstanceOf[Long]

    val ergsAdded = bankBoxOut("value").asInstanceOf[Long] - bankBoxIn("value").asInstanceOf[Long]
    val bankRate = Math.floor((oracleRate * (bankFeeNum + feeDenom)) / feeDenom).toLong
    val validBankDelta = ergsAdded >= dexyMinted * bankRate && ergsAdded > 0

    val buybackErgsAdded = buybackOut("value").asInstanceOf[Long] - buybackBoxIn("value").asInstanceOf[Long]
    val buybackRate = Math.floor((oracleRate * buybackFeeNum) / feeDenom).toLong
    val validBuybackDelta = buybackErgsAdded >= dexyMinted * buybackRate && buybackErgsAdded > 0
    val validDelta = validBankDelta && validBuybackDelta

    val maxAllowedIfReset = lpReservesY / 100
    val availableToMint = if (isCounterReset) maxAllowedIfReset else selfInR5.asInstanceOf[Long]

    val validAmount = dexyMinted <= availableToMint

    val validSuccessorR4 = if (!isCounterReset) {
      successorR4 == selfInR4
    } else {
      successorR4.asInstanceOf[Long] >= HEIGHT + T_free &&
        successorR4.asInstanceOf[Long] <= HEIGHT + T_free + T_buffer
    }

    val validSuccessorR5 = successorR5.asInstanceOf[Long] == availableToMint - dexyMinted

    val validBankBoxInOut = bankBoxIn("tokens").asInstanceOf[Function1[Option[Int], Map[String, Any]]](Some(0))
      .get("_1") == bankNFT

    val validBuyBackIn = buybackBoxIn("tokens").asInstanceOf[Function1[Option[Int], Map[String, Any]]](Some(0))
      .get("_1") == buybackNft

    val validLpBox = lpBox("tokens").asInstanceOf[Function1[Option[Int], Map[String, Any]]](Some(0))
      .get("_1") == lpNFT

    val validOracleBox = oracleBox("tokens").asInstanceOf[Function1[Option[Int], Map[String, Any]]](Some(0))
      .get("_1") == oracleNFT

    val validSuccessor =
      successor("tokens") == SELF("tokens") &&
        successor("propositionBytes") == SELF("propositionBytes") &&
        successor("value").asInstanceOf[Long] >= SELF("value").asInstanceOf[Long] &&
        validSuccessorR5 &&
        validSuccessorR4

    val contract = sigmaProp(
      validAmount &&
        validBankBoxInOut &&
        validLpBox &&
        validOracleBox &&
        validBuyBackIn &&
        validSuccessor &&
        validDelta &&
        validRateFreeMint
    ) || PK("9gJa6Mict6TVu9yipUX5aRUW87Yv8J62bbPEtkTje28sh5i3Lz8")

    println(contract)
  }
}
