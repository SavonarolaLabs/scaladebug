package ergfi

import Utils._

object Main {
  def main(args: Array[String]): Unit = {
    val tx = loadJsonFromFile("freemintTx.json")
    val height = getTxHeight(tx)
    assert(height == 1449119, s"Invalid transaction height: $height")
  }
}
