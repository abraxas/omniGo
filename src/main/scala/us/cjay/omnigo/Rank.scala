package us.cjay.omnigo

/**
 * Created by User on 2/8/2015.
 */

object RankClass extends Enumeration {
  type RankClass = Value
  val Kyu, Dan, Pro = Value
}

import us.cjay.omnigo.RankClass._

object Rank {
  def apply(level: Integer, rankClass: RankClass): Rank = {
    new Rank(level, rankClass)
  }

  def apply(rankString: String): Rank = {
    val re = """(\d+)\s*([kKdDPp])""".r

    rankString match {
      case re(levelStr, cl) =>
        val rankClass = cl.toUpperCase match {
          case "K" => Kyu
          case "D" => Dan
          case "P" => Pro
        }
        val level = levelStr.toInt
        new Rank(level, rankClass)
    }
  }
}

class Rank private(val level: Integer, val rankClass: RankClass) {

}
