package us.cjay.omnigo

import com.github.nscala_time.time.Imports._


/**
 * Created by C.Jay on 2/8/2015.
 *
 * Yo
 */

/*
object SGFField extends Enumeration {
  type SGFField = Value
  val BlackName,BlackRank,WhiteName,WhiteRank,Komi,Date,Result,Source = Value
  val Unknown = Value
}
import SGFField._




PB[Shin Minjun]       BLACK NAME
BR[2p]                BLACK RANK
PW[Na Hyun]           WHITE NAME
WR[5p]                WHITE RANK
KM[6.5]               KOMI
DT[2015-02-06]        DATE
RE[W+R]               RESULT (B+R = black by resignation, B+3.5 = black by points)
SO[gokifu.com]        SOURCE
BC[]                  ???
WC[kr]                ???

*/

case class SGFHeader(field: String, value: String)

object SGFHeaders {
  def fromTokens(tokens: Array[SGFHeader]): SGFHeaders = {
    var blackName: Option[String] = None
    var blackRank: Option[Rank] = None
    var whiteName: Option[String] = None
    var whiteRank: Option[Rank] = None
    var komi: Option[Double] = None
    var date: Option[DateTime] = None
    var result: Option[String] = None //TODO Result Object
    var source: Option[String] = None
    for (tok <- tokens) {
      tok.field match {
        case "PB" => blackName = Some(tok.value)
        case "BR" => blackRank = Some(Rank(tok.value))
        case "PW" => whiteName = Some(tok.value)
        case "WR" => whiteRank = Some(Rank(tok.value))
        case "KM" => komi = Some(tok.value.toDouble)
        case "DT" => date = Some(DateTime.parse(tok.value))
        case "RE" => result = Some(tok.value)
        case "SO" => source = Some(tok.value)
        case default => ; //TODO: Save unknowns
      }
    }
    new SGFHeaders(blackName, blackRank, whiteName, whiteRank, komi, date, result, source)
  }
}

class SGFHeaders(
                  val blackName: Option[String],
                  val blackRank: Option[Rank],
                  val whiteName: Option[String],
                  val whiteRank: Option[Rank],
                  val komi: Option[Double],
                  val date: Option[DateTime],
                  val result: Option[String],
                  val source: Option[String]) {

}
