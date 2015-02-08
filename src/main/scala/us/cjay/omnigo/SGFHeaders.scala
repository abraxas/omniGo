package us.cjay.omnigo

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
*/

case class SGFHeader(field: String, value: String)

class SGFHeaders {

}
