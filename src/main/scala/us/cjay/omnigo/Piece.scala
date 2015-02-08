package us.cjay.omnigo

/**
 * Created by User on 1/18/2015.
 */
object Piece extends Enumeration {
  type Piece = Value
  val Black, White = Value

  def apply(color: String): Option[Piece] = {
    color match {
      case "W" => Some(White)
      case "B" => Some(Black)
      case _ => None
    }
  }
}