package us.cjay.omnigo

import us.cjay.omnigo.Piece._

/**
 * Created by User on 2/8/2015.
 */
class Move(val location: Location, val piece: Piece, val comment: Option[String] = None) {
  //override def equals(other: Any): Boolean = {
  //other match {
  //case foo: Move =>
  //foo.location==location && foo.piece==piece &&
  //}
  //false

  //}
}

object Move {
  def apply(loc: Location, piece: Piece): Move = {
    new Move(loc,piece)
  }
  def apply(locString: String, piece: Piece): Move = {
    new Move(Location(locString),piece)
  }


  def apply(color: String, loc1: String, loc2: String, comment: Option[String] = None): Move = {
    val piece = Piece(color)
    piece match {
      case Some(p) =>
        new Move(Location(loc1,loc2),p,comment)
      case None =>
        throw new UnknownError(s"Piece Expected B or W but got $piece")
    }
  }
}

//type Mover = Either[Move,MoveList]

class MoveList(val get: List[Move])

object MoveList {

  def apply(get: List[Move]): MoveList = {
    new MoveList(get)
  }

  def apply(get: Array[Move]): MoveList = {
    new MoveList(get.toList)
  }

}

class MoveNode(get: Move, parent: MoveNode, children: Array[MoveNode] = Array.empty)

class MoveTree(root: MoveNode) {

}

/*object MoveTree {
  apply
}*/
