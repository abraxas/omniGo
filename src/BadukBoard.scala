/**
 * Created by User on 1/18/2015.
 */
import Piece._

class BadukCaptures(b:Int,w:Int) {
  var black = b
  var white = w
  def this() = this(0,0)

  override def clone(): BadukCaptures = new BadukCaptures(black,white)
}

class BadukBoard(previous_board: Array[Array[Option[Piece]]],previousCaptures: BadukCaptures) {
  private var board = previous_board
  private var captures = previousCaptures

  def this() = this(Array.fill[Option[Piece]](19,19)(None),new BadukCaptures)
  override def clone(): BadukBoard = new BadukBoard(board.clone(),captures.clone())

  def set(x: Int, y: Int, piece: Piece): Unit = this.set(x,y,Some(piece))
  def set(x: Int, y: Int, piece: Option[Piece]): Unit = board(x).update(y,piece)

  def get(x: Int, y: Int): Option[Piece] = board(x)(y)

  override def toString: String = {
    var rval = ""
    board.foreach((row: Array[Option[Piece]]) => {
      var rvalRow : Array[String] =  row.map((o: Option[Piece]) => {
        println("MATCHY")
        println(o)
        o match {
          case Some(x) => if(Piece.Black.equals(x)) "B" else "W"
          case None => "x"
        }
      })
      rval += rvalRow.mkString(",") + "\n"
    })
    rval
  }
}
