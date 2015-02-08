package us.cjay.omnigo


object Location {


  def apply(col: Int, row: Int): Location = {
    if(col >= 0 && col < 19 && row >= 0 && row < 19) {
      new Location(col,row)
    }
    else {
      throw new InvalidGoCoordinateException(col,row)
    }
  }

  def apply(coordinate: String): Location = {
    fromCoordinates(coordinate)
  }

  def fromCoordinates(coordinate: String): Location = {
    val re = "([a-tA-T])([0-9][0-9]?)".r
    coordinate match {
      case re(xL, y) => {
        val x = xL.toCharArray()(0).toUpper - 'A'
        new Location(x,y.toInt - 1)
      }
    }
  }

}

class Location private(val col: Int, val row: Int) {

}

