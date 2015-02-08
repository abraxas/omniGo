package us.cjay.omnigo

import org.scalatest._

/**
 * Created by C.Jay on 2/7/2015.
 */
class LocationTest extends FlatSpec with Matchers {
  "A Location" should "be buildable" in {
    val x = Location(4, 3)
    x.col should equal(4)
    x.row should equal(3)
  }
  "A Location" should "answer to fromCoordinates properly" in {
    val x = Location.fromCoordinates("K4")
    x.col should equal(10)
    x.row should equal(3)

    val y = Location.fromCoordinates("k4")
    y.col should equal(10)
    y.row should equal(3)
  }
  "A Location" should "answer to apply by coordinate" in {
    val z = Location("d4")
    z.col should equal(3)
    z.row should equal(3)

    val z2 = Location("D4")
    z2.col should equal(3)
    z2.row should equal(3)
  }

}
