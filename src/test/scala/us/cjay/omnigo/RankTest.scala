package us.cjay.omnigo

import org.scalatest._

/**
 * Created by C.Jay on 2/8/2015.
 *
 * Yo!
 */
class RankTest extends FlatSpec with Matchers {
  "A Rank" should "be buildable" in {
    val r = Rank(20, RankClass.Kyu)
    r.level should equal(20)
    r.rankClass should equal(RankClass.Kyu)
  }
  it should "be buildable by string" in {
    var r = Rank("20k")
    r.level should equal(20)
    r.rankClass should equal(RankClass.Kyu)

    r = Rank("3d")
    r.level should equal(3)
    r.rankClass should equal(RankClass.Dan)
  }

  it should "error on a bad rank string" in {
    intercept[MatchError] {
      var r = Rank("20x")
    }

    intercept[MatchError] {
      var r = Rank("wd")
    }
  }
}
