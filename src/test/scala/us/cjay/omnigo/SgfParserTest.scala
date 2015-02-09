package us.cjay.omnigo

import com.github.nscala_time.time.Imports._
import org.scalatest._

import scala.util.parsing.input.CharSequenceReader


/**
 * Created by C.Jay on 2/8/2015.
 *
 * Yo
 */
class SgfParserTest extends FunSpec with Matchers {

  describe("SgfParser") {
    describe("Comment Parser") {
      it("should work on a simple comment") {
        var input = new CharSequenceReader("C[test]")
        var x = SgfParsers.comment(input)
        var p = x.get
        p should equal("test")
      }
    }
    describe("Header Parser") {
      it("should be able to parse a simple header") {
        var input = new CharSequenceReader("PC[OGS: http://online-go.com/game/1092711]")
        var x = SgfParsers.header(input)
        var p = x.get
        p.field should equal("PC")
        p.value should equal("OGS: http://online-go.com/game/1092711")

        input = new CharSequenceReader("PW[abraxas]")
        x = SgfParsers.header(input)
        p = x.get
        p.field should equal("PW")
        p.value should equal("abraxas")
      }
      it("should fail to parse a bad header") {
        val input = new CharSequenceReader("PC[bad header")
        val x = SgfParsers.header(input)
        x shouldBe 'isEmpty
      }
      it("should parse a header with newline") {
        var input = new CharSequenceReader("PC[OGS:\nhttp://online-go.com/game/1092711]")
        var x = SgfParsers.header(input)
        var p = x.get
        p.field should equal("PC")
        p.value should equal("OGS:\nhttp://online-go.com/game/1092711")
      }
    }
    describe("Headers Parser") {
      it("should be able to parse simple headers") {
        var input = new CharSequenceReader(";FF[4]GM[1]SZ[19]CA[UTF-8]SO[gokifu.com]BC[]WC[kr]EV[]PB[Shin Minjun]BR[2p]PW[Na Hyun]WR[5p]KM[6.5]DT[2015-02-06]RE[W+R]")
        var x = SgfParsers.headers(input)
        var p = x.get
        p.blackName.get should equal("Shin Minjun")
        p.blackRank.get.level should equal(2)
        p.blackRank.get.rankClass should equal(RankClass.Pro)
        p.whiteName.get should equal("Na Hyun")
        p.whiteRank.get.level should equal(5)
        p.whiteRank.get.rankClass should equal(RankClass.Pro)
        p.komi.get should equal(6.5)
        p.date.get should equal(DateTime.parse("2015-02-06"))
        p.result.get should equal("W+R")
        p.source.get should equal("gokifu.com")
      }
    }
    describe("Move Parser") {
      it("should be able to parse a simple move") {
        var input = new CharSequenceReader(";W[aq]")
        var x = SgfParsers.move(input)
        var p = x.get
        p.location.col should equal(0)
        p.location.row should equal(16)
        p.piece should equal(Piece.White)

        input = new CharSequenceReader(";B[dd]")
        x = SgfParsers.move(input)
        p = x.get
        p.location.col should equal(3)
        p.location.row should equal(3)
        p.piece should equal(Piece.Black)
      }
      it("should fail to parse a bad color") {
        val input = new CharSequenceReader(";C[aq]")
        val x = SgfParsers.move(input)
        x shouldBe 'isEmpty
      }
      it("should fail to parse bad coordinates") {
        var input = new CharSequenceReader(";B[aw]")
        var x = SgfParsers.move(input)
        x shouldBe 'isEmpty

        input = new CharSequenceReader(";B[wa]")
        x = SgfParsers.move(input)
        x shouldBe 'isEmpty

        input = new CharSequenceReader(";W[a2]")
        x = SgfParsers.move(input)
        x shouldBe 'isEmpty

        input = new CharSequenceReader(";W[2a]")
        x = SgfParsers.move(input)
        x shouldBe 'isEmpty

        input = new CharSequenceReader("W[aa]")
        x = SgfParsers.move(input)
        x shouldBe 'isEmpty
      }
      it("should be able to parse a move with simple comment") {
        var input = new CharSequenceReader(";W[aq]C[test comment]")
        var x = SgfParsers.move(input)
        var p = x.get
        p.location.col should equal(0)
        p.location.row should equal(16)
        p.piece should equal(Piece.White)
        p.comment should equal(Some("test comment"))
      }
      it("should be able to parse a move with whitespace") {
        var input = new CharSequenceReader(" ;W[aq] ")
        var x = SgfParsers.move(input)
        var p = x.get
        p.location.col should equal(0)
        p.location.row should equal(16)
        p.piece should equal(Piece.White)
      }
    }
    describe("Mover Parser") {
      it("should return a MoveList as left") {
        val b = SgfParsers.mover(new CharSequenceReader("(;W[aq];B[aa])"))
        b.get shouldBe 'isLeft
        val l = b.get.left.get.get
        l.length should equal(2)
        val m = l(0)
        m.piece should equal(Piece.White)
        m.location.col should equal(0)
        m.location.row should equal(16)

      }
      it("should return a Move as right") {
        val a = SgfParsers.mover(new CharSequenceReader(";W[aq]"))
        a.get shouldBe 'isRight
        val m = a.get.right.get
        m.piece should equal(Piece.White)
        m.location.col should equal(0)
        m.location.row should equal(16)
      }
    }
    describe("Moves Parser") {
      it("should be able to parse 2 simple moves") {
        val input = new CharSequenceReader(";W[aq];B[dd]")
        val result = SgfParsers.moves(input)
        val moves = result.get
        val firstMove = moves(0)

        firstMove.right.get.location.col should equal(0)
        firstMove.right.get.location.row should equal(16)
        firstMove.right.get.piece should equal(Piece.White)
        val secondMove = moves(1)
        secondMove.right.get.location.col should equal(3)
        secondMove.right.get.location.row should equal(3)
        secondMove.right.get.piece should equal(Piece.Black)
      }
      it("should be able to parse many moves") {
        val input = new CharSequenceReader(";W[aq];B[dd];W[aq];B[dd];W[aq];B[dd];W[aq];B[dd];W[aq];B[dd]")
        val result = SgfParsers.moves(input)
        val moves = result.get
        moves.length should equal(10)
      }
      it("should fail to parse a bad color") {
        val input = new CharSequenceReader(";W[aq];B[dd];W[aq];C[dd];W[aq];B[dd];W[aq];B[dd];W[aq];B[dd]")
        val x = SgfParsers.moves(input)
        x.get.length should equal(3)
        x.next should not be 'atEnd
      }
      it("should fail to parse a bad coordinate") {
        val input = new CharSequenceReader(";W[aq];B[dd];W[aq];B[d2];W[aq];B[dd];W[aq];B[dd];W[aq];B[dd]")
        val x = SgfParsers.moves(input)
        x.get.length should equal(3)
        x.next should not be 'atEnd

      }
      it("should be able to parse moves with spacing") {
        val input = new CharSequenceReader(";W[aq] ;B[dd]")
        val result = SgfParsers.moves(input)
        val moves = result.get
        moves.length should equal(2)
      }
      it("should be able to parse moves with newlines") {
        val input = new CharSequenceReader(";W[aq]\n;B[dd]")
        val result = SgfParsers.moves(input)
        val moves = result.get
        moves.length should equal(2)
      }
    }

  }


}
