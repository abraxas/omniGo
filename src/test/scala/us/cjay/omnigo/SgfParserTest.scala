package us.cjay.omnigo

import org.scalatest._

import scala.util.parsing.input.CharSequenceReader

/**
 * Created by C.Jay on 2/8/2015.
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
    describe("Move Parser") {
      it("should be able to parse a simple move") {
        var input = new CharSequenceReader("W[aq]")
        var x = SgfParsers.move(input)
        var p = x.get
        p.location.col should equal(0)
        p.location.row should equal(16)
        p.piece should equal(Piece.White)

        input = new CharSequenceReader("B[dd]")
        x = SgfParsers.move(input)
        p = x.get
        p.location.col should equal(3)
        p.location.row should equal(3)
        p.piece should equal(Piece.Black)
      }
      it("should fail to parse a bad color") {
        val input = new CharSequenceReader("C[aq]")
        val x = SgfParsers.move(input)
        x shouldBe 'isEmpty
      }
      it("should fail to parse bad coordinates") {
        var input = new CharSequenceReader("B[aw]")
        var x = SgfParsers.move(input)
        x shouldBe 'isEmpty

        input = new CharSequenceReader("B[wa]")
        x = SgfParsers.move(input)
        x shouldBe 'isEmpty

        input = new CharSequenceReader("W[a2]")
        x = SgfParsers.move(input)
        x shouldBe 'isEmpty

        input = new CharSequenceReader("W[2a]")
        x = SgfParsers.move(input)
        x shouldBe 'isEmpty
      }
      it("should be able to parse a move with simple comment") {
        var input = new CharSequenceReader("W[aq]C[test comment]")
        var x = SgfParsers.move(input)
        var p = x.get
        p.location.col should equal(0)
        p.location.row should equal(16)
        p.piece should equal(Piece.White)
        p.comment should equal(Some("test comment"))
      }
    }
    describe("Moves Parser") {
      it("should be able to parse 2 simple moves") {
        val input = new CharSequenceReader(";W[aq];B[dd]")
        val result = SgfParsers.moves(input)
        val moves = result.get
        val firstMove = moves(0)
        firstMove.location.col should equal(0)
        firstMove.location.row should equal(16)
        firstMove.piece should equal(Piece.White)
        val secondMove = moves(1)
        secondMove.location.col should equal(3)
        secondMove.location.row should equal(3)
        secondMove.piece should equal(Piece.Black)
      }
      it("should be able to parse many moves") {
        val input = new CharSequenceReader(";W[aq];B[dd];W[aq];B[dd];W[aq];B[dd];W[aq];B[dd];W[aq];B[dd]")
        val result = SgfParsers.moves(input)
        val moves = result.get
        moves.length should equal(10)
      }
      it("should fail to parse a bad color") {
        val input = new CharSequenceReader(";W[aq];B[dd];W[aq];C[dd];W[aq];B[dd];W[aq];B[dd];W[aq];B[dd]")
        val x = SgfParsers.move(input)
        x shouldBe 'isEmpty
      }
      it("should fail to parse a bad coordinate") {
        val input = new CharSequenceReader(";W[aq];B[dd];W[aq];B[d2];W[aq];B[dd];W[aq];B[dd];W[aq];B[dd]")
        val x = SgfParsers.move(input)
        x shouldBe 'isEmpty
      }
    }
  }


}
