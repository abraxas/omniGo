package us.cjay.omnigo

import scala.util.parsing.combinator._

/**
 * Created by User on 2/8/2015.
 *
(;FF[4]GM[1]SZ[19]CA[UTF-8]AP[SGFC:1.17]

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

;B[qd];W[dd];B[pp];W[dq];B[nc];W[qn];B[nq];W[rp];B[ql]
;W[qq];B[qo];W[ro];B[pn];W[pm];B[qm];W[rn];B[om];W[hp];B[pj]
;W[fc];B[cn];W[cp];B[dj];W[ch];B[en];W[ei];B[ej];W[fi];B[fj]
;W[gi];B[jd];W[hc];B[jq];W[gj];B[bi];W[bh];B[cc];W[be];B[ib]
;W[he];B[gk];W[hk];B[hl];W[gl];B[fk];W[ik];B[il];W[jl];B[jm]
;W[jk];B[km];W[jp];B[iq];W[hn];B[gm];W[hq];B[jf];W[od];B[nd]
;W[pd];B[qe];W[pf];B[qf];W[qc];B[rc];W[oc];B[pb];W[ob];B[nf]
;W[qb];B[rb];W[nb];B[mb];W[pa];B[na];W[ng];B[ne];W[of];B[fq]
;W[fp];B[og];W[pg];B[oh];W[qg];B[ep];W[eq];B[fr];W[eo];B[rg]
;W[rh];B[sf];W[pi];B[qi];W[oi];B[nh];W[ph];B[fo];W[gp];B[do]
;W[gr];B[eb];W[fb];B[ec];W[ed];B[pr];W[qr];B[mi];W[pk];B[qj]
;W[oj];B[is];W[hr];B[ok];W[nk];B[mk];W[ol];B[pl];W[qh];B[qk]
;W[ra];B[sd];W[mj];B[lj];W[lk];B[ml];W[li];B[nj];W[ni];B[mh]
;W[mj];B[kj];W[ll];B[nj];W[re];B[sb];W[mj];B[qs];W[rs];B[nj]
;W[rf];B[sg];W[mj];B[cq];W[bq];B[nj];W[sh];B[se];W[mj];B[cr]
;W[dp];B[nj];W[pe];B[rd];W[mj];B[bp];W[bo];B[nj];W[sk];B[sl]
;W[mj];B[ap];W[ao];B[nj];W[rl];B[sj];W[mj];B[co];W[lc];B[er]
;W[eo];B[mc];W[nj];B[ep];W[aq];B[bp];W[eo];B[mm];W[ki];B[ep]
;W[ap];B[dr];W[eo];B[lh];W[kh];B[ep];W[lg];B[bp];W[le];B[ld]
;W[kd];B[me];W[kc];B[lf];W[ke];B[mg];W[kg];B[kf];W[je];B[bd]
;W[ea];B[bb];W[ca];B[ce];W[bf];B[ba];W[ac];B[fd];W[da];B[ad]
;W[ab];B[hb];W[jc];B[ic];W[fe];B[lm];W[kk])
 */
object SgfParsers extends RegexParsers {
  def comment: Parser[String] = "C[" ~> unquotedString <~ "]"

  def headers: Parser[SGFHeaders] = ";" ~> repsep(header, "") ^^ { headerList =>
    SGFHeaders.fromTokens(headerList.toArray)
  }

  def header: Parser[SGFHeader] = field ~ "[" ~ unquotedString ~ "]" ^^ {
    case field ~ "[" ~ value ~ "]" => SGFHeader(field, value)
  }

  private def field: Parser[String] = """\w+""".r

  private def unquotedString: Parser[String] = """(?:[^\]]||\\\])*""".r

  def mover: Parser[Either[MoveList, Move]] = {
    moveList ^^ { y => Left(y)} | move ^^ { x => Right(x)}
  }

  def moves: Parser[Array[Either[MoveList, Move]]] = {
    repsep(mover, "") ^^ (_.toArray)
  }

  /*
  Moves I wanna parse right: ;B[pd](;W[pp];B[dd];W[dp])(;W[cd];B[pq];W[dq];B[qk])

  AKA:
    ;B[pd]
      (;W[pp];B[dd];W[dp])
      (;W[cd];B[pq];W[dq];B[qk])

   */

  def moveList: Parser[MoveList] = {
    "(" ~ repsep(move, "") ~ ")" ^^ {
      case "(" ~ moves ~ ")" => MoveList(moves)
    }
  }

  def move: Parser[Move] = {
    ";" ~ color ~ "[" ~ loc ~ loc ~ "]" ~ comment.? ^^ {
      case ";" ~ color ~ "[" ~ loc1 ~ loc2 ~ "]" ~ comment => Move(color, loc1, loc2, comment)
    }
  }

  private def color: Parser[String] = """[WB]""".r

  private def loc: Parser[String] = """[A-Ta-t]""".r


}

