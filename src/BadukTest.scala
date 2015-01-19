/**
 * Created by User on 1/18/2015.
 */

//import BadukBoard

import javax.swing.JFrame

import Piece._

object BadukTest {
  def main(Args: Array[String]) : Unit = {
    //x = new BadukBoard()
    val b = Piece.Black
    val w = Piece.White

    println(w)
    var x = new BadukBoard()
    x.set(2,2,None)
    x.set(4,5,Piece.Black)
    x.set(7,2,Piece.White)
    println(x)

    var frm = new JFrame("MainForm")
    frm.setContentPane(new MainForm().mainPanel)
    frm.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    frm.pack
    frm.setVisible(true)


  }
}
