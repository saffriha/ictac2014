package gaganalysis

import java.awt.Color
import java.awt.Paint

import scala.swing.BoxPanel
import scala.swing.Component
import scala.swing.Dimension
import scala.swing.Font
import scala.swing.Orientation
import scala.swing.TextArea

import org.apache.commons.collections15.Transformer

import edu.uci.ics.jung.algorithms.layout.FRLayout
import edu.uci.ics.jung.algorithms.layout.Layout
import edu.uci.ics.jung.graph.DirectedSparseGraph
import edu.uci.ics.jung.graph.Graph
import edu.uci.ics.jung.visualization.VisualizationViewer
import edu.uci.ics.jung.visualization.decorators.ToStringLabeller
import javax.swing.JComponent
import javax.swing.border.LineBorder

// This file describes a Scala Swing component for visualizing an GAG using the
// JUNG Graph Library.

class GAGComponent(gag: GAG) extends BoxPanel(Orientation.Vertical) {
  val g: Graph[GVertex, GEdge] = new DirectedSparseGraph()
  addToGraph(g, gag)
  val c = showGAG(g)
  c.setBorder(new LineBorder(Color.BLACK, 1))
  contents += new TextArea {
//    def align(s: String, step: Int): String =
//      (s.lines fold "") { (s0, l) => s0 +
//        ((for { i <- List.range(0, l.size, step) }
//          yield { l.substring(i, Math.min(i+step, l.size)) }) fold "") {_+"\n"+_} + "\n"
//      }
    def sepWithNl(xs: Iterator[String]): String = (xs fold "") { _ + "\n" + _ }
    def align(s: String, step: Int): String =
      s . split('\n')
        . map { _ grouped step }
        . map { sepWithNl(_) }
        . fold("") { _ + "\n" + _ }
    text = align(gag.toString, 40)
//    text = gag.toString
    editable = false
    font = new Font("Ariel", java.awt.Font.PLAIN, 16)
    background = new Color(255, 255, 255)
  }
  contents += Component.wrap(c)
  
  def showGAG(g: Graph[GVertex, GEdge]): JComponent = {
    val l: Layout[GVertex, GEdge] =
        new FRLayout(g)
//        new CircleLayout(g)
//        new FRLayout2(g)
//        new ISOMLayout(g)
//        new KKLayout(g)
    
    val dim = new Dimension(150, 150)
    l.setSize(dim)
    
    val vertexFill = new Transformer[GVertex, Paint]() {
      override def transform(n: GVertex): Paint = {
        val rootAmount = if (n.isRoot) 0.5f else 1.0f
        if (n.isExit) new Color(0.0f, rootAmount, 0.0f)
        else          new Color(rootAmount, 0.0f, 0.0f)
      }
    }
        
    val vv = new VisualizationViewer[GVertex, GEdge](l)
    vv.getRenderContext.setVertexFillPaintTransformer(vertexFill)
    vv.getRenderContext.setVertexLabelTransformer(new ToStringLabeller[GVertex])
    val dim2 = new Dimension(dim.width+200, dim.height+40)
    vv.setPreferredSize(dim2)
    vv.setMaximumSize(dim2)
    vv.setMinimumSize(dim2)
    vv.setSize(dim2)
    vv
  }
  
  def addToGraph(g: Graph[GVertex, GEdge], gag: GAG) = {
    for (n <- gag.nodes)
      g.addVertex(new GVertex(n, gag hasRoot n, gag hasExit n))
    
    for (gag.graph.EdgeT(src, tgt) <- gag.graph.edges)
      g.addEdge(
        new GEdge(src, tgt),
        new GVertex(src, gag hasRoot src, gag hasExit src),
        new GVertex(tgt, gag hasRoot tgt, gag hasExit tgt))
  }
  
  case class GVertex(n: Node, isRoot: Boolean, isExit: Boolean) {
    override def toString: String  =
      "<" + n.id + ", " + n.pp +
        (if (isRoot) ", root" else "") +
        (if (isExit) ", exit" else "") +
        ">"
//      n.toString +
//        (if (isExit || isRoot) ":") +
//        (if (isRoot) "R" else "") +
//        (if (isExit) "E" else "")
  }

  case class GEdge(src: Node, tgt: Node)
  
}