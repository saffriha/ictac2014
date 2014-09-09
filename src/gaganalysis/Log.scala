package gaganalysis

import scala.swing._
import scala.swing.BorderPanel.Position._
import scala.collection.mutable.Map
import event._
import java.awt.{ Color, Graphics2D }
import scala.util.Random
import edu.uci.ics.jung.graph.Graph
import edu.uci.ics.jung.graph.DirectedSparseGraph
import edu.uci.ics.jung.algorithms.layout.Layout
import edu.uci.ics.jung.algorithms.layout.FRLayout
import edu.uci.ics.jung.visualization.VisualizationViewer
import edu.uci.ics.jung.visualization.decorators.ToStringLabeller
import org.apache.commons.collections15.Transformer
import java.awt.Paint
import javax.swing.JComponent
import javax.swing.border.LineBorder
import java.awt.GridBagConstraints
import soot.SootMethod
import scala.swing.TabbedPane.Page
import java.util.Date
import java.util.Calendar
import scala.collection.mutable.HashMap
import java.io.FileWriter
import java.io.PrintWriter
import java.text.DateFormat
import java.util.Locale
import soot.options.Options
import soot.Scene
import java.text.SimpleDateFormat

// This file contains procedures for logging, profiling and statistics
// visualization.

case class CountingTimer() {
  var intervals: List[(Date, Date)] = List()
  var startTime: Date = null

  def start = startTime = IO.now
  def stop = intervals = (startTime, IO.now) +: intervals
  def time[T](s: => T): T = { start; val x = s; stop; x }

  def deltas: List[Int] = intervals map { case (d1, d2) => (d2.getTime - d1.getTime).toInt }
  def deltasSec: List[Float] = deltas map { _.toFloat / 1000.0f }

  def size = intervals.size
  def sum: Int = deltas.sum
  def sumSec: Float = deltasSec.sum
  def max: Option[Int] = if (deltas.isEmpty) None else Some(deltas.max)
  def min: Option[Int] = if (deltas.isEmpty) None else Some(deltas.min)
  def avg: Option[Int] = if (deltas.isEmpty) None else Some(sum / size)
  def maxSec: Option[Float] = if (deltasSec.isEmpty) None else Some(deltasSec.max)
  def minSec: Option[Float] = if (deltasSec.isEmpty) None else Some(deltasSec.min)
  def avgSec: Option[Float] = if (deltasSec.isEmpty) None else Some(sumSec / size)
}

object Log { var v = new Log2(true) }
  
class Log2(val showWindow: Boolean) {
  object Timer {
    val total = CountingTimer()
    val interprocAnalysis = CountingTimer()
    val intraprocAnalysis = CountingTimer()
    val ptsAnalysis = CountingTimer()
//    val ptsCfgNode = CountingTimer()
    val seAnalysis = CountingTimer()
//    val seCfgNode = CountingTimer()
    val sccGraphConstruction = CountingTimer()
    val profiling = CountingTimer()
    val astcreation = CountingTimer()
  }
  val t = new TabbedPane
  
  var ptsLogs = HashMap[SootMethod, PtsLog]()
  var curPtsLog: PtsLog = null
  var seLogs = HashMap[SootMethod, SELog]()
  var curSELog: SELog = null

  def showLogs(logs: Iterable[(SootMethod, Component)]) =
    new TabbedPane {
      logs.toList . sortBy { _._1.getName } . map { case (m, l) =>
        pages += new Page(m.getName, new BoxPanel(Orientation.Vertical) {
          contents ++= List(fromMethod(m), new ScrollPane(l))
        })
      }
    }
  
  def finished(m: Map[SootMethod, MethodSummary]) = {
    Timer.profiling.start
    new MainFrame {
      title = "Log"
      contents = new TabbedPane {
        pages ++= List(
          new Page("Summaries", fromSummaryMap(m)),
          new Page("Pts Flow Logs", showLogs(ptsLogs)),
          new Page("SE Flow Logs", showLogs(seLogs)),
          new Page("Timers", timerLog),
          new Page("Set Sizes", new ScrollPane(showSetSizes(m)))
          , new Page("Transfers", t)
        )
      }
    } . visible = showWindow
    logToFile
    Timer.profiling.stop
  }
  
  object Stats {
    var numMethods: Int = 0
    var numPureMethods: Int = 0
    var percentPureMethods: Double = 0.0
  }

  def logToFile = IO.writeToFile(
    "/home/d0lphchrist/benchmarks/" + IO.timestamp + " " +
      Scene.v.getMainClass.getName,
    "Benchmark:      " + Scene.v.getMainClass.getName + "\n" +
    "Compacting:     " + PtsMap.compactingMode + "\n" +
    "# Methods:      " + Stats.numMethods + "\n" +
    "# Pure:         " + Stats.numPureMethods + "\n" +
    "% Pure:         " + Stats.percentPureMethods + "\n" +
    "Total time:     " + Timer.total.sumSec + "\n" +
    "Interproc time: " + Timer.interprocAnalysis.sumSec + "\n" +
    "Pts time:       " + Timer.ptsAnalysis.sumSec + "\n" +
    "SE time:        " + Timer.seAnalysis.sumSec + "\n" +
    "Profiling time: " + Timer.profiling.sumSec + "\n" +
    "# Interproc:    " + Timer.interprocAnalysis.size + "\n" +
    "# Intraproc:    " + Timer.intraprocAnalysis.size + "\n" +
    "")
  
  def showSetSizes(summaries: Map[SootMethod, MethodSummary]) = new GridBagPanel {
    def gridConstraints(x: Int, y: Int) = {
      val c = new Constraints
      c.gridx = x
      c.gridy = y
      c.insets = new Insets(5, 5, 5, 5)
      c
    }
    
    def addRow(y: Int, cs: Iterable[Component]) = 
      (cs zip List.range(0, cs.size)) map {
        case (c, i) => add(c, gridConstraints(i, y)) }

    var curY: Int = 0
    def addTextRow(ss: String*): Unit = { addRow(curY, ss map mkText); curY += 1 }
    
    addTextRow("Points-to Set Compacting Mode: ",
               PtsMap.compactingMode.toString)

    addTextRow("")
               
    addTextRow("Method",
               "# Pts Env Pairs", "# Pts Env Nodes",
               "# Pts Set Pairs", "# Pts Set Nodes",
               "# Read Nodes", "# Write Nodes")
               
    summaries . toList
              . sortWith { _._1.getSignature <= _._1.getSignature }
              . map { case (m, s) => addTextRow(
                  m.getSignature,
                  s.ptsEnv.map.size.toString,
                  s.ptsEnv.map
                    . foldLeft(0) { case (n, (_, g)) => n + g.nodes.size }
                    . toString,
                  s.ptsMap.map.size.toString,
                  s.ptsMap.map
                    . foldLeft(0) { case (n, (Ref(g1, _), g2)) =>
                                      n + g1.nodes.size + g2.nodes.size }
                    . toString,
                  s.reads.nodes.size.toString,
                  s.writes.nodes.size.toString)
                }

    val numMethods = summaries.size
    val numPureMethods =
      summaries . values
                . filter { sum => sum.writes.hasEmptyLang &&
                                  sum.ptsMap.map.isEmpty }
                . size
    val percentPure = (100.0 * numPureMethods) / numMethods
    addTextRow(numMethods.toString + " methods")
    addTextRow(numPureMethods.toString + " pure methods")
    addTextRow(percentPure.toString + "% pure")
    Stats.numMethods = numMethods
    Stats.numPureMethods= numPureMethods
    Stats.percentPureMethods= percentPure
  }

  def onTransfer(callee: Call, calleeFO: PtsFlowObject, obj: GAG,
                 in: Map[Node, GAG], out: Map[Node, GAG], result: GAG) = {
    Timer.profiling.start
    t.pages += new Page(callee.method.getName,
                        new ScrollPane(fromTrans(callee, calleeFO, obj, in, out,
                                                 result)))
    Timer.profiling.stop
  }
  
  def onStartIntraproc(m: SootMethod) = {
    Timer.profiling.start
    curPtsLog = PtsLog()
    ptsLogs.put(m, curPtsLog)
    curSELog = SELog()
    seLogs.put(m, curSELog)
    Timer.profiling.stop
  }

  def timerLog = new GridBagPanel {
    def gridConstraints(x: Int, y: Int) = {
      val c = new Constraints
      c.gridx = x
      c.gridy = y
      c.insets = new Insets(5, 5, 5, 5)
      c
    }

    def addRow(y: Int, cs: Iterable[Component]) = 
      (cs zip List.range(0, cs.size)) map {
        case (c, i) => add(c, gridConstraints(i, y)) }

    var curY = 0
    def addTextRow(ss: String*): Unit = { addRow(curY, ss map mkText); curY += 1 }
    
    addTextRow("Timer", "Count", "Sum", "Average", "Min", "Max")
    
    def addTimer(ct: CountingTimer, text: String) =
      addTextRow(text,
                 ct.size.toString,
                 ct.sumSec.toString,
                 ct.avgSec.getOrElse(-1).toString,
                 ct.minSec.getOrElse(-1).toString,
                 ct.maxSec.getOrElse(-1).toString)
                 
    Timer.profiling.stop
    Timer.profiling.start
    
    addTimer(Timer.total,                "Total")
    addTimer(Timer.profiling,            "Profiling overhead")
    addTimer(Timer.interprocAnalysis,    "Interprocedural Analysis")
    addTimer(Timer.intraprocAnalysis,    "Intraprocedural Analysis")
    addTimer(Timer.ptsAnalysis,          "Points-to Analysis")
//  addTimer(Timer.ptsCfgNode,           "Points-to Analysis CFG Node")
    addTimer(Timer.seAnalysis,           "Sideeffect Analysis")
//  addTimer(Timer.seCfgNode,            "Sideeffect Analysis CFG Node")
    addTimer(Timer.sccGraphConstruction, "SCC Graph Construction")
    addTimer(Timer.astcreation,          "AST creation")

    addTextRow("Total without Profiling", (Timer.total.sumSec - Timer.profiling.sumSec).toString)
  }

  case class PtsLog() extends BoxPanel(Orientation.Vertical) {
    def onNode(stmt: soot.jimple.Stmt,
               gen: PtsFlowObject,
               killWith: PtsFlowObject) = {
      Timer.profiling.start
      contents += new BoxPanel(Orientation.Vertical) {
        contents ++= List(
          mkHeader(stmt.toString),
          mkSubHeader("Kill by update"), fromPtsEnv(killWith.env),
          mkSubHeader("Gen"),            fromPtsMap(gen.map)
        )
      }
      Timer.profiling.stop
    }
    
    def onJoin(in1: PtsFlowObject, in2: PtsFlowObject, out: PtsFlowObject) = {
      Timer.profiling.start
      contents += new BoxPanel(Orientation.Vertical) {
        contents ++= List(
          mkHeader("Joining outs(preds(n)) to new in(n)"),
          mkSubHeader("in1"), fromPtsFlowObject(in1),
          mkSubHeader("in2"), fromPtsFlowObject(in2),
          mkSubHeader("out"), fromPtsFlowObject(out)
        )
      }
      Timer.profiling.stop
    }
  }
  
  def fromPtsFlowObject(fo: PtsFlowObject): Component = {
    val mapSize = fo.map.map.size
    val envSize = fo.env.map.size
    fo.map.compact
    val mapSizeCompacted = fo.map.map.size
    
    new BoxPanel(Orientation.Vertical) { contents ++= List(
      mkText("Pts Env (" + envSize + " pairs)"), fromPtsEnv(fo.env),
      mkText("Pts Map (" + mapSize + " pairs, " +
             mapSizeCompacted + " compacted)"), fromPtsMap(fo.map)
    ) }
  }
  
  case class SELog() extends BoxPanel(Orientation.Vertical) {
    def onNode(stmt: soot.jimple.Stmt,
               gen: SEFlowObject) = {
      Timer.profiling.start
      contents += new BoxPanel(Orientation.Vertical) {
        contents ++= List(
          mkHeader(stmt.toString),
          mkSubHeader("Gen"),
          fromSEFlowObject(gen)
        )
      }
      Timer.profiling.stop
    }
    
    def onJoin(in1: SEFlowObject, in2: SEFlowObject, out: SEFlowObject) = {
      Timer.profiling.start
      contents += new BoxPanel(Orientation.Vertical) {
        contents ++= List(
          mkHeader("Joining outs(preds(n)) to new in(n)"),
          mkSubHeader("in1"), fromSEFlowObject(in1),
          mkSubHeader("in2"), fromSEFlowObject(in2),
          mkSubHeader("out"), fromSEFlowObject(out)
        )
      }
      Timer.profiling.stop
    }
  }
  
  def fromSEFlowObject(fo: SEFlowObject): Component =
    new BoxPanel(Orientation.Vertical) { contents ++= List(
      mkText("Reads"),  new GAGComponent(fo.reads),
      mkText("Writes"), new GAGComponent(fo.writes)
    ) }

  def fromTrans(callee: Call, calleeFO: PtsFlowObject, obj: GAG,
                in: Map[Node, GAG], out: Map[Node, GAG],
                result: GAG) : Component = {
    new BoxPanel(Orientation.Vertical) {
      contents ++= List(
        mkHeader("Transfer object"),
        new GAGComponent(obj),
        
        mkHeader("Transfer result"),
        new GAGComponent(result)
      )
      
      contents += mkHeader("In Map")
      contents ++= 
        in map { p: (Node, GAG) => new BoxPanel(Orientation.Horizontal) {
          contents += mkText(p._1.toString)
          contents += new GAGComponent(p._2)
        } }
      
      contents += mkHeader("Out Map")
      contents ++= 
        out map { p => new BoxPanel(Orientation.Horizontal) {
          contents += mkText(p._1.toString)
          contents += new GAGComponent(p._2)
        } }
      
      contents ++= List(
        mkHeader("Callee Env"),
        fromPtsEnv(calleeFO.env),
        
        mkHeader("Callee Map"),
        fromPtsMap(calleeFO.map)
      )
    }
  }
  
  def showSummaryMap(ms: Map[SootMethod, MethodSummary]) = new MainFrame {
      title    = "Method Summaries"
      size     = new Dimension(300, 200)
      contents = fromSummaryMap(ms)
    }

  def fromSummaryMap(ms: Map[SootMethod, MethodSummary]): Component = 
    new TabbedPane {
      ms.toList . sortBy { _._1.getName } . map {
        (p => pages += new Page(p._1.getName, new BoxPanel(Orientation.Vertical) {
          contents += fromMethod(p._1)
          contents += fromSummary(p._2)
      } )) }
    }

  def fromMethod(m: SootMethod): Component =
    new BoxPanel(Orientation.Vertical) {
      contents ++= List(
        mkHeader("Method source (Jimple)"),
        mkCode(if (m.hasActiveBody) m.getActiveBody.toString
               else                 "<no jimple body>")
     )
    }

  def fromSummary(summary: MethodSummary): Component = {
    val mapSize = summary.ptsFO.map.map.size
    val envSize = summary.ptsFO.env.map.size
    summary.ptsFO.map.compact
    val mapSizeCompacted = summary.ptsFO.map.map.size
    new ScrollPane(new BoxPanel(Orientation.Vertical) {
      contents ++= List(
        mkHeader("Return Obj"),    new GAGComponent(summary.retObj),
        mkHeader("Pts Env (" + envSize + " pairs)"), fromPtsEnv(summary.ptsEnv),
        mkHeader("Pts Map (" + mapSize + " pairs, " +
                 mapSizeCompacted + " compacted)"), fromPtsMap(summary.ptsMap),
        mkHeader("Read Effects"),  new GAGComponent(summary.reads),
        mkHeader("Write Effects"), new GAGComponent(summary.writes)
      )
    })
  }
  
  def fromPtsEnv(e: PtsEnv): Component = new GridBagPanel {
      e.map zip List.range(0, e.map.size) map
      { p: ((VarID, GAG), Int) => p match { case ((v, g), i) => {
        add(mkText(v.toString), (0, i))
        add(mkText("   →   "), (1, i))
        add(new GAGComponent(g), (2, i))
      } } }
    }
  
  def fromPtsMap(e: PtsMap): Component = new GridBagPanel {
      e.map zip List.range(0, e.map.size) map
      { p: ((Ref, GAG), Int) => p match { case ((r, g), i) => {
        add(new GAGComponent(r.base), (0, i))
        add(mkText("   .   "), (1, i))
        add(mkText(r.field.toString), (2, i))
        add(mkText("   →   "), (3, i))
        add(new GAGComponent(g), (4, i))
      } } }
    }
  
  def mkHeader(t: String): Component =
    mkText(t, Style.Color.BG.Header, Style.Font.Header)

  def mkSubHeader(t: String): Component =
    mkText(t, Style.Color.BG.SubHeader, Style.Font.SubHeader)
  
  def mkText(t: String): Component =
    mkText(t, Style.Color.BG.Text, Style.Font.Text)

  def mkCode(t: String): Component =
    new ScrollPane(mkText(t, Style.Color.BG.Code, Style.Font.Code))
  
  def mkText(t: String, bg: java.awt.Color, f: java.awt.Font) =
    new TextArea {
      background = bg
      editable = false
      font = f
      text = t
    }
    
  object Style {
    object Color {
      object BG {
        val Header    = new Color(150, 200, 255)
        val SubHeader = new Color(200, 235, 255)
        val Text      = new Color(255, 255, 255)
        val Code      = new Color(255, 255, 255)
      }
      object FG {
        val Header    = new Color(  0,   0,   0)
        val SubHeader = new Color(  0,   0,   0)
        val Text      = new Color(  0,   0,   0)
        val Code      = new Color(  0,   0,   0)
      }
    }
    object Font {
      val Header    = new Font("Ariel", java.awt.Font.PLAIN, 24)
      val SubHeader = new Font("Ariel", java.awt.Font.PLAIN, 20)
      val Text      = new Font("Ariel", java.awt.Font.PLAIN, 16)
      val Code      = new Font("Ariel", java.awt.Font.PLAIN, 14)
    }
  }
}

object IO {
  def using[A <: {def close(): Unit}, B](param: A)(f: A => B): B =
  try { f(param) } finally { param.close() }

  def writeToFile(fileName:String, data:String) = 
    using (new FileWriter(fileName)) {
      fileWriter => fileWriter.write(data)
    }

  def appendToFile(fileName:String, textData:String) =
    using (new FileWriter(fileName, true)){ 
      fileWriter => using (new PrintWriter(fileWriter)) {
        printWriter => printWriter.println(textData)
      }
    }
  
  def now: Date = Calendar.getInstance.getTime
  
  def timestamp: String =
    new SimpleDateFormat("yyyy-MM-dd HH:mm:ss") format now
}
