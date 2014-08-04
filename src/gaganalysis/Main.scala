package gaganalysis

import soot.options.Options
import sootproxy.SceneTransformerProxy
import soot.PackManager
import soot.SceneTransformer
import soot.Transform
import sootproxy.ScalaSceneTransformer
import soot.Value
import soot.jimple.AssignStmt
import scalax.collection.mutable.Graph
import soot.SootMethod
import scalax.collection.GraphEdge.DiEdge
import soot.Scene
import soot.toolkits.graph.DirectedGraph
import scala.collection.mutable.Queue
import scala.collection.mutable.HashMap
import scala.collection.mutable.Map
import soot.toolkits.graph.UnitGraph
import soot.toolkits.graph.BriefUnitGraph

object Main {
  def main(args: Array[String]) {
    if (args.isEmpty) { println(usage); System.exit(1); }
    
    Log.Timer.total.start
    
    // Prepare the soot options for our analysis.
    setOptions
    
    // Handle the pts-compacting parameter if one is provided.
    val sootArgs = stripOwnArgs(args)
    
    // Create the SceneTransformer of our analysis.
    // A soot SceneTransformer represents a whole program analysis,
    // which get's called once and has access to the whole program term (Scene).
    val st = new InterprocAnalysis(args.head)
    
    // Add the SceneTransformer to the whole-program-analysis jimple
    // transformation pack (wjtp), such that the program analysed by
    // soot.Main.main get's passed to our analysis.
    PackManager.v.getPack("wjtp").add(
            new Transform("wjtp.agSideeffect",
                new SceneTransformerProxy(st)))
    
    // Starts soot, loads
    // java sources specified in the arguments, translates them to jimple
    // and then passes them to our SceneTransformer, which performs our analysis
    // and then opens a Scala Swing frame presenting the analysis results.
    soot.Main.main(sootArgs)
  }
  
  def stripOwnArgs(args: Array[String]): Array[String] = {
    args . filter { _.startsWith("--pts-compacting=") }
         . map { s => PtsMap.compactingMode =
             s.substring(s.indexOf("=")+1, s.length).toLowerCase match {
               case "none"         => PtsCompactingMode.None
               case "conservative" => PtsCompactingMode.Conservative
               case "aggressive"   => PtsCompactingMode.Aggressive
               case s =>
                 println("Unsupported pts-compacting mode specified: " + s)
                 System.exit(1)
                 PtsCompactingMode.None
           } }
    args . filter { ! _.startsWith("--pts-compacting=") }
  }
  
  def setOptions {
    // The soot options are set globally using a Java style singleton.
    val opt = Options.v;
    
    // Prefer .java over .jimple sources if both are available.
    opt.set_src_prec(Options.src_prec_java)
    
    // Create no output, only analyse the input.
    opt.set_output_format(Options.output_format_none)
    
    // Interpret the input sources in context of the default java classpath.
    opt.set_prepend_classpath(true)
    
    // Enable whole program analysis mode, leading to call graph creation.
    opt.set_whole_program(true)

    // Specify the call graph algorithm.
    // - CHA restricts targets for virtual calls only by type
    // - SPARK uses a context-insensitive points-to analysis to further
    //   restrict the targets of virtual calls.
    opt.setPhaseOption("cg.spark", "enabled:true")
    opt.setPhaseOption("cg.cha",   "enabled:false")
    
    // Use the main function as the only call graph entry.
    opt.setPhaseOption("cg",       "implicit-entry:false")
    
    // Disable unused packs.
    List("wjap", "wjpp", "wspp", "wstp", "gb", "bb", "shimple", "tag", "stp",
         "db", "jap") map { opt.setPhaseOption(_, "enabled:false") }
  }
  
  def usage: String =
    "No arguments provided.\n" +
    "\n" +
    "Usage:  scala AnalysisBinary [MainClass] -cp [ClassPath]\n" +
    "where the Java Project on which the analysis should be run is " +
    "described by the [ClassPath] to the project's source directory and " +
    "the [MainClass] specifies the class containing the project's main function " +
    "relative to the [ClassPath].\n" +
    "\n" +
    "Example: scala AnalysisBinary pkg.subpkg.MainClass -cp ./project/src\n" +
    "for a project which has a main function in " +
    "\"./project/src/pkg/subpkg/MainClass.java\"."
    
}
