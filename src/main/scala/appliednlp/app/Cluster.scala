package appliednlp.app

import org.apache.log4j.Level
import org.apache.log4j.Logger
import nak.cluster.Point
import nak.cluster.ClusterConfusionMatrix
import nak.cluster.ClusterReport
import nak.cluster.Kmeans
import nak.cluster.DistanceFunction
import nak.cluster.PointTransformer

import appliednlp.cluster._

/**
 * A standalone object with a main method for reading an input file and running
 * k-means with different options.
 */
object Cluster {

  def main(args: Array[String]) {
    // Parse and get the command-line options
    val opts = ClusterOpts(args)

    // This tells the k-means algorithm how much debugging information to show you
    // while you run the algorithm. 
    val logLevel = if (opts.verbose()) Level.DEBUG else Level.INFO
    Logger.getRootLogger.setLevel(logLevel)
    
    // Your code starts here. You'll use and extend it during every problem.
    // val filename = "~/applied-nlp/data/cluster/generated/" + args.takeRight(1)


    val dataTriples: IndexedSeq[(String, String, Point)] = {
        if (opts.features() == "schools") (SchoolsCreator("/Users/jamesevans/applied-nlp/data/cluster/schools/" + opts.filename())).toIndexedSeq
        else if (opts.features() == "countries") (CountriesCreator("/Users/jamesevans/applied-nlp/data/cluster/countries/" + opts.filename())).toIndexedSeq 
        else if (opts.features() == "fed-simple") (new FederalistCreator(simple = true)("/Users/jamesevans/applied-nlp/data/cluster/federalist/" + opts.filename())).toIndexedSeq 
        else if (opts.features() == "fed-full") (new FederalistCreator()("/Users/jamesevans/applied-nlp/data/cluster/federalist/" + opts.filename())).toIndexedSeq 
        else (DirectCreator("/Users/jamesevans/applied-nlp/data/cluster/generated/" + opts.filename())).toIndexedSeq
        }

    val dataPreTrans = for (triple <- dataTriples) yield triple._3

    val data = PointTransformer(opts.transform(), dataPreTrans)(dataPreTrans)

    val kmeansRes = new Kmeans(data, DistanceFunction(opts.distance()), fixedSeedForRandom = true).run(opts.k())

    val predicted = new Kmeans(data, DistanceFunction(opts.distance()), fixedSeedForRandom = true).computeClusterMemberships(kmeansRes._2)


    val gold = dataTriples.map(triple => triple._2)

    val ids = dataTriples.map(triple => triple._1)

println(kmeansRes._1)
kmeansRes._2.foreach(println)

println(ClusterConfusionMatrix(gold, opts.k(), predicted._2))

if (opts.report() == true) println(ClusterReport(ids, gold, predicted._2))


  }

}


/**
 * An object that sets of the configuration for command-line options using
 * Scallop and returns the options, ready for use.
 */
object ClusterOpts {

  import org.rogach.scallop._
  
  def apply(args: Array[String]) = new ScallopConf(args) {
    banner("""
Cluster application using nak.cluster.Kmeans

The distance functions are standard cosine, euclidean, and manhattan.

The transform option is one ident, zscore, and pca.
 - ident: no transformation of the input points
 - zscore: use standard z-scores in each dimension to scale the points
 - pca: scale the points with z-scores, then run PCA and use (as transformed dimensions) only the principle components that explain 95% of the variance

For usage see below:
	     """)

    val featureTypes = Set("standard","schools","countries","fed-simple","fed-full")
    val transformers = Set("i","ident","z","zscore","p","pca")
    val distanceFunctions = Set("c","cosine","e","euclidean","m","manhattan")
    val transform = opt[String]("transform", default=Some("ident"), validate = transformers, descr = "The transformation to use. Possible values: " + transformers.toSeq.sorted.mkString(",") )
    val distance = opt[String]("dist", default=Some("cosine"), validate = distanceFunctions, descr = "The distance function to use. Possible values: " + distanceFunctions.toSeq.sorted.mkString(",") )      
    val features = opt[String]("features", default=Some("standard"), validate = featureTypes, descr = "The type of features to extract. Possible values: " + distanceFunctions.toSeq.sorted.mkString(",") )
    val k = opt[Int]("num-clusters",short='k', required=true, validate = (0<), descr="The number of clusters to find.")
    val help = opt[Boolean]("help", noshort = true, descr = "Show this message")
    val verbose = opt[Boolean]("verbose")
    val showCentroids = opt[Boolean]("output-centroids",short='c', descr="Show centroids.")
    val report = opt[Boolean]("output-report",short='r', descr="Show full cluster report.")
    val filename = trailArg[String]("filename", descr = "The input filename.")
  }
}
