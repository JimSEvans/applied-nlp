package appliednlp.classify

import nak.core.AttrVal
import chalk.lang.eng.PorterStemmer

import scala.collection.mutable

// tokenizer from tshrdlu

object SimpleTokenizer {
  def apply(text: String): IndexedSeq[String] = text
    .replaceAll("""([\?!()\";\|\[\].,'])""", " $1 ")
    .trim
    .split("\\s+")
    .toIndexedSeq
    .map(x=>x.toLowerCase)
}




/**
 * An object that sets up the configuration for command-line options using
 * Scallop and returns the options, ready for use.
 */
object ScFeaturesOpts {

  import org.rogach.scallop._
  
  def apply(args: Array[String]) = new ScallopConf(args) {
    banner("""
For usage see below:
	     """)
    val help = opt[Boolean]("help", noshort = true, descr = "Show this message")
    val verbose = opt[Boolean]("verbose")
    val bitstringsSource = opt[String]("bitstrings", descr = "File containing bitstrings")
    val extendedFeatures = opt[Boolean]("extended",short='e', descr="Use extended features.")
    val inputFile = trailArg[String]("inputfile", descr = "Input file to create features from.")
  }
}


/**
 * An application for extracting features from the native format for 
 * classification.
 */
object ScFeatures {

  /**
   * The main method -- do the work. Don't change it.
   */
  def main(args: Array[String]) {

    // Parse and get the command-line options

    val opts = ScFeaturesOpts(args)
   
    val inputFile = opts.inputFile()

    val bitstrings = opts.bitstringsSource.get match {
      case Some(bitstringsSource) =>
        io.Source.fromFile(bitstringsSource).getLines.map { line =>
          val Array(word, bitstring) = line.split("\\s+")
          (word -> BitVector(bitstring))
        }.toMap

      case None => new collection.immutable.HashMap[String, BitVector]()
    }

    val featureExtractor = BasicScFeatureExtractor
      // if (opts.extendedFeatures()) new ExtendedScFeatureExtractor(bitstrings)
      // else BasicScFeatureExtractor





    io.Source.fromFile(inputFile).getLines.foreach { line =>
      val label = line.slice(0,2)
      val features = featureExtractor(line)
      println(features.map(_.toString).mkString(",") + "," + label)
    }

  }

}




/**
 * A trait for classes that can extract features from the information in
 * the PPA files.
 */
trait ScFeatureExtractor {
  
  /**
   * Given the verb, noun, preposition, and prepositional object,
   * create a set of AttrVal objects. (A "feature" is an attribute with a
   * value.) 
   */
  def apply(line: String): Iterable[AttrVal]
}



/**
 * The simplest feature extractor: each word gets a feature, where the 
 * attribute is the type of the word. 
 */
object BasicScFeatureExtractor extends ScFeatureExtractor {

  val attributes = Vector("jenny", "mccarthy", "measles", "tuberculosis", "polio", "influenza", "mumps", "parent", "parents", "link", "population", "risk", "natural", "danger", "evidence", "debunk", "myth","deaths","health","children", "child", "pose", "claim", "difficult", "believe", "reality", "doctor", "disproven", "epidemic", "disease", "mortality", "always", "skepticism", "pseudoscience", "untrue", "simply", "mercury", "lie", "thimerosal", "metal", "compound", "significant", "anti-vaccination", "anti-vaccine", "skeptics", "industry")
  def getAttrVal (tuple: Tuple2[String, String]):AttrVal = {
      AttrVal(tuple._1, tuple._2)
    }

 //   lazy val stemmer = new PorterStemmer

  override def apply(
    line: String): Iterable[AttrVal] = {

    val tokens = SimpleTokenizer(line)

    val values = attributes.map(x=>tokens.contains(x).toString)

    attributes.zip(values).map(x=>getAttrVal(x))


    
    // List(AttrVal("word", SimpleTokenizer(line)(10)))

    // List(
    //   AttrVal("verb", verb),
    //   AttrVal("noun", noun),
    //   AttrVal("prep", prep),
    //   AttrVal("prep_obj", prepObj))
  }

}















// /**
//  * An extended feature extractor. It is your job to fill this out further.
//  */
// class ExtendedFeatureExtractor(bitvectors: Map[String, BitVector])
//   extends FeatureExtractor {

//   lazy val stemmer = new PorterStemmer

//   override def apply(
//     verb: String, noun: String, prep: String, prepObj: String): Iterable[AttrVal] = {

//     // Use the basic feature extractor to get the basic features (no need to 
//     // duplicate effort and specify it again).
//     val basicFeatures = BasicFeatureExtractor(verb, noun, prep, prepObj)

//     // Extract more features
//     val moreFeatures = {
//       val vClass = bitvectors(verb).keepTopBits(20).toString
//       val nClass = bitvectors(noun).keepTopBits(15).toString
//       val vStem = stemmer(verb)
//       val nStem = stemmer(noun)
//       val vStem_nStem = vStem + "_" + nStem
//       val vClass_nClass = vClass + "_" + nClass
//       val vClass_prep = vClass + "_" + prep
//       val nClass_prep = nClass + "_" + prep
//       val nLength = noun.length.toString
//       val vLength = verb.length.toString

//       // val verb_prep = verb + "_" + prep
//       // val verb_noun = verb + "_" + noun

//       List(
//         AttrVal("vClass", vClass)
//         ,AttrVal("nClass", nClass)
//         ,AttrVal("vStem", vStem)
//         ,AttrVal("nStem", nStem)
//         ,AttrVal("vStem_nStem", vStem_nStem)
//         ,AttrVal("vClass_nClass", vClass_nClass)
//         ,AttrVal("vClass_prep", vClass_prep)
//         ,AttrVal("nLength", nLength)
        
//         // ,AttrVal("nClass_prep", nClass_prep)
//         // ,AttrVal("vLength", vLength)

//         )

//       //AttrVal("verb+noun", verb_and_noun),


//     }

//     // Return the features. You should of course add your features to basic ones.
//     basicFeatures ++ moreFeatures
//   }

// }

// /**
//  * This is an entirely cruddy, slow implementation of a bit vector,
//  * not using any bitwise ops, etc., but it should suffice for this problem.
//  *
//  * And, yes, we are using Ints where it could be Booleans, and we could have
//  * the wrong values in there, but this keeps it easy, and again, is sufficient
//  * for this problem.
//  * 
//  * Feel free to add more capability to this if it helps you create better
//  * features.
//  */
// class BitVector(bits: IndexedSeq[Int]) {

//   /**
//    * Get the bit value at the given index.
//    */
//   def apply(index: Int) = bits(index)

//   /**
//    * Get the integer value of the bits
//    */
//   lazy val toInt = Integer.parseInt(bits.mkString, 2)

//   /**
//    *  Keep the top bits up to the given index, and then make the remaining bits
//    *  zero.
//    */
//   def keepTopBits(index: Int) =
//     new BitVector(bits.take(index) ++ Vector.fill(bits.length - index)(0))

//   /**
//    * Concatenate the bits together.
//    */
//   override def toString = bits.mkString
// }

// /**
//  * Companion object to the BitVector class.
//  */
// object BitVector {

//   /**
//    * Create a bit vector from a string of zeros and ones.
//    */
//   def apply(bitstring: String) =
//     new BitVector(bitstring.split("").drop(1).map(_.toInt).toIndexedSeq)
// }



