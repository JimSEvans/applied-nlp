package appliednlp.cluster

import nak.cluster._
import nak.util.CollectionUtil._
import chalk.util.SimpleTokenizer

import org.apache.log4j.Logger
import org.apache.log4j.Level

import scala.io.Source

/**
 *  Read data and produce data points and their features.
 *
 *  @param filename the name of the file containing the data
 *  @return a triple, the first element of which is a sequence of id's
 *     (unique for each row / data point), the second element is the sequence
 *     of (known) cluster labels, and the third of which is the sequence of
 *     Points to be clustered.
 */
trait PointCreator extends (String => Iterator[(String,String,Point)])


/**
 * Read data in the standard format for use with k-means.
 */
object DirectCreator extends PointCreator {


 def apply (filename: String) = {
  val lines = Source.fromFile(filename).getLines
  val itSplit = for (line <- lines) yield line.split(" ")

  for (arr <- itSplit) 
    yield (arr(0), arr(1), new Point(IndexedSeq(arr(2).toDouble,arr(3).toDouble)))
  
}

}


/**
 * A standalone object with a main method for converting the achieve.dat rows
 * into a format suitable for input to RunKmeans.
 */

// object SchoolsCreator extends PointCreator {

// //   def apply(filename: String) = List[(String,String,Point)]().toIterator

// // }

object SchoolsCreator extends PointCreator { 


  // val filt = itSplit.toIndexedSeq.filter(x=>x.mkString != "").toIterator


  def apply(filename: String) = {
  val lines = Source.fromFile(filename).getLines

  // filter out empty lines
  // val realLines = lines.toIndexedSeq.filter(x => x != "").toIterator

  // split them on whitespace
  val itSplit: Iterator[Array[String]] = for (line <- lines) yield  line.split("\\s+")
  // val itReal = itSplit.toIndexedSeq.dropRight(1).toIterator

// for (arr <- itSplit) println(arr.mkString(" "))

  val it1: Iterator[(String, Point, Point)] = for (arr <- itSplit) 
                yield (arr.dropRight(4).mkString("_"),
                                  new Point(IndexedSeq(arr.takeRight(4)(0).toDouble, arr.takeRight(4)(1).toDouble)),
                                  new Point(IndexedSeq(arr.takeRight(4)(2).toDouble.toDouble, arr.takeRight(4)(3).toDouble)))

  val it2 = for (tuple <- it1) yield IndexedSeq((tuple._1 + "_4th", tuple._2), (tuple._1 + "_6th", tuple._3))


  val seq1: Iterator[(String, Point)] = it2.toIndexedSeq.flatten.toIterator
  
  // val indexes = (1 to seq1.length).toIndexedSeq

  // val seq2 = indexes.zip(seq1)         // : IndexedSeq[Tuple2[Int, Tuple2[String, Point]]]

  // val seq3 = for (tuple <- seq2) yield (tuple._1.toString, tuple._2._1.takeRight(3).dropRight(2), tuple._2._2)

  // val seq = seq3.toIterator

  // seq

  for (tuple <- seq1) yield (tuple._1, tuple._1.takeRight(3).dropRight(2), tuple._2)

  } 

}

/**
 * A standalone object with a main method for converting the birth.dat rows
 * into a format suitable for input to RunKmeans.
 */
object CountriesCreator extends PointCreator {

  def apply(filename: String) = {
  val lines = Source.fromFile(filename).getLines
  val itSplit = for (line <- lines) yield line.split("\\s+")

  val it: Iterator[(String, String, Point)] = for (arr <- itSplit) 
                yield (arr.dropRight(2).mkString("_"), "1",
                                  new Point(IndexedSeq(arr.takeRight(2)(0).toDouble, arr.takeRight(2)(1).toDouble)))
  it

  }

}


































/**
 * A class that converts the raw Federalist
 * papers into rows with a format suitable for input to Cluster. As part of
 * this, it must also perform feature extraction, converting the texts into
 * sets of values for each feature (such as a word count or relative
 * frequency).
 */
class FederalistCreator(simple: Boolean = false) extends PointCreator {

  def apply(filename: String) = {

    // val texts = FederalistArticleExtractor(filename).map(x=>x("text")).toIndexedSeq

    val Space = """ """.r

    val seqOfMaps = FederalistArticleExtractor(filename)

    val ids = for (map <- seqOfMaps) yield map("id")

    val gold = for (map <- seqOfMaps) yield Space.replaceAllIn(map("author"), "_")

    val texts = for (map <- seqOfMaps) yield map("text")

    val points: IndexedSeq[Point] = {
      if (simple == true) extractSimple(texts)
      else extractFull(texts)}

    val seq = for (index <- (0 to seqOfMaps.length-1))
                yield (ids(index), gold(index), points(index))

    seq.toIterator
}




  val lower = "abcdefghijklmnopqrstuvwxyz"
  val upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  val big2little = upper.zip(lower).toMap

  def mayLow (letter: Char): Char = {
    if (upper.contains(letter)) big2little(letter)
    else letter
  }

  def lowercase (word: String):String = {
    word.map(x => mayLow(x)).mkString("")
  }

def getCountMap (text: String): Map[String, Int] = {
        SimpleTokenizer(text)
        .map(x => lowercase(x))
        .groupBy(x=>x)
        .mapValues(x=>x.length)
        .withDefault(x=>0)
      }

  /**
   * Given the text of an article, compute the frequency of "the", "people"
   * and "which" and return a Point per article that has the frequency of
   * "the" as the value of the first dimension, the frequency of "people"
   * for the second, and the frequency of "which" for the third.
   *
   * @param texts A sequence of Strings, each of which is the text extracted
   *              for an article (i.e. the "text" field produced by
   *              FederalistArticleExtractor).
   */
  def extractSimple(texts: IndexedSeq[String]): IndexedSeq[Point] = {
    
      val mapsSimple = texts.map(x=>getCountMap(x))

      for (map <- mapsSimple) yield new Point(IndexedSeq(map("the").toDouble, map("people"), map("which")))


  }


  def getAvgLen (text: String): Double = {
      val words = SimpleTokenizer(text)
      val wordLens = words.map(x=>x.length)
      wordLens.sum.toDouble/(words.length).toDouble
      }

  def getTyTokRatio (text: String): Double = {
      val words = SimpleTokenizer(text).map(lowercase)
      val tokens = words.length.toDouble
      val types = words.toSet.size 
      types/tokens     
    }

val StartsCap = """([A-Z][a-z]+)""".r
  def getCapRatio (text: String): Double = {
      val words = SimpleTokenizer(text)
      words.filter(x=>x match {case StartsCap(x) => true; case _ => false}).length/words.length.toDouble
    }


  val func = IndexedSeq(
  "the", "a", "an", "all", "every", "each", "many", "some", ".", ",",
  "to", "for", "on", "with", "without", "by", "of", "at", "off", "in", "out", "from",
  "can", "will", "would", "should", "shall", "could", "does",
  // "which", "what", "how", "who",    //x
  "he", "she", "we", "they", "it", "him", "her", "us", "them", "you",
  "not", "no", "have", "and", "since", "or", "yet", "while", "because", "though"
  // "therefore", "thus", "as", "such", "so"   //x

  )




  // def getFreq (text: String, words: IndexedSeq[String]): Double = {
  //     val tokens = SimpleTokenizer(text).map(x=>lowercase(x))
  //     val theMap = tokens.groupBy(x=>x)
  //                   .mapValues(x=>x.length)
  //                   .withDefault(x=>0)
  //     val numWds = tokens.length
  //     val count = for (word <- words) yield theMap(word)
  //     (count.sum)/numWds

  //     }


def getFreqs (text: String): Map[String, Double] = {
        val tokens = SimpleTokenizer(text)
        .map(x => lowercase(x))
        
        val total = tokens.length

        tokens.groupBy(x=>x)
        .mapValues(x=>x.length)
        .mapValues(x=>x/total.toDouble)
        .withDefault(x=>0.0)

      }

def getPoint (text: String): Point = {
    val map = getFreqs(text)
    val dimensions = (for (word <- func) yield map(word)).toIndexedSeq
    new Point(dimensions ++ 
      IndexedSeq(getAvgLen(text)) ++ 
      IndexedSeq(getTyTokRatio(text)) ++ 
      IndexedSeq(getCapRatio(text)))

}



  /**
   * Given the text of an article, extract features as best you can to try to
   * get good alignment of the produced clusters with the known authors.
   *
   * @param texts A sequence of Strings, each of which is the text extracted
   *              for an article (i.e. the "text" field produced by
   *              FederalistArticleExtractor).
   */
  def extractFull(texts: IndexedSeq[String]): IndexedSeq[Point] = {
  
    texts.map(getPoint)

    // for (text <- texts) yield Point(IndexedSeq(getFreq(text, IndexedSeq("the")), getFreq(text, prep), getFreq(text, IndexedSeq(",")), getAvgLen(text)))
  // val map = texts.map(getCountMap)
  // for (text <- texts) yield new Point(IndexedSeq(map("the"), map("people"), map("which")))

    // getFreqOfWord(texts)

    // val maps = texts.map(x=>getX(x))

    // for (map <- maps) yield new Point(IndexedSeq(map("the").toDouble, map("people"), map("which")))
      

  }



}

































object FederalistArticleExtractor {
  /**
   * A method that takes the raw Federalist papers input and extracts each
   * article into a structured format.
   *
   * @param filename The filename containing the Federalist papers.
   * @return A sequence of Maps (one per article) from attributes (like
   *         "title", "id", and "text") to their values for each article.
   */
  def apply(filename: String): IndexedSeq[Map[String, String]] = {

    // Regex to identify the text portion of a document.
    val JustTextRE = (
      """(?s)\*\*\* START OF THIS PROJECT GUTENBERG.+""" +
      """\*\*\*(.+)\*\*\* END OF THIS PROJECT GUTENBERG""").r

    // Regex to capture different parts of each article.
    val ArticleRE = (
      """(?s)(\d+)\n+""" + // The article number.
      """(.+?)\n+""" + // The title (note non-greedy match).
      """((?:(?:For|From)[^\n]+?)?)\s+""" + // The publication venue (optional).
      """((?:(?:Monday|Tuesday|Wednesday|Thursday|Friday|Saturday|Sunday).+\d\d\d\d\.)?)\n+""" + // The date (optional).
      """((?:MAD|HAM|JAY).+?)\n+""" + // The author(s).
      """(To the [^\n]+)""" + // The addressee.
      """(.+)""" // The text.
      ).r

    val book = io.Source.fromFile(filename).mkString
    val text = JustTextRE.findAllIn(book).matchData.next.group(1)
    val rawArticles = text.split("FEDERALIST.? No. ")

    // Use the regular expression to parse the articles.
    val allArticles = rawArticles.flatMap {
      case ArticleRE(id, title, venue, date, author, addressee, text) =>
        Some(Map("id" -> id.trim,
          "title" -> title.replaceAll("\\n+", " ").trim,
          "venue" -> venue.replaceAll("\\n+", " ").trim,
          "date" -> date.replaceAll("\\n+", " ").trim,
          "author" -> author.replaceAll("\\n+", " ").trim,
          "addressee" -> addressee.trim,
          "text" -> text.trim))

      case _ => None
    }.toIndexedSeq

    // Get rid of article 71, which is a duplicate, and return the rest.
    allArticles.take(70) ++ allArticles.slice(71, allArticles.length)
  }

}
