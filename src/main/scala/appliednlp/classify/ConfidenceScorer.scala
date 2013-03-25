package appliednlp.classify

/**
 * An application that takes a gold labeled file and a file containing
 * predictions, and then computes the accuracy for the top-third most
 * confident instances and the accuracy for the bottom-third (least 
 * confident instances).
 */
object ConfidenceScorer {

  def main(args: Array[String]) {

  	val gold:IndexedSeq[String] = {   // PPA dev set
  		val lines:IndexedSeq[String] = io.Source.fromFile(args(0).toString).getLines.toIndexedSeq
  		val lines2:IndexedSeq[Array[String]] = lines.map(line => line.split("\\s+"))
  		lines2.map(x=>x(5))
 
  	}

  	val predict:IndexedSeq[Tuple2[String, Double]] = {     //predictFile for PPA dev set
  		val lines = io.Source.fromFile(args(1).toString).getLines.toIndexedSeq
  		val lines2 = lines.map(line => line.split("\\s+"))
  		lines2.map(x => Tuple2(x(0),x(1).toDouble))
  	}

  	// gives an IndexedSeq over Tuples e.g. (confidence, predicted, gold)
  	
  	val predictZipGold:IndexedSeq[Tuple2[Tuple2[String,Double],String]] = predict.zip(gold)

    val flatten:IndexedSeq[Tuple3[Double,String,String]] = predictZipGold.map(tuple=> (tuple._1._2, tuple._1._1, tuple._2))


  	val sortedByConf = flatten.sorted.reverse

  	val numInstance = gold.length

  	val bandLength = numInstance/3

  	val high = sortedByConf.slice(0, bandLength)
  	val mid = sortedByConf.slice(bandLength, 2*bandLength)
  	val low = sortedByConf.slice(2*bandLength, numInstance+1)

  	def check (instance: Tuple3[Double, String, String]):String = {
  		if (instance._2 == instance._3) "correct"
  		else "x"
  	}

  	def getAccuracy (seq: IndexedSeq[Tuple3[Double, String, String]]):Double = {

      seq.map(x=>check(x)).filter(x=>x=="correct").length/seq.length.toDouble


  	}

  	val Vector(highAc, midAc, lowAc) = Vector(high, mid, low).map(x=>getAccuracy(x))

  	println("High confidence accuracy: " + highAc)
  	println("Mid confidence accuracy: " + midAc)
  	println("Low confidence accuracy: " + lowAc)




}

}
