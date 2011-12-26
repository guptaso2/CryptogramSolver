import scala.swing.FileChooser
import scala.io.Source
import scala.util.Random
import scala.collection.parallel

object crypto {

  val cipherObj = new Cipher
  val dict = new Dictionary
  
  def main(args: Array[String]): Unit = {
    val msg = readMessage

    val startTime = System.currentTimeMillis()
    //imperativeSolution(msg)
    val iterativeTime = System.currentTimeMillis()
    
    functionalSolution(msg)
    val scalaTime = System.currentTimeMillis()
    
    println("imperative solution time: " + (iterativeTime - startTime) + " milliseconds")
    println("parallelized functional solution time: " + (scalaTime - iterativeTime) + " milliseconds")
  }
  
  def imperativeSolution(msg: String)
  {
    var bestCipher = "abcdefghijklmnopqrstuvwxyz"
    var bestScore = 0
    
    val encodedMsg = cipherObj.encodeMessage(msg)
    
    for (i <- 0 until 100)
    {
      var parentCipher = cipherObj.genRandomCipher.mkString
      val parentDecoding = cipherObj.encodeString(encodedMsg, parentCipher)
      var parentScore = dict.getScore(parentDecoding)
      
      for(j <- - 0 until 2000)
      {
        val childCipher = mutateCipher(parentCipher)
        val childDecoding = cipherObj.encodeString(encodedMsg, childCipher)
        val childScore = dict.getScore(childDecoding)
        
        if (childScore > parentScore)
        {
          parentCipher = childCipher
          parentScore = childScore
        }
      }
      
      if (parentScore > bestScore)
      {
        bestCipher = parentCipher
        bestScore = parentScore
        println("switched cipher: " + bestScore)
      }
      println(i.toString() ++ "% done")
    }
    
    val bestDecoding = cipherObj.encodeString(encodedMsg, bestCipher)
    println("bestScore: " ++ bestScore.toString())
    println(bestDecoding)
  }

  def readMessage: String = 
  {
    val chooser = new FileChooser
    val result = chooser.showOpenDialog(null)
    try {
      if (result == FileChooser.Result.Approve)
        Source.fromFile(chooser.selectedFile).getLines.toList.foldLeft("")((s,x) => x ++ "\n" ++ s)
      else ""
    } finally {
      Source.fromFile(chooser.selectedFile).close
    }
  }
  
  def mutateCipher(cipher: String): String =
  {
    val x = Random.nextInt(26)
    val y = Random.nextInt(26)
    
    if (x == y)
      mutateCipher(cipher)
    else
    {
      val xChar = cipher(x)
      val yChar = cipher(y)
      cipher.replace(xChar, '?').replace(yChar, xChar).replace('?',yChar)
    }
  }
  
  def functionalSolution(msg: String)
  {    
    val encodedMsg = cipherObj.encodeMessage(msg)
        
    val randomCiphers = (1 to 100).toList.par.map(_ => cipherObj.genRandomCipher.mkString)
    
    val cipherScores = randomCiphers.map(x => findLocalMaximum(encodedMsg, x))
    
    cipherScores.map(x => println(x._2))
    
    val bestCipher = cipherScores.foldLeft("",0)(
        (bestSoFar,x) => if (x._2 > bestSoFar._2) x else bestSoFar )
      
    val decodedText = cipherObj.encodeString(encodedMsg,bestCipher._1)
    println(decodedText)
    
    val bestScore = bestCipher._2
    println("Score: " + bestScore)
  }
  
  def findLocalMaximum(encodedText: String, cipher: String): (String, Int) =
  {
    val mutationList = (1 to 2000).toList
    val baseScore = dict.getScore(cipherObj.encodeString(encodedText,cipher))
    
    val bestCipher = mutationList.foldLeft((cipher,baseScore))((x,_) => 
      {
        val mutatedCipher = mutateCipher(x._1)
        val mutatedScore = dict.getScore(cipherObj.encodeString(encodedText,mutatedCipher))
        if (mutatedScore > x._2) (mutatedCipher,mutatedScore) else x
      })
     
    bestCipher
  }  
}
