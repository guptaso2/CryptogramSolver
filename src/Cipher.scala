import scala.util.Random

class Cipher {
  val alphabet = "abcdefghijklmnopqrstuvwxyz".toList
  val cipherMap = (alphabet zip genRandomCipher).toMap

  def cipherRandomize(y: List[Char], x: Char): List[Char] =
  {
    val filtAlpha = alphabet.filter(e => !(x::y).contains(e))
    val nextCipher = filtAlpha(Random.nextInt(filtAlpha.length))
    nextCipher::y
  }
  
  def encodeChar(x: Char): Char = cipherMap.get(x) match
  {
    case Some(y) => y
    case None	 => x
  }
  
  def encodeMessage(s: String): String =
  {
	s.toLowerCase().map(encodeChar)
  }
  
  def encodeString(s: String, cipher: String): String =
  {
    val custMap = (alphabet zip cipher).toMap
    s.toLowerCase().map(s => encodeCustChar(s,custMap))
  }
  
  def encodeCustChar(x: Char, m: Map[Char,Char]): Char = m.get(x) match
  {
    case Some(y) => y
    case None	 => x
  }
  
  def genRandomCipher : List[Char] =
  {
    try
    {
      alphabet.foldLeft(List[Char]())(cipherRandomize)
    }
    catch
    {
      case e:java.lang.IllegalArgumentException => 
        alphabet.foldLeft(List[Char]())(cipherRandomize)
    }
  }
}