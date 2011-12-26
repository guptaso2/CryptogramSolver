import scala.io.Source

class Dictionary {
	val dict = Source.fromFile("src/US.dic").getLines.toList.map(s => s.toLowerCase()).sort((e1,e2) => (e1 compareTo e2) < 0).foldLeft(List[(String,Int)]())((ls,s) => (s,s.length()*s.length())::ls).toMap
	def getScore(decodedString: String): Int =
	{
	  val words = decodedString.split(Array(' ','\n')).toList
	  words.foldLeft(0)((score, word) => score + dict.getOrElse(word.filter(a => a.isLetter),0))
	}
}