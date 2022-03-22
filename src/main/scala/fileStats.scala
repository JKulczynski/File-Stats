import scala.io.Source

object FileStats extends App {
  fileStats(args)

  def fileStats(args: Array[String]): Unit = {
    val filename = args(0)
    val command = args(1)
    val text = Source.fromFile(filename).getLines().toSeq
    val wordsStats = wordOcc(text)

    if (command == "--words") wordsStats.foreach(println)
    else if (command == "--lines") {
      linesOcc(text).foreach(println)
      println(wordsStats.size)
      wordsStats.foreach(println)
    }
  }

  private def wordOcc(text: Seq[String]): Map[String, Int] = {
    text
      .flatMap(line => line.split("\\s+"))
      .groupBy(identity)
      .map { case (word, wordList) => (word, wordList.length) }
  }

  private def linesOcc(text: Seq[String]): Seq[(String, Int)] = {
    text.map(line => (line, line.size))
  }
}
