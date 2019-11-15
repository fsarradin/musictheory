package fs.perso.music

object MusicMain {

  import Note._

  def main(args: Array[String]): Unit = {
    val chords: List[List[String]] =
      for (root <- majorScaleOf(C)) yield {
        val c =
          for (note <- majorScaleOf(root)) yield {
            chord(note, majorScaleOf(root)).mkString
          }
        root.toString :: c
      }

    println(List("root", "I", "II", "III", "IV", "V", "VI", "VII").map(_.padTo(6, ' ')).mkString("\t"))
    chords.foreach(l => println(l.map(_.padTo(6, ' ')).mkString("\t")))
  }

  def chord(root: Note, scale: List[Note]): Chord = {
    val i = scale.indexOf(root)

    Chord(
      List(
        root,
        scale(mod(i + 2, scale.size)),
        scale(mod(i + 4, scale.size)),
        scale(mod(i + 6, scale.size))
      )
    )
  }
}
