package fs.perso.music

object MusicMain {

  import Note._
  import Guitar._

  def main(args: Array[String]): Unit = {
    val chords: List[List[String]] =
      for (root <- majorScaleOf(C)) yield {
        val c =
          for (note <- majorScaleOf(root)) yield {
            chord7(note, majorScaleOf(root)).mkString
          }
        root.toString :: c
      }

    println(List("root", "I", "II", "III", "IV", "V", "VI", "VII").map(_.padTo(6, ' ')).mkString("\t"))
    chords.foreach(l => println(l.map(_.padTo(6, ' ')).mkString("\t")))

    printChord(A, C) // VI
    printChord(D, C) // II
    printChord(G, C) // V
    printChord(C, C) // I
    printChord(C, F) // V
    printChord(F, F) // I / IV
    printChord(B, C) // VII
    printChord(E, A) // V

//    println(PitchedNote(E, 2) + MinorSixth)
//    println(PitchedNote(B, 2) + 1)
//    println(PitchedNote(A, 2) + 3)
//    println(PitchedNote(A, 2) + 2)

    println((0 to 22).map(_.toString.padTo(4, ' ')).mkString(""))
    println(standardTunedGuitar.take(22).transpose.map(_.map(n => s"${n.note}${n.pitch}".padTo(3, ' ')).mkString("|")).mkString("\n"))
  }

  def printChord(note: Note, root: Note): Unit = {
    val c = chord7(note, majorScaleOf(root))
    println(c.toString + " - " + c.mkString)
  }

//  def chordComposition()

  def chord7(root: Note, scale: List[Note]): Chord = {
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
