package fs.perso.music

object MusicMain {

  import fs.perso.music.domain._
  import Note._
  import Chord._
  import Guitar._

  def main(args: Array[String]): Unit = {
    println(chordMaj7(C, C))

//    harmonizedMajorScale()
//    harmonizedMinorScale()

//    flyMeToTheMoon()

    //    println(PitchedNote(E, 2) + MinorSixth)
//    println(PitchedNote(B, 2) + 1)
//    println(PitchedNote(A, 2) + 3)
//    println(PitchedNote(A, 2) + 2)

//    println((0 to 22).map(_.toString.padTo(4, ' ')).mkString(""))
//    println(
//      standardTunedGuitar
//        .take(22)
//        .transpose
//        .map(_.map(n => s"${n.note}${n.pitch}".padTo(3, ' ')).mkString("|"))
//        .mkString("\n")
//    )
  }

  def flyMeToTheMoon(): Unit = {
    printChord(A, C) // VI
    printChord(D, C) // II
    printChord(G, C) // V
    printChord(C, C) // I
    printChord(C, F) // V
    printChord(F, F) // I / IV
    printChord(B, C) // VII
    printChord(E, A) // V
  }

  def harmonizedMajorScale(): Unit = {
    val chords: List[List[String]] =
      for (root <- majorScaleOf(C)) yield {
        val c =
          for (note <- majorScaleOf(root))
            yield chordMaj7(note, root).mkString
        root.toString :: c
      }

    println(List("root", "I", "II", "III", "IV", "V", "VI", "VII").map(_.padTo(6, ' ')).mkString("\t"))
    chords.foreach(l => println(l.map(_.padTo(6, ' ')).mkString("\t")))
  }

  def harmonizedMinorScale(): Unit = {
    val chords: List[List[String]] =
      for (root <- majorScaleOf(C)) yield {
        val c =
          for (note <- minorHarmonicScaleOf(root))
            yield chordm7(note, root).mkString
        root.toString :: c
      }

    println(chordm7(D, C))
    println(chordm7(Eb, C))
    println(chordm7(F, C))

    println(List("root", "I", "II", "III", "IV", "V", "VI", "VII").map(_.padTo(6, ' ')).mkString("\t"))
    chords.foreach(l => println(l.map(_.padTo(6, ' ')).mkString("\t")))
  }

  def printChord(note: Note, root: Note): Unit = {
    val c = chordMaj7(note, root)
    println(c.toString + " - " + c.mkString)
  }

//  def chordComposition()

}
