package fs.perso.music

object MusicMain {

  import fs.perso.music.domain._
  import Note._
  import Interval._
  import Chord._
  import Guitar._

  def main(args: Array[String]): Unit = {
//    println(chordMaj7(C, C))
//    println(chordm7(C, C))

    println(Chord(List(C, E, G, B, D)).intervals)

//    harmonizedMajorScale()
//    println()
//    harmonizedMinorScale()

//    flyMeToTheMoon()

//    println(PitchedNote(E, 2) + MinorSixth)
//    println(PitchedNote(B, 2) + 100)
//    println(PitchedNote(A, 2) + 300)
//    println(PitchedNote(A, 2) + 200)
//
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

    println(List("root", "I", "II", "III", "IV", "V", "VI", "VII").map(_.padTo(6, ' ')).mkString("\t"))
    chords.foreach(l => println(l.map(_.padTo(6, ' ')).mkString("\t")))
  }

  def printChord(note: Note, root: Note): Unit = {
    val c = chordMaj7(note, root)
    println(c.toString + " - " + c.mkString)
  }

//  def chordComposition()

}
