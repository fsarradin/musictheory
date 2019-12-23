package fs.perso.music

import fs.perso.music.domain.Note._
import fs.perso.music.domain._

object Guitar {

  case class GuitarChord(tuning: PitchedNote) {
    def fret(n: Int): PitchedNote       = selectPitchedNote(tuning + n)
    def take(n: Int): List[PitchedNote] = (0 to n).map(fret).toList
    def drop(n: Int): GuitarChord       = GuitarChord(selectPitchedNote(tuning + n))
    def find(note: Note): (PitchedNote, Int) = {
      val interval = note - tuning.note
      (selectPitchedNote(tuning + interval), interval.semitone)
    }
  }

  case class Location(chord: Int, fret: Int)

  case class Guitar(chords: List[GuitarChord]) {
    def fret(n: Int): List[PitchedNote]       = chords.map(_.fret(n))
    def take(n: Int): List[List[PitchedNote]] = (0 to n).map(fret).toList

//    def find(note: Note, chord: Int): Location = {
//      val halfWidth = 3
//      val (n, fret) = chords.last.find(note)
//      val offset = Math.max(0, -halfWidth)
//      val result: List[List[PitchedNote]] =
//        chords.map(_.drop(fret + offset).take(2 * halfWidth))
//
//      (-halfWidth to halfWidth)
//        .map(_ + offset)
//        .map(f => chords.map(_.fret(f)))
//    }

  }

  val standardTunedGuitar =
    // E2–A2–D3–G3–B3–E4
    Guitar(
      List(
        GuitarChord(PitchedNote(E, 4)),
        GuitarChord(PitchedNote(B, 3)),
        GuitarChord(PitchedNote(G, 3)),
        GuitarChord(PitchedNote(D, 3)),
        GuitarChord(PitchedNote(A, 2)),
        GuitarChord(PitchedNote(E, 2))
      )
    )

}
