package fs.perso.music.domain

import fs.perso.music.domain.Note._

trait Flat {
  def isFlat: Boolean
}
trait IsFlat extends Flat {
  override def isFlat: Boolean = true
}

trait Sharp {
  def isSharp: Boolean
}
trait IsSharp extends Sharp {
  override def isSharp: Boolean = true
}

sealed trait Note extends Flat with Sharp {
  val semitone: Int
  val tone: Double = semitone * 0.5

  override def isFlat: Boolean  = false
  override def isSharp: Boolean = false

  def -(note: Note): Interval =
    intervals(mod(this.semitone - note.semitone, 12))

  def +(semitone: Int): List[Note]      = semitoneToNote(mod(this.semitone + semitone, 12))
  def +(interval: Interval): List[Note] = this + interval.semitone
}

object Note {
  /* semitone representation are used here in a view to only have int operations. */

  // true math modulo
  val mod: (Int, Int) => Int = Math.floorMod

  // -- NOTE ----
  case object C extends Note { override val semitone: Int = 0 }
  case object Cs extends Note with IsSharp {
    override val semitone: Int = 1
    override def toString      = "C#"
  }
  case object Db extends Note with IsFlat { override val semitone: Int = 1 }
  case object D  extends Note { override val semitone: Int             = 2 }
  case object Ds extends Note with IsSharp {
    override val semitone: Int = 3
    override def toString      = "D#"
  }
  case object Eb extends Note with IsFlat { override val semitone: Int = 3 }
  case object E  extends Note { override val semitone: Int             = 4 }
  case object F  extends Note { override val semitone: Int             = 5 }
  case object Fs extends Note with IsSharp {
    override val semitone: Int = 6
    override def toString      = "F#"
  }
  case object Gb extends Note with IsFlat { override val semitone: Int = 6 }
  case object G  extends Note { override val semitone: Int             = 7 }
  case object Gs extends Note with IsSharp {
    override val semitone: Int = 8
    override def toString      = "G#"
  }
  case object Ab extends Note with IsFlat { override val semitone: Int = 8 }
  case object A  extends Note { override val semitone: Int             = 9 }
  case object As extends Note with IsSharp {
    override val semitone: Int = 10
    override def toString      = "A#"
  }
  case object Bb extends Note with IsFlat { override val semitone: Int = 10 }
  case object B  extends Note { override val semitone: Int             = 11 }

  val notes: List[Note] =
    List(C, Cs, Db, D, Ds, Eb, E, F, Fs, Gb, G, Gs, Ab, A, As, Bb, B)

  val semitoneToNote: List[List[Note]] =
    notes.groupBy(_.semitone).toList.sortBy(_._1).map(_._2)

  def selectNote(notes: List[Note]): Note =
    notes
      .find(_.isFlat)
      .getOrElse(notes.head)

  def selectPitchedNote(notes: List[PitchedNote]): PitchedNote =
    notes
      .find(_.note.isFlat)
      .getOrElse(notes.head)
}

case class PitchedNote(note: Note, pitch: Int) {

  def +(semitone: Int): List[PitchedNote] = {
    val newPitch: Int = pitch + (note.semitone + semitone) / 12

    (note + semitone).map(n => PitchedNote(n, newPitch))
  }

  def +(interval: Interval): List[PitchedNote] = this + interval.semitone

}
