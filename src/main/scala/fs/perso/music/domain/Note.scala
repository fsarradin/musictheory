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
  val cents: Int
  final val semitone: Double = cents / 100.0
  final val tone: Double     = cents / 50.0

  override def isFlat: Boolean  = false
  override def isSharp: Boolean = false

  def -(note: Note): Interval =
    Interval.intervals(mod((this.cents - note.cents) / 100, 12))

  def +(cents: Int): List[Note]         = semitoneToNote(mod((this.cents + cents) / 100, 12))
  def +(interval: Interval): List[Note] = this + interval.cents
}

object Note {
  /* cent representation are used here in a view to only have int operations. */

  // true math modulo
  val mod: (Int, Int) => Int = Math.floorMod

  // -- NOTE ----
  case object C extends Note { override val cents: Int = 0 }
  case object Cs extends Note with IsSharp {
    override val cents: Int = 100
    override def toString   = "C#"
  }
  case object Db extends Note with IsFlat { override val cents: Int = 100 }
  case object D  extends Note { override val cents: Int             = 200 }
  case object Ds extends Note with IsSharp {
    override val cents: Int = 300
    override def toString   = "D#"
  }
  case object Eb extends Note with IsFlat { override val cents: Int = 300 }
  case object E  extends Note { override val cents: Int             = 400 }
  case object F  extends Note { override val cents: Int             = 500 }
  case object Fs extends Note with IsSharp {
    override val cents: Int = 600
    override def toString   = "F#"
  }
  case object Gb extends Note with IsFlat { override val cents: Int = 600 }
  case object G  extends Note { override val cents: Int             = 700 }
  case object Gs extends Note with IsSharp {
    override val cents: Int = 800
    override def toString   = "G#"
  }
  case object Ab extends Note with IsFlat { override val cents: Int = 800 }
  case object A  extends Note { override val cents: Int             = 900 }
  case object As extends Note with IsSharp {
    override val cents: Int = 1000
    override def toString   = "A#"
  }
  case object Bb extends Note with IsFlat { override val cents: Int = 1000 }
  case object B  extends Note { override val cents: Int             = 1100 }

  val notes: List[Note] =
    List(C, Cs, Db, D, Ds, Eb, E, F, Fs, Gb, G, Gs, Ab, A, As, Bb, B)

  val semitoneToNote: List[List[Note]] =
    notes.groupBy(_.cents).toList.sortBy(_._1).map(_._2)

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

  def +(cents: Int): List[PitchedNote] = {
    val newPitch: Int = pitch + (note.cents + cents) / 1200

    (note + cents).map(n => PitchedNote(n, newPitch))
  }

  def +(interval: Interval): List[PitchedNote] = this + interval.cents

}
