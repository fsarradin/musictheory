package fs.perso.music

object Note {
  /* semitone representation are used here in a view to only have int operations. */

  // true math modulo
  val mod: (Int, Int) => Int = Math.floorMod

  // -- NOTE ----
  sealed trait Note {
    val semitone: Int

    val tone: Double = semitone * 0.5

    def -(note: Note): Interval =
      intervals(mod(this.semitone - note.semitone, 12))

    def +(semitone: Int): List[Note] = semitoneToNote(mod(this.semitone + semitone, 12))
    def +(interval: Interval): List[Note] = semitoneToNote(mod(this.semitone + interval.semitone, 12))
  }

  case object C extends Note { override val semitone: Int = 0 }
  case object Cs extends Note {
    override val semitone: Int = 1
    override def toString = "C#"
  }
  case object Db extends Note { override val semitone: Int = 1 }
  case object D extends Note { override val semitone: Int = 2 }
  case object Ds extends Note {
    override val semitone: Int = 3
    override def toString = "D#"
  }
  case object Eb extends Note { override val semitone: Int = 3 }
  case object E extends Note { override val semitone: Int = 4 }
  case object F extends Note { override val semitone: Int = 5 }
  case object Fs extends Note {
    override val semitone: Int = 6
    override def toString = "F#"
  }
  case object Gb extends Note { override val semitone: Int = 6 }
  case object G extends Note { override val semitone: Int = 7 }
  case object Gs extends Note {
    override val semitone: Int = 8
    override def toString = "G#"
  }
  case object Ab extends Note { override val semitone: Int = 8 }
  case object A extends Note { override val semitone: Int = 9 }
  case object As extends Note {
    override val semitone: Int = 10
    override def toString = "A#"
  }
  case object Bb extends Note { override val semitone: Int = 10 }
  case object B extends Note { override val semitone: Int = 11 }

  val notes: List[Note] =
    List(C, Cs, Db, D, Ds, Eb, E, F, Fs, Gb, G, Gs, Ab, A, As, Bb, B)

  val semitoneToNote: List[List[Note]] =
    notes.groupBy(_.semitone).toList.sortBy(_._1).map(_._2)

  // -- INTERVAL ----
  sealed trait Interval {
    val semitone: Int

    val tone: Double = semitone * 0.5
  }

  case object Unison extends Interval { override val semitone: Int = 0 }
  case object MinorSecond extends Interval { override val semitone: Int = 1 }
  case object MajorSecond extends Interval { override val semitone: Int = 2 }
  case object MinorThird extends Interval { override val semitone: Int = 3 }
  case object MajorThird extends Interval { override val semitone: Int = 4 }
  case object PerfectFourth extends Interval { override val semitone: Int = 5 }
  case object FlatFifth extends Interval { override val semitone: Int = 6 }
  case object PerfectFifth extends Interval { override val semitone: Int = 7 }
  case object MinorSixth extends Interval { override val semitone: Int = 8 }
  case object MajorSixth extends Interval { override val semitone: Int = 9 }
  case object MinorSeventh extends Interval { override val semitone: Int = 10 }
  case object MajorSeventh extends Interval { override val semitone: Int = 11 }

  val intervals: List[Interval] =
    List(
      Unison,
      MinorSecond,
      MajorSecond,
      MinorThird,
      MajorThird,
      PerfectFourth,
      FlatFifth,
      PerfectFifth,
      MinorSixth,
      MajorSixth,
      MinorSeventh,
      MajorSeventh
    )

  // -- CHORD & SCALE ----
  case class Chord(notes: List[Note]) {
    def root: Note = notes.head
    val intervals: List[Interval] = notes.map(_ - root)
    def hasInterval(interval: Interval): Boolean = intervals.contains(interval)
    def mkString: String = {
      var st = root.toString
      if (hasInterval(MinorThird)) st += "m"
      if (hasInterval(MajorSeventh)) st += "M7"
      if (hasInterval(MinorSeventh)) st += "7"
      if (hasInterval(FlatFifth)) st += "b5"

      st
    }
  }

  val majorScale: List[Interval] =
    List(Unison, MajorSecond, MajorThird, PerfectFourth, PerfectFifth, MajorSixth, MajorSeventh)

  def majorScaleOf(root: Note): List[Note] = majorScale.map(i => (root + i).head)

  val CMajorScale: List[Note] = majorScaleOf(C)
}
