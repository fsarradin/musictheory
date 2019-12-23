package fs.perso.music.domain

sealed trait Interval { outer =>
  val semitone: Int

  val tone: Double = semitone * 0.5

  def *(n: Int): Interval =
    new Interval {
      override val semitone: Int = outer.semitone * n
    }
}

object Interval {
  // -- INTERVAL ----
  // Names from https://en.wikipedia.org/wiki/Interval_(music)

  case object Unison            extends Interval { override val semitone: Int = 0 }
  case object MinorSecond       extends Interval { override val semitone: Int = 1 }
  case object MajorSecond       extends Interval { override val semitone: Int = 2 }
  case object MinorThird        extends Interval { override val semitone: Int = 3 }
  case object MajorThird        extends Interval { override val semitone: Int = 4 }
  case object PerfectFourth     extends Interval { override val semitone: Int = 5 }
  case object FlatFifth         extends Interval { override val semitone: Int = 6 }
  case object PerfectFifth      extends Interval { override val semitone: Int = 7 }
  case object MinorSixth        extends Interval { override val semitone: Int = 8 }
  case object MajorSixth        extends Interval { override val semitone: Int = 9 }
  case object MinorSeventh      extends Interval { override val semitone: Int = 10 }
  case object MajorSeventh      extends Interval { override val semitone: Int = 11 }
  case object Octave            extends Interval { override val semitone: Int = 12 }
  case object MinorNinth        extends Interval { override val semitone: Int = 13 }
  case object MajorNinth        extends Interval { override val semitone: Int = 14 }
  case object MinorTenth        extends Interval { override val semitone: Int = 15 }
  case object MajorTenth        extends Interval { override val semitone: Int = 16 }
  case object PerfectEleventh   extends Interval { override val semitone: Int = 17 }
  case object AugmentedEleventh extends Interval { override val semitone: Int = 18 }
  case object PerfectTwelfth    extends Interval { override val semitone: Int = 19 }
  case object MinorThirteenth   extends Interval { override val semitone: Int = 20 }
  case object MajorThirteenth   extends Interval { override val semitone: Int = 21 }
  case object MinorFourteenth   extends Interval { override val semitone: Int = 22 }
  case object MajorFourteenth   extends Interval { override val semitone: Int = 23 }

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

  val extendedIntervals: List[Interval] =
    intervals ++ List(
      Octave,
      MinorNinth,
      MajorNinth,
      MinorTenth,
      MajorTenth,
      PerfectEleventh,
      AugmentedEleventh,
      PerfectTwelfth,
      MinorThirteenth,
      MajorThirteenth,
      MinorFourteenth,
      MajorFourteenth
    )
}
