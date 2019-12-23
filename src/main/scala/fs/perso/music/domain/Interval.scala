package fs.perso.music.domain

sealed trait Interval { outer =>
  val cents: Int
//  val semitone: Int

  val tone: Double = cents / 50.0

  def *(n: Int): Interval =
    new Interval {
      override val cents: Int = outer.cents * n
    }
}

object Interval {
  // -- INTERVAL ----
  // Names from https://en.wikipedia.org/wiki/Interval_(music)

  case object Unison            extends Interval { override val cents: Int = 0 }
  case object MinorSecond       extends Interval { override val cents: Int = 100 }
  case object MajorSecond       extends Interval { override val cents: Int = 200 }
  case object MinorThird        extends Interval { override val cents: Int = 300 }
  case object MajorThird        extends Interval { override val cents: Int = 400 }
  case object PerfectFourth     extends Interval { override val cents: Int = 500 }
  case object FlatFifth         extends Interval { override val cents: Int = 600 }
  case object PerfectFifth      extends Interval { override val cents: Int = 700 }
  case object MinorSixth        extends Interval { override val cents: Int = 800 }
  case object MajorSixth        extends Interval { override val cents: Int = 900 }
  case object MinorSeventh      extends Interval { override val cents: Int = 1000 }
  case object MajorSeventh      extends Interval { override val cents: Int = 1100 }
  case object Octave            extends Interval { override val cents: Int = 1200 }
  case object MinorNinth        extends Interval { override val cents: Int = 1300 }
  case object MajorNinth        extends Interval { override val cents: Int = 1400 }
  case object MinorTenth        extends Interval { override val cents: Int = 1500 }
  case object MajorTenth        extends Interval { override val cents: Int = 1600 }
  case object PerfectEleventh   extends Interval { override val cents: Int = 1700 }
  case object AugmentedEleventh extends Interval { override val cents: Int = 1800 }
  case object PerfectTwelfth    extends Interval { override val cents: Int = 1900 }
  case object MinorThirteenth   extends Interval { override val cents: Int = 2000 }
  case object MajorThirteenth   extends Interval { override val cents: Int = 2100 }
  case object MinorFourteenth   extends Interval { override val cents: Int = 2200 }
  case object MajorFourteenth   extends Interval { override val cents: Int = 2300 }

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
