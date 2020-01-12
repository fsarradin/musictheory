package fs.perso.music.domain

import fs.perso.music.domain.Interval._
import fs.perso.music.domain.Note._

case class Chord(notes: List[Note]) {
  def root: Note = notes.head
  val intervals: List[Interval] =
    notes.foldLeft(List[Interval]()) {
      case (Nil, n) => List(n - root)
      case (is, n) =>
        val interval = n - root
        if (interval.cents > is.last.cents)
          is :+ interval
        else
          is :+ extendedIntervals((interval.cents + 1200) / 100)
    }
  def hasInterval(interval: Interval): Boolean    = intervals.contains(interval)
  def hasIntervals(intervals: Interval*): Boolean = this.intervals.diff(intervals).isEmpty

  override def toString: String =
    s"${getClass.getSimpleName}(${notes.mkString(", ")})"

  def mkString: String =
    if (hasIntervals(Unison, MinorThird, FlatFifth, MajorSixth))
      root.toString + "dim7"
    else if (intervals == List(Unison, PerfectFifth))
      root.toString + "5"
    else {
      var st = root.toString
      if (hasInterval(MinorThird)) st += "m"
      if (hasInterval(MajorSixth) && hasInterval(MajorNinth) && !hasInterval(MajorSeventh)) st += "6/9"
      else {
        if (hasInterval(MajorSixth)) st += "6"
        if (hasInterval(MajorSeventh)) st += "M7"
        if (hasInterval(MinorSeventh)) st += "7"
        if (hasInterval(FlatFifth)) st += "b5"
        if (hasInterval(MinorSixth)) st += "#5"
      }

      st
    }
}

object Chord {

  def chordMaj7(note: Note, root: Note): Chord = {
    val scale = majorScaleOf(root)
    val i     = scale.indexOf(note)

    Chord(
      List(
        note,
        scale(mod(i + 2, scale.size)),
        scale(mod(i + 4, scale.size)),
        scale(mod(i + 6, scale.size))
      )
    )
  }

  def chordm7(note: Note, root: Note): Chord = {
    val scale = minorHarmonicScaleOf(root)
    val i     = scale.indexOf(note)

    Chord(
      List(
        note,
        scale(mod(i + 2, scale.size)),
        scale(mod(i + 4, scale.size)),
        scale(mod(i + 6, scale.size))
      )
    )
  }

}
