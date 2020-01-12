package fs.perso.music

import fs.perso.music.domain.Interval._
import fs.perso.music.domain.Note._

package object domain {

  val chromaticScale: List[Interval] = intervals

  val majorScale: List[Interval] =
    List(Unison, MajorSecond, MajorThird, PerfectFourth, PerfectFifth, MajorSixth, MajorSeventh)

  val minorHarmonicScale: List[Interval] =
    List(Unison, MajorSecond, MinorThird, PerfectFourth, PerfectFifth, MinorSixth, MajorSeventh)

  val majorPentatonicScale: List[Interval] =
    List(Unison, MajorSecond, MajorThird, PerfectFifth, MajorSixth)

  val minorPentatonicScale: List[Interval] =
    List(Unison, MajorSecond, MinorThird, PerfectFifth, MinorSeventh)

  def scaleOf(scale: List[Interval])(root: Note): List[Note] =
    scale.map(i => selectNote(root + i))

  val majorScaleOf: Note         => List[Note] = scaleOf(majorScale)
  val minorHarmonicScaleOf: Note => List[Note] = scaleOf(minorHarmonicScale)
  val chromaticScaleOf: Note     => List[Note] = scaleOf(chromaticScale)

  val CMajorScale: List[Note] = majorScaleOf(C)
}
