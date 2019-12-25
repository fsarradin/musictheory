package fs.perso.music

import fs.perso.music.domain.Chord
import fs.perso.music.domain.Interval._
import fs.perso.music.domain.Note._
import org.scalatest.{FunSuiteLike, Matchers}

class ChordTest extends FunSuiteLike with Matchers {

  test("Chord C-E is unison and major third") {
    Chord(List(C, E)).intervals shouldEqual(List(Unison, MajorThird))
  }

  test("Chord C-E-G is unison, major third, and perfect fifth") {
    Chord(List(C, E, G)).intervals shouldEqual(List(Unison, MajorThird, PerfectFifth))
  }

  test("Chord C-E-G-A-D is unison, major third, perfect fifth, major sixth, and major ninth") {
    Chord(List(C, E, G, A, D)).intervals shouldEqual(List(Unison, MajorThird, PerfectFifth, MajorSixth, MajorNinth))
  }

  test("Chord C-E-G-A-D hasInterval major sixth") {
    Chord(List(C, E, G, A, D)).hasInterval(MajorSixth) shouldEqual(true)
  }

  test("Chord C-E-G-A-D hasInterval major ninth") {
    Chord(List(C, E, G, A, D)).hasInterval(MajorNinth) shouldEqual(true)
  }

  test("Chord C-A-D hasIntervals unison, major sixth, and major ninth") {
    Chord(List(C, A, D)).hasIntervals(Unison, MajorSixth, MajorNinth) shouldEqual(true)
  }

  test("Chord C-E-G-A-D has no interval major seventh") {
    Chord(List(C, E, G, A, D)).hasInterval(MajorSeventh) shouldEqual(false)
  }

  test("should display C triad as C") {
    Chord(List(C, E, G)).mkString shouldEqual("C")
  }

  test("should display C minor triad as Cm") {
    Chord(List(C, Eb, G)).mkString shouldEqual("Cm")
  }

  test("should display C major seventh as CM7") {
    Chord(List(C, E, G, B)).mkString shouldEqual("CM7")
  }

  test("should display C dominant seventh as C7") {
    Chord(List(C, E, G, Bb)).mkString shouldEqual("C7")
  }

  test("should display C minor seventh as Cm7") {
    Chord(List(C, Eb, G, Bb)).mkString shouldEqual("Cm7")
  }

  test("should display C major sixth as C6") {
    Chord(List(C, E, G, A)).mkString shouldEqual("C6")
  }

  test("should display C minor sixth as Cm6") {
    Chord(List(C, Eb, G, A)).mkString shouldEqual("Cm6")
  }

  test("should display C diminished seventh as Cdim7") {
    Chord(List(C, Eb, Gb, A)).mkString shouldEqual("Cdim7")
  }

  test("should display C 6/9 as C6/9") {
    Chord(List(C, E, G, A, D)).mkString shouldEqual("C6/9")
  }

}
