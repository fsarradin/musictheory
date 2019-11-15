package fs.perso.music

import org.scalacheck.{Gen, Prop}
import org.scalatest.FunSuiteLike
import org.scalatestplus.scalacheck.Checkers

class NoteTest extends FunSuiteLike with Checkers {
  import Note.{ Note, notes }

  val noteGen: Gen[Note] = Gen.oneOf(notes)

  test("note plus 0 semitone gives the same note") {
    check(Prop.forAll(noteGen) { n => (n + 0).contains(n) })
  }

  test("note plus 12 semitones gives the same note") {
    check(Prop.forAll(noteGen) { n => (n + 12).contains(n) })
  }

  test("getting the interval between two notes and adding it to the second note give the first note") {
    check(Prop.forAll(noteGen, noteGen) { case (n1, n2) => (n2 + (n1 - n2)).contains(n1) })
  }

}
