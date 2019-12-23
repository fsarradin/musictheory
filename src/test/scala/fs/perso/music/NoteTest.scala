package fs.perso.music

import org.scalacheck.{Gen, Prop}
import org.scalatest.FunSuiteLike
import org.scalatestplus.scalacheck.Checkers

class NoteTest extends FunSuiteLike with Checkers {
  import fs.perso.music.domain._
  import Note._

  val noteGen: Gen[Note] = Gen.oneOf(notes)
  val pitchedNoteGen: Gen[PitchedNote] =
    for {
      note  <- noteGen
      pitch <- Gen.oneOf(Gen.negNum[Int], Gen.posNum[Int])
    } yield PitchedNote(note, pitch)

  test("note plus 0 cent gives the same note") {
    check(Prop.forAll(noteGen) { n =>
      (n + 0).contains(n)
    })
  }

  test("note plus 1200 cents gives the same note") {
    check(Prop.forAll(noteGen) { n =>
      (n + 1200).contains(n)
    })
  }

  test("getting the interval between two notes and adding it to the second note give the first note") {
    check(Prop.forAll(noteGen, noteGen) { case (n1, n2) => (n2 + (n1 - n2)).contains(n1) })
  }

  test("pitched note plu 0 cent gives the same note") {
    check(Prop.forAll(pitchedNoteGen) { n =>
      (n + 0).contains(n)
    })
  }

}
