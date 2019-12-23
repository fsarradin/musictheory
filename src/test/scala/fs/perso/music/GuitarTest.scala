package fs.perso.music

import fs.perso.music.domain.PitchedNote
import org.scalatest.{FunSuiteLike, Matchers}

class GuitarTest extends FunSuiteLike with Matchers {

  import fs.perso.music.domain.Note._
  import Guitar._

  test("3rd fret on E2 chord is a G2") {
    val chord = GuitarChord(PitchedNote(E, 2))

    chord.fret(3) should be(PitchedNote(G, 2))
  }

}
