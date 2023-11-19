import Oraculo._

package object ReconstCadenas{

  def reconstruirCadenaIngenuo(n: Int, o: Oraculo): Seq[Char] = {
    val alphabet = Seq('a', 'c', 'g', 't')

    def checkSubsequence(subseq: String): Boolean = {
      if (subseq.length == n) o(subseq)
      else false
    }

    val validSequence = alphabet.foldLeft("") { (acc, char) =>
      if (checkSubsequence(acc + char)) acc + char
      else acc
    }

    validSequence.toSeq
  }

  // Función para la solución mejorada
  def reconstruirCadenaMejorado(n: Int, o: Oraculo): Seq[Char] = {
    val alphabet = Seq('a', 'c', 'g', 't')
    var currentSubsequence: Seq[Seq[Char]] = Seq(Seq.empty)

    // Generar subcadenas de longitud n y verificar si son subcadenas válidas según el oráculo
    for (k <- 1 to n) {
      var candidate = Seq.empty[Seq[Char]]
      for {
        s <- currentSubsequence
        c <- alphabet
      } {
        val newCandidate = s ++ Seq(c)
        if (o(newCandidate)) {
          candidate = candidate :+ newCandidate
        }
      }
      currentSubsequence = candidate
      if (currentSubsequence.isEmpty) return Seq.empty[Char]
    }

    currentSubsequence.headOption.getOrElse(Seq.empty[Char])
  }
}

/*
import Oraculo._

package object ReconstCadenas{

  def reconstruirCadenaIngenuo(n: Int, o: Oraculo): Seq[Char] = {
    val cadenaObjetivo = "acgtacgtagctagctagctagctagctagctagctagctagctagctagctagctagctagctagctagctagctagctagctagctagctagctaga"

    def findValidSubsequence(startIndex: Int): Option[Seq[Char]] = {
      val subsequence = cadenaObjetivo.slice(startIndex, startIndex + n)
      if (subsequence.length == n && o(subsequence)) Some(subsequence)
      else None
    }

    val validSubsequence = (0 to cadenaObjetivo.length - n).flatMap(i => findValidSubsequence(i)).headOption

    validSubsequence.getOrElse(Seq.empty[Char])
  }

  // Función para la solución mejorada
  def reconstruirCadenaMejorado(n: Int, o: Oraculo): Seq[Char] = {
    val alphabet = Seq('a', 'c', 'g', 't')
    var currentSubsequence: Seq[Seq[Char]] = Seq(Seq.empty)

    // Generar subcadenas de longitud n y verificar si son subcadenas válidas según el oráculo
    for (k <- 1 to n) {
      var candidate = Seq.empty[Seq[Char]]
      for {
        s <- currentSubsequence
        c <- alphabet
      } {
        val newCandidate = s ++ Seq(c)
        if (o(newCandidate)) {
          candidate = candidate :+ newCandidate
        }
      }
      currentSubsequence = candidate
      if (currentSubsequence.isEmpty) return Seq.empty[Char]
    }

    currentSubsequence.headOption.getOrElse(Seq.empty[Char])
  }
}

/*

import Oraculo._

package object ReconstCadenas{

  // Función para la solución ingenua
  def reconstruirCadenaIngenuo(n: Int, o: Oraculo): Seq[Char] = {
    val alphabet = Seq('a', 'c', 'g', 't')

    // Función para verificar si una secuencia es subcadena
    def isSubsequence(s: Seq[Char]): Boolean = o(s)

    // Generar todas las posibles cadenas y probar si son subcadenas
    val result = alphabet.flatMap(c1 =>
      alphabet.flatMap(c2 =>
        alphabet.flatMap(c3 =>
          alphabet.map(c4 =>
            Seq(c1, c2, c3, c4)
          )
        )
      )
    ).find(isSubsequence)

    result.getOrElse(Seq.empty)
  }

  // Función para la solución mejorada
  def reconstruirCadenaMejorado(n: Int, o: Oraculo): Seq[Char] = {
    val alphabet = Seq('a', 'c', 'g', 't')

    // Función para verificar si una secuencia es subcadena
    def isSubsequence(s: Seq[Char]): Boolean = o(s)

    // Generar cadenas candidatas y consultar al oráculo
    val candidates = collection.mutable.Set.empty[Seq[Char]]
    val result = collection.mutable.Set.empty[Seq[Char]]

    for {
      k <- 1 to n
    } {
      candidates ++= candidates.flatMap(candidate =>
        alphabet.map(candidate :+ _)
      ).filter(isSubsequence)

      result ++= candidates.filter(_.length == n)
    }

    result.headOption.getOrElse(Seq.empty)
  }
}
*/