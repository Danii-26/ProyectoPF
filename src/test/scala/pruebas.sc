import ReconstCadenas._
import Oraculo._

//oraculo
val cadenaObjetivo = "acgtacgtagctagctagctagctagctagctagctagctagctagctagctagctagctagctagctagctagctagctagctagctagctagctagctaga"

val oraculo: Oraculo = (s: Seq[Char]) => cadenaObjetivo.contains(s.mkString)

//ReconstCadenas
// Prueba para reconstruirCadenaIngenuo
reconstruirCadenaIngenuo(50, oraculo)
// Debería retornar una subcadena de longitud 50 si se encuentra en la cadena objetivo

// Prueba para reconstruirCadenaMejorado
reconstruirCadenaMejorado(50, oraculo)
// También debería retornar una subcadena de longitud 50 si se encuentra en la cadena objetivo



