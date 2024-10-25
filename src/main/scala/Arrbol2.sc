// Paso 1: Definición de la estructura de datos para el árbol de Huffman
abstract class ArbolHuffman {
  def peso: Int = this match {
    case HojaHuff(_, peso) => peso
    case RamaHuff(izq, dch, _) => izq.peso + dch.peso
  }

  def caracteres: List[Char] = this match {
    case HojaHuff(caracter, _) => List(caracter)
    case RamaHuff(izq, dch, _) => izq.caracteres ++ dch.caracteres
  }

  def decodificar(bits: List[Bit]): String =
}

case class HojaHuff(caracter: Char, peso: Int) extends ArbolHuffman
case class RamaHuff(izq: ArbolHuffman, dch: ArbolHuffman, peso: Int) extends ArbolHuffman

// Paso 2: Definir el tipo Bit
type Bit = Int // 0 o 1

def cadenaAListaChars(cadena: String): List[Char] = cadena.toList

def listaCharsACadena(listaChar: List[Char]): String = listaChar.mkString





val arbolHuffmanAMano: ArbolHuffman =
  RamaHuff(
    HojaHuff ('S', 4),
    RamaHuff(
      HojaHuff('O', 3),
      RamaHuff(
        HojaHuff('E', 2),
        HojaHuff(' ', 2),
      )
    )
  )