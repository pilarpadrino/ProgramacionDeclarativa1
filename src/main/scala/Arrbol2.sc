import scala.annotation.tailrec

// Clase base para el Árbol de Huffman
sealed abstract class ArbolHuffman {
  // Devuelve el peso de un ArbolHuffman
  def peso: Int = this match {
    case HojaHuff(_, pesoHoja) => pesoHoja
    case RamaHuff(izq, dch) => izq.peso + dch.peso
  }

  // Devuelve la lista de caracteres de un ArbolHuffman
  def caracteres: List[Char] = this match {
    case HojaHuff(caracter, _) => List(caracter)
    case RamaHuff(izq, dch) => izq.caracteres ++ dch.caracteres
  }

  // Decodifica una lista de bits y devuelve el mensaje resultante
  def decodificar(bits: List[Bit]): String = {
    @tailrec
    def decodAux(subarbol: ArbolHuffman, bitsRestantes: List[Bit], resultado: String): String = subarbol match {
      case HojaHuff(caracter, _) =>
        if (bitsRestantes.isEmpty) resultado + caracter
        else decodAux(this, bitsRestantes, resultado + caracter)
      case RamaHuff(izq, dch) =>
        bitsRestantes match {
          case Nil => resultado
          case 0 :: tail => decodAux(izq, tail, resultado)
          case 1 :: tail => decodAux(dch, tail, resultado)
        }
    }

    decodAux(this, bits, "")
  }

  // Comprueba si el árbol contiene un carácter específico
  def contieneCaracter(caracter: Char): Boolean = this match {
    case HojaHuff(c, _) => c == caracter
    case RamaHuff(izq, dch) => izq.contieneCaracter(caracter) || dch.contieneCaracter(caracter)
  }

  // Codifica una cadena en una lista de bits
  def codificar(cadena: String): List[Bit] = {
    def caminoParaCaracter(subarbol: ArbolHuffman, caracter: Char, camino: List[Bit]): List[Bit] = subarbol match {
      case HojaHuff(c, _) if c == caracter => camino
      case RamaHuff(izq, dch) =>
        if (izq.contieneCaracter(caracter)) caminoParaCaracter(izq, caracter, camino :+ 0)
        else caminoParaCaracter(dch, caracter, camino :+ 1)
      case _ => List()
    }

    cadena.toList.flatMap(caracter => caminoParaCaracter(this, caracter, List()))
  }
}

// Clases específicas de nodos para el árbol de Huffman
case class HojaHuff(caracter: Char, pesoHoja: Int) extends ArbolHuffman
case class RamaHuff(izq: ArbolHuffman, dch: ArbolHuffman) extends ArbolHuffman

// Convierte el string a una lista de caracteres
def cadenaAListaChars(cadena: String): List[Char] = cadena.toList

// Convierte una lista de caracteres en un string
def listaCharsACadena(listaCar: List[Char]): String = listaCar.mkString("")

// Definición del tipo Bit y TablaCodigos
type Bit = 0 | 1
type TablaCodigos = List[(Char, List[Bit])]

// Convierte la lista de caracteres en distribución de frecuencias
def ListaCharsADistFrec(listaChar: List[Char]): List[(Char, Int)] =
  listaChar.groupBy(identity).view.mapValues(_.size).toList

// Convierte la distribución en una lista de hojas ordenada
def DistribFrecAListaHojas(frec: List[(Char, Int)]): List[HojaHuff] =
  frec.sortBy(_._2).map { case (caracter, peso) => HojaHuff(caracter, peso) }

// Crea un objeto RamaHuff integrando dos ArbolHuffman
def creaRamaHuff(izq: ArbolHuffman, dch: ArbolHuffman): RamaHuff = RamaHuff(izq, dch)

// Combina los dos primeros elementos de la lista y preserva el orden según el peso
def combinar(nodos: List[ArbolHuffman]): List[ArbolHuffman] = {
  if (nodos.length <= 1) nodos
  else {
    val nodosOrdenados = nodos.sortBy(_.peso)
    val izq = nodosOrdenados.head
    val dch = nodosOrdenados.tail.head
    val nuevaRama = creaRamaHuff(izq, dch)
    (nuevaRama :: nodosOrdenados.drop(2)).sortBy(_.peso)
  }
}

// Comprueba si la lista tiene un solo elemento
def esListaSingleton(lista: List[ArbolHuffman]): Boolean = lista.length == 1

// Currificación para aplicar combinar hasta que haya un solo elemento
@tailrec
def repetirHasta(
                  combinar: List[ArbolHuffman] => List[ArbolHuffman],
                  esListaSingleton: List[ArbolHuffman] => Boolean
                )(lista: List[ArbolHuffman]): List[ArbolHuffman] = {
  if (esListaSingleton(lista)) lista
  else repetirHasta(combinar, esListaSingleton)(combinar(lista))
}

// Crear un árbol de Huffman desde una cadena de texto
def crearArbolHuffman(cadena: String): ArbolHuffman = {
  val listaChars = cadenaAListaChars(cadena)
  val frecuencias = ListaCharsADistFrec(listaChars)
  val listaHojas = DistribFrecAListaHojas(frecuencias)
  repetirHasta(combinar, esListaSingleton)(listaHojas) match {
    case List(arbol) => arbol
    case _ => throw new IllegalStateException("Error al crear el árbol")
  }
}

// Objeto de compañía con constructor alternativo para crear el árbol
object ArbolHuffman {
  def apply(cadena: String): ArbolHuffman = crearArbolHuffman(cadena)
}

// Programa principal
object MiPrograma extends App {
  val texto = "huffman"
  val arbolHuffman = ArbolHuffman(texto)

  println("Árbol de Huffman para '" + texto + "': " + arbolHuffman)
  println("Peso total del árbol: " + arbolHuffman.peso)
  println("Caracteres en el árbol: " + arbolHuffman.caracteres.mkString(", "))

  val mensaje = "man"
  val bitsCodificados = arbolHuffman.codificar(mensaje)
  println("Bits codificados para '" + mensaje + "': " + bitsCodificados)

  val mensajeDecodificado = arbolHuffman.decodificar(bitsCodificados)
  println("Mensaje decodificado desde los bits: " + mensajeDecodificado)
}








///val arbolHuffmanAMano: ArbolHuffman =
//  RamaHuff(
 //   HojaHuff ('S', 4),
//    RamaHuff(
//      HojaHuff('O', 3),
  //    RamaHuff(
//        HojaHuff('E', 2),
       // HojaHuff(' ', 2),
     // )
   // )
 // )