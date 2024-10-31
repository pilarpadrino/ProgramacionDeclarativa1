import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

// Definición global del tipo Bit
type Bit = 0 | 1

// Clase base para el Árbol de Huffman
abstract class ArbolHuffman {
  def peso: Int = this match {
    case HojaHuff(_, pesoHoja) => pesoHoja
    case RamaHuff(izq, dch) => izq.peso + dch.peso
  }

  def caracteres: List[Char] = this match {
    case HojaHuff(caracter, _) => List(caracter)
    case RamaHuff(izq, dch) => izq.caracteres ++ dch.caracteres
  }

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

  def contieneCaracter(caracter: Char): Boolean = this match {
    case HojaHuff(c, _) => c == caracter
    case RamaHuff(izq, dch) => izq.contieneCaracter(caracter) || dch.contieneCaracter(caracter)
  }

  def codificar(cadena: String): List[Bit] = {
    def caminoParaCaracter(subarbol: ArbolHuffman, caracter: Char, camino: List[Bit]): List[Bit] = subarbol match {
      case HojaHuff(c, _) if c == caracter => camino
      case RamaHuff(izq, dch) =>
        if (izq.contieneCaracter(caracter)) caminoParaCaracter(izq, caracter, camino :+ 0)
        else caminoParaCaracter(dch, caracter, camino :+ 1)
      case _ => List()
    }

    val bitsCodificados = ListBuffer[Bit]()
    for (caracter <- cadena) {
      bitsCodificados ++= caminoParaCaracter(this, caracter, List())
    }
    bitsCodificados.toList
  }
}

// Clases específicas de nodos para el árbol de Huffman
case class HojaHuff(caracter: Char, pesoHoja: Int) extends ArbolHuffman
case class RamaHuff(izq: ArbolHuffman, dch: ArbolHuffman) extends ArbolHuffman

// Objeto para construir el árbol de Huffman
object HuffmanTreeBuilder {
  def ListaCharsADistFrec(listaChars: List[Char]): List[(Char, Int)] = {
    listaChars.groupBy(identity).view.mapValues(_.size).toList
  }

  def DistribFrecAListaHojas(frecuencias: List[(Char, Int)]): List[ArbolHuffman] = {
    frecuencias.map { case (caracter, peso) => HojaHuff(caracter, peso) }
  }

  def combinar(nodos: List[ArbolHuffman]): List[ArbolHuffman] = {
    if (nodos.length <= 1) nodos
    else {
      val nodosOrdenados = nodos.sortBy(_.peso)
      val izq = nodosOrdenados.head
      val dch = nodosOrdenados.tail.head
      val nuevaRama = RamaHuff(izq, dch)
      nuevaRama :: nodosOrdenados.drop(2)
    }
  }

  def esListaSingleton(lista: List[ArbolHuffman]): Boolean = lista.length == 1

  def repetirHasta(
                    combinar: List[ArbolHuffman] => List[ArbolHuffman],
                    esListaSingleton: List[ArbolHuffman] => Boolean
                  )(lista: List[ArbolHuffman]): List[ArbolHuffman] = {
    if (esListaSingleton(lista)) lista
    else repetirHasta(combinar, esListaSingleton)(combinar(lista))
  }

  def crearArbolHuffman(cadena: String): ArbolHuffman = {
    val listaChars = cadena.toList
    val frecuencias = ListaCharsADistFrec(listaChars)
    val listaHojas = DistribFrecAListaHojas(frecuencias)
    repetirHasta(combinar, esListaSingleton)(listaHojas) match {
      case List(arbol) => arbol
      case _ => throw new IllegalStateException("Error al crear el árbol")
    }
  }
}

// Objeto para inicializar el árbol de Huffman desde una cadena
object ArbolHuffman {
  def apply(cadena: String): ArbolHuffman = HuffmanTreeBuilder.crearArbolHuffman(cadena)
}

// Objeto principal para ejecutar el programa
object Main {
  def main(args: Array[String]): Unit = {
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

    val listaChars = texto.toList
    val distribucionFrecuencias = HuffmanTreeBuilder.ListaCharsADistFrec(listaChars)
    println("Distribución de frecuencias para '" + texto + "': " + distribucionFrecuencias)

    val listaHojas = HuffmanTreeBuilder.DistribFrecAListaHojas(distribucionFrecuencias)
    println("Lista de hojas generada: " + listaHojas)

    val h1 = HojaHuff('a', 2)
    val h2 = HojaHuff('b', 3)
    val h3 = HojaHuff('c', 5)
    val l3: List[ArbolHuffman] = List(h1, h2, h3)
    val listaCombinada = HuffmanTreeBuilder.combinar(l3)
    println("Lista combinada de nodos: " + listaCombinada)

    val l1: List[ArbolHuffman] = List(h1, h2)
    val l2: List[ArbolHuffman] = List(h1)
    println("Es lista singleton (l1): " + HuffmanTreeBuilder.esListaSingleton(l1))
    println("Es lista singleton (l2): " + HuffmanTreeBuilder.esListaSingleton(l2))
  }
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