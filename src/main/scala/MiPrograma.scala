// Definición del tipo Bit
type Bit = 0 | 1

// Clase base para el Árbol de Huffman
sealed abstract class ArbolHuffman {
  def peso: Int = this match {
    case HojaHuff(_, pesoHoja) => pesoHoja
    case RamaHuff(izq, dch) => izq.peso + dch.peso
  }

  def caracteres: List[Char] = this match {
    case HojaHuff(caracter, _) => List(caracter)
    case RamaHuff(izq, dch) => izq.caracteres ++ dch.caracteres
  }

  def decodificar(bits: List[Bit]): String = {
    @scala.annotation.tailrec
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
    cadena.toList.flatMap(caracter => caminoParaCaracter(this, caracter, List()))
  }
}

// Definición de las hojas y ramas del árbol de Huffman
case class HojaHuff(caracter: Char, pesoHoja: Int) extends ArbolHuffman
case class RamaHuff(izq: ArbolHuffman, dch: ArbolHuffman) extends ArbolHuffman

// Funciones de conversión
def cadenaAListaChars(cadena: String): List[Char] = cadena.toList
def listaCharsACadena(listaCar: List[Char]): String = listaCar.mkString

// Función para la distribución de frecuencias
def ListaCharsADistFrec(listaChar: List[Char]): List[(Char, Int)] = {
  listaChar.groupBy(identity).view.mapValues(_.size).toList
}

// Función para convertir la distribución de frecuencias a una lista de hojas
def DistribFrecAListaHojas(frec: List[(Char, Int)]): List[HojaHuff] = {
  frec.sortBy(_._2).map { case (caracter, peso) => HojaHuff(caracter, peso) }
}

// Función para crear una rama Huffman
def creaRamaHuff(izq: ArbolHuffman, dch: ArbolHuffman): RamaHuff = RamaHuff(izq, dch)

// Función para combinar nodos de una lista en una rama
def combinar(nodos: List[ArbolHuffman]): List[ArbolHuffman] = {
  if (nodos.length <= 1) nodos
  else {
    val nodosOrdenados = nodos.sortBy(_.peso)
    val izq = nodosOrdenados.head
    val dch = nodosOrdenados.tail.head
    creaRamaHuff(izq, dch) :: nodosOrdenados.drop(2)
  }
}

// Función para verificar si la lista contiene solo un elemento
def esListaSingleton(lista: List[ArbolHuffman]): Boolean = lista.length == 1

// Función currificada de orden superior
def repetirHasta(combinar: List[ArbolHuffman] => List[ArbolHuffman], esListaSingleton: List[ArbolHuffman] => Boolean)
                (lista: List[ArbolHuffman]): List[ArbolHuffman] = {
  if (esListaSingleton(lista)) lista
  else repetirHasta(combinar, esListaSingleton)(combinar(lista))
}

// Función para crear el árbol de Huffman desde una cadena de texto
def crearArbolHuffman(cadena: String): ArbolHuffman = {
  val listaChars = cadenaAListaChars(cadena)
  val frecuencias = ListaCharsADistFrec(listaChars)
  val listaHojas = DistribFrecAListaHojas(frecuencias)
  repetirHasta(combinar, esListaSingleton)(listaHojas) match {
    case List(arbol) => arbol
    case _ => throw new IllegalStateException("Error al crear el árbol")
  }
}

// Objeto de compañía para crear el árbol de Huffman usando el método apply
object ArbolHuffman {
  def apply(cadena: String): ArbolHuffman = crearArbolHuffman(cadena)
}

// Clase principal MiPrograma
class MiPrograma {
  def ejecutar(): Unit = {
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
}


// Al final del archivo `MiPrograma.scala`
object EjecutarPrograma extends App {
  val programa = new MiPrograma()
  programa.ejecutar()
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