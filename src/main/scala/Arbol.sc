// Paso 1: Definición de la estructura de datos para el árbol de Huffman
abstract class ArbolHuffman {
  def peso: Int
  def caracteres: List[Char]
}

// Caso para una rama del árbol
case class RamaHuff(izq: ArbolHuffman, dch: ArbolHuffman) extends ArbolHuffman {
  def peso: Int = izq.peso + dch.peso
  def caracteres: List[Char] = izq.caracteres ++ dch.caracteres
}

// Caso para una hoja del árbol
case class HojaHuff(caracter: Char, peso: Int) extends ArbolHuffman {
  def caracteres: List[Char] = List(caracter)
}

// Paso 2: Definir el tipo Bit
type Bit = Int

// Paso 3: Función para calcular las frecuencias de los caracteres en una cadena
def calcularFrecuencias(cadena: String): List[(Char, Int)] = {
  cadena.toList.groupBy(identity).view.mapValues(_.length).toList
}

// Paso 4: Crear un árbol de Huffman desde una lista de frecuencias
def crearArbolHuffman(frecuencias: List[(Char, Int)]): ArbolHuffman = {
  def crearListaHojas(frecuencias: List[(Char, Int)]): List[ArbolHuffman] = {
    frecuencias.map { case (char, freq) => HojaHuff(char, freq) }.sortBy(_.peso)
  }

  def combinar(nodos: List[ArbolHuffman]): List[ArbolHuffman] = nodos match {
    case izq :: dch :: resto =>
      val nuevaRama = RamaHuff(izq, dch)
      (nuevaRama :: resto).sortBy(_.peso)
    case _ => nodos
  }

  def repetirHasta(lista: List[ArbolHuffman]): ArbolHuffman = {
    if (lista.size == 1) lista.head
    else repetirHasta(combinar(lista))
  }

  val listaHojas = crearListaHojas(frecuencias)
  repetirHasta(listaHojas)
}

// Paso 5: Función para codificar un mensaje usando el árbol de Huffman
def codificar(arbol: ArbolHuffman)(cadena: String): List[Bit] = {
  def codificarCaracter(arbol: ArbolHuffman, caracter: Char): List[Bit] = arbol match {
    case HojaHuff(c, _) if c == caracter => Nil
    case RamaHuff(izq, dch) =>
      if (izq.caracteres.contains(caracter)) 0 :: codificarCaracter(izq, caracter)
      else 1 :: codificarCaracter(dch, caracter)
  }
  cadena.toList.flatMap(c => codificarCaracter(arbol, c))
}

// Paso 6: Función para decodificar una lista de bits usando el árbol de Huffman
def decodificar(arbol: ArbolHuffman)(bits: List[Bit]): String = {
  def decodificarAux(arbol: ArbolHuffman, bits: List[Bit]): (Char, List[Bit]) = arbol match {
    case HojaHuff(c, _) => (c, bits)
    case RamaHuff(izq, dch) =>
      if (bits.head == 0) decodificarAux(izq, bits.tail) else decodificarAux(dch, bits.tail)
  }

  def recorrer(bits: List[Bit], actualArbol: ArbolHuffman): String = bits match {
    case Nil => ""
    case _ =>
      val (caracter, restoBits) = decodificarAux(actualArbol, bits)
      caracter + recorrer(restoBits, actualArbol)
  }

  recorrer(bits, arbol)
}

// Paso 7: Función para mostrar el árbol de Huffman
def mostrarArbol(arbol: ArbolHuffman): Unit = arbol match {
  case HojaHuff(caracter, peso) => println(s"Hoja: $caracter, Peso: $peso")
  case RamaHuff(izq, dch) =>
    println(s"Rama con peso: ${arbol.peso}")
    mostrarArbol(izq)
    mostrarArbol(dch)
}

// Ejemplo de uso

val cadena = "este es un ejemplo de codificación huffman"
val frecuencias = calcularFrecuencias(cadena)
println("Frecuencias de caracteres:")
frecuencias.foreach { case (char, freq) => println(s"'$char': $freq") }

val arbolHuffman = crearArbolHuffman(frecuencias)
println("\nÁrbol de Huffman:")
mostrarArbol(arbolHuffman)

val bitsCodificados = codificar(arbolHuffman)(cadena)
println("\nBits codificados:")
println(bitsCodificados)

val mensajeDecodificado = decodificar(arbolHuffman)(bitsCodificados)
println("\nMensaje decodificado:")
println(mensajeDecodificado)

