import scala.annotation.tailrec

abstract class ArbolHuff {
  def pesoArbol: Int = this match
    case HojaHuff(_, p) => p
    case RamaHuff(nodoIzq, nodoDcha) => nodoIzq.pesoArbol + nodoDcha.pesoArbol

  //Función que devuelve la lista de caracteres de un ArbolHuff
  def caracteres:List[Char]= this match
    case HojaHuff(c, _) => List(c)
    case RamaHuff(nodoIzq, nodoDcha) => nodoIzq.caracteres ++ nodoDcha.caracteres

  //Función que pasa de cadena de texto a lista de caracteres
  def cadenaAListaChars(cadena: String): List[Char] =
    @tailrec
    def cAlCharsAux(posicion: Int, listaAux: List[Char]): List[Char] = posicion match
      case p if p >= cadena.length => listaAux.reverse //caso base, hemos llegado al final de la cadena
      case _ => cAlCharsAux(posicion + 1, cadena.charAt(posicion) :: listaAux) // Agregamos el carácter a la listaAux y continuamos con la recursividad

    cAlCharsAux(0, Nil)

  def listaCharsACadena(listaCar: List[Char]): String =
    @tailrec
    def lCharsACAux(listaCar: List[Char], cadena: String): String = listaCar match
      case Nil => cadena // Cuando la lista está vacía, devuelve el resultado acumulado como String
      case head :: tail => lCharsACAux(tail, cadena + head) // Añadimos el carácter al StringBuilder y continuamos

    lCharsACAux(listaCar, "")
  type Bit = 0 | 1 //Creación del tipo de dato bit

  // Función que a partir de una lista de bits devuelve el texto
  def decodificar(bits: List[Bit]): String =
    @tailrec
    def decodificarAux(subArbol: ArbolHuff, restobits: List[Bit], resultado: List[Char]): String = (subArbol, restobits) match
      //Caso hoja, añadir caracter y empezar de nuevo
      case (HojaHuff(caracter, _), _) => decodificarAux(this, restobits, caracter :: resultado)
      //Caso rama, depende del bit 0 izq 1dcha
      case (RamaHuff(nodoIzq, _), 0 :: tail) => decodificarAux(nodoIzq, tail, resultado)
      case (RamaHuff(_, nodoDcha), 1 :: tail) => decodificarAux(nodoDcha, tail, resultado)
      //No quedan bits
      case _ => listaCharsACadena(resultado.reverse) //Invertimos y convertimos a cadena de texto

    decodificarAux(this, bits, Nil)

  //Determina si el árbol contiene o no un caracter
  def arbolcontiene(char:Char): Boolean = this match
    case HojaHuff(caracter, _) => caracter == char
    case RamaHuff(nodoIzq, nodoDcha) => nodoIzq.arbolcontiene(char) || nodoDcha.arbolcontiene(char)
  //Convierte el texto a una cadena de bits
  def codificar(cadena: String): List[Bit] =
    @tailrec
    def codificarAux(caracter: Char, arbol: ArbolHuff, listabits: List[Bit]): List[Bit] = arbol match
      case HojaHuff(c, _) if c == caracter => listabits
      case RamaHuff(nodoIzq, _) if nodoIzq.arbolcontiene(caracter) => codificarAux(caracter, nodoIzq, listabits :+ 0)
      case RamaHuff(_, nodoDcha) if nodoDcha.arbolcontiene(caracter) => codificarAux(caracter, nodoDcha, listabits :+ 1)
      case _ => throw new IllegalArgumentException("Carácter no está")
    //Convertir cada caracter de la cadena a binario y concatena la lista
    cadena.foldLeft(List[Bit]())((acumulador, char) => acumulador ++ codificarAux(char, this, Nil))

  // Convierte la lista de caracteres en distribución de frecuencias.
  def ListaCharsADistFrec(listaChar: List[Char]): List[(Char, Int)] =
    // Función auxiliar recursiva de cola
    @tailrec
    def ListaCharAux(listaChars: List[Char], listaDevolver: List[(Char, Int)]): List[(Char, Int)] = listaChars match
      case Nil => listaDevolver.reverse // Caso base: si la lista está vacía, devolvemos el acumulador
      case head :: tail =>
        listaDevolver.find(_._1 == head) match
          case Some((c, count)) =>
            // Si el carácter ya está en el acumulador, actualizamos la frecuencia
            ListaCharAux(tail, listaDevolver.filterNot(_._1 == head) :+ (c, count + 1))
          case None =>
            // Si el carácter no está, lo agregamos con frecuencia 1
            ListaCharAux(tail, (head, 1)::listaDevolver)

    // Llamada inicial con la lista de caracteres y una lista vacía como acumulador
    ListaCharAux(listaChar, List.empty)


  // Convierte la distribución en una lista de hojas ordenada
  def DistribFrecAListaHojas(frec: List[(Char, Int)]): List[HojaHuff] =
    @tailrec
    def insertar(hoja: HojaHuff, lista: List[HojaHuff], acumulado: List[HojaHuff]): List[HojaHuff] = lista match
      case Nil => acumulado.reverse :+ hoja // insertamos hoja al final del acumulado
      case head :: tail if hoja.peso <= head.peso => (acumulado.reverse :+ hoja) ::: lista // Insertamos la hoja en la posición sin invertir
      case head :: tail => insertar(hoja, tail, head :: acumulado) // Recorremos hasta encontrar la posición

    // Convertimos cada tupla a HojaHuff e insertamos en la lista ordenada
    frec.foldLeft(List.empty[HojaHuff]) { case (acc, (caracter, peso)) => insertar(HojaHuff(caracter, peso), acc, Nil)}

  // Crea un objeto RamaHuff integrando los dos ArbolHuff (izquierdo y derecho) que se le pasan como parámetros
  def creaRamaHuff(izq: ArbolHuff, dcha: ArbolHuff): RamaHuff =
    RamaHuff(izq, dcha)

  //Comprobar que la lista solo tenga un elemento
  def esListaSingleton(lista: List[ArbolHuff]): Boolean =
    lista.length == 1 //la lista tiene un elemento


  //Función que coge una lista de nodos de clase ArbolHuff ordenandolos por sus pesos de forma creciente y combinandolos
  def combinar(nodos: List[ArbolHuff]): List[ArbolHuff] =
    if nodos.length <= 1 then nodos
    else
      val (izq, dch) = (nodos.head, nodos.tail.head) //dos primeros nodos
      val nuevaRama = creaRamaHuff(izq, dch)
      val nuevalista = insertarConOrden(nuevaRama, nodos.tail.tail) //añade la rama a la lista
      combinar(nuevalista)

  def insertarConOrden(nuevaRama: ArbolHuff, lista: List[ArbolHuff]): List[ArbolHuff] = lista match
    case Nil => List(nuevaRama)
    case head :: tail =>
      if (nuevaRama.pesoArbol <= head.pesoArbol) nuevaRama :: lista
      else  head :: insertarConOrden(nuevaRama,tail )

  def repetirHasta(combinar: List[ArbolHuff] => List[ArbolHuff], esListaSingleton: List[ArbolHuff] => Boolean)(lista: List[ArbolHuff]): List[ArbolHuff] = {
    if (esListaSingleton(lista)) lista // Caso base
    else {
      val nuevalista = combinar(lista)
      repetirHasta(combinar, esListaSingleton)(nuevalista)
    } // combina la lista
  }
  //Crear el Arbol de Huffman
  def crearArbolHuffman(cadena:String):ArbolHuff=
    //Lista de caracteres
    val listaChars= cadenaAListaChars(cadena)
    //frecuencias
    val frecuencias=ListaCharsADistFrec(listaChars)
    //lista de hojas segun el peso
    val listaHojas = DistribFrecAListaHojas(frecuencias)
    //Arbol combinando nodos
    repetirHasta(combinar, esListaSingleton)(listaHojas) match
      case List(arbol)=> arbol //devuelve el arbol completo
      case _ => throw new IllegalStateException("Error al crear el arbol")
}

object ArbolHuff {
  def apply(cadena: String): ArbolHuff =
    new ArbolHuff {
      override def crearArbolHuffman(cadena: String): ArbolHuff = super.crearArbolHuffman(cadena)
    }crearArbolHuffman(cadena)

  type Bit = 0 | 1
  type TablaCodigos = List[(Char, List[Bit])]

  // Transforma un árbol de Huffman en una tabla de codificación
  def deArbolATabla(arbol: ArbolHuff): TablaCodigos =
    def recorrer(arbol: ArbolHuff, codigoActual: List[Bit]): TablaCodigos = arbol match
      case HojaHuff(caracter, _) => List((caracter, codigoActual))
      case RamaHuff(nodoIzq, nodoDcha) =>
        recorrer(nodoIzq, codigoActual :+ 0) ::: recorrer(nodoDcha, codigoActual :+ 1)
    recorrer(arbol, Nil)


}

case class RamaHuff(nodoIzq:ArbolHuff, nodoDcha: ArbolHuff) extends ArbolHuff
case class HojaHuff(caracter:Char, peso: Int) extends ArbolHuff

@main
def main():Unit= {
  //MANERA DEFINITIVA DE CREAR EL ARBOL HUFFMAN
  // Prueba del object para construir el árbol usando el método apply
  val miArbol = ArbolHuff("huffman")
  println(miArbol)

  //INSTANCIAR EL ARBOL
  //Hojas
  val hojaEspacio = HojaHuff(' ', 7)
  val hojaA = HojaHuff('a', 4)
  val hojaE = HojaHuff('e', 4)
  val hojaF = HojaHuff('f', 3)
  val hojaH = HojaHuff('h', 2)
  val hojaI = HojaHuff('i', 2)
  val hojaM = HojaHuff('m', 2)
  val hojaN = HojaHuff('n', 2)
  val hojaS = HojaHuff('s', 2)
  val hojaT = HojaHuff('t', 2)
  val hojaL = HojaHuff('l', 1)
  val hojaO = HojaHuff('o', 1)
  val hojaP = HojaHuff('p', 1)
  val hojaR = HojaHuff('r', 1)
  val hojaU = HojaHuff('u', 1)
  val hojaX = HojaHuff('x', 1)
  //Ramas
  val ramaLO = RamaHuff(hojaL, hojaO)
  val ramaUX = RamaHuff(hojaU, hojaX)
  val ramaPR = RamaHuff(hojaP, hojaR)

  val ramaLRUX = RamaHuff(ramaLO, ramaUX)
  val ramaPRF = RamaHuff(ramaPR, hojaF)

  val ramaLRUXPRF = RamaHuff(ramaLRUX, ramaPRF)

  val ramaHT = RamaHuff(hojaH, hojaT)
  val ramaMN = RamaHuff(hojaM, hojaN)
  val ramaSI = RamaHuff(hojaS, hojaI)

  val ramaHTMN = RamaHuff(ramaHT, ramaMN)
  val ramaSIEA = RamaHuff(ramaSI, hojaE)

  val ramaA = RamaHuff(ramaSIEA, hojaA)
  val ramaESPACIO = hojaEspacio

  //Arbol
  val arbolHuffman: ArbolHuff = RamaHuff(ramaHTMN, ramaLRUXPRF)
  println(arbolHuffman.pesoArbol)
  val arbolHuff2: ArbolHuff = RamaHuff(HojaHuff('s', 4), RamaHuff(HojaHuff('o', 3), RamaHuff(HojaHuff('e', 2), HojaHuff(' ', 2))))
  println(arbolHuff2.pesoArbol)
  println(arbolHuff2.caracteres.mkString(", "))
  println(arbolHuff2.decodificar(List(0, 1, 0, 1, 1, 0)))
  println(arbolHuff2.codificar("soe"))
  // Prueba de lista de frecuencias de caracteres
  val texto = "huffman"
  val listaChars = texto.toList
  println("Distribución de frecuencias: " + arbolHuffman.ListaCharsADistFrec(listaChars))
  //Prueba de lista de Hojas
  println(arbolHuffman.DistribFrecAListaHojas(arbolHuffman.ListaCharsADistFrec(listaChars)))
  //Prueba de creación de rama
  val h1= HojaHuff('a', 2)
  val r1 = RamaHuff(HojaHuff('b',1), HojaHuff('c',1))
  println(arbolHuff2.creaRamaHuff(h1, r1))
  //Prueba de si es Lista Singleton
  val l1:List[ArbolHuff] = List(h1, r1)
  val l2:List[ArbolHuff] = List(h1)
  println(arbolHuff2.esListaSingleton(l1)) //Debería devolver False
  println(arbolHuff2.esListaSingleton(l2)) //Debería devolver True
  //Prueba la función combinar
  val h2 = HojaHuff('d', 3)
  val h3 = HojaHuff('e', 5)
  val l3:List[ArbolHuff] = List(h1, r1, h2, h3)
  println(arbolHuff2.combinar(l3))
  //Prueba de insertar con orden
  val l4:List[ArbolHuff] = List(h1, r1, h3)
  println(arbolHuff2.insertarConOrden(h2, l4))
  //Prueba repetirHasta y crearArbolHuffman
  val texto2 = "huffman"
  val arbolHuffman2: ArbolHuff = arbolHuffman.crearArbolHuffman(texto2)
  println(arbolHuffman2)
}