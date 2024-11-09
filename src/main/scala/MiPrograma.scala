// Definición del tipo Bit
type Bit = 0 | 1

// Definición de la tabla de códigos
type TablaCodigos = List[(Char, List[Bit])]

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

  // Decodificar una lista de bits usando el árbol de Huffman
  def decodificar(bits: List[Bit]): String = {
    @scala.annotation.tailrec
    def decodAux(subarbol: ArbolHuffman, bitsRestantes: List[Bit], resultado: String): String = subarbol match {
      case HojaHuff(caracter, _) =>
        // Si llega a una hoja, agrega el carácter al resultado
        if (bitsRestantes.isEmpty) resultado + caracter
        else decodAux(this, bitsRestantes, resultado + caracter) // Re-inicia en el árbol principal si quedan bits por decodificar
      case RamaHuff(izq, dch) =>
        bitsRestantes match {
          case Nil => resultado // Si no quedan bits, retorna el resultado
          case 0 :: tail => decodAux(izq, tail, resultado)
          case 1 :: tail => decodAux(dch, tail, resultado)
        }
    }
    decodAux(this, bits, "") // Llama a la función auxiliar con el árbol, los bits y un resultado vacío
  }

  // Verificar si el árbol contiene un carácter específico
  def contieneCaracter(caracter: Char): Boolean = this match {
    case HojaHuff(c, _) => c == caracter
    case RamaHuff(izq, dch) => izq.contieneCaracter(caracter) || dch.contieneCaracter(caracter)
  }

  // Codificar una cadena en bits usando el árbol de Huffman
  def codificar(cadena: String): List[Bit] = {
    // Encuentra el camino (lista de bits) para un carácter en el árbol
    def caminoParaCaracter(subarbol: ArbolHuffman, caracter: Char, camino: List[Bit]): List[Bit] = subarbol match {
      case HojaHuff(c, _) if c == caracter => camino
      case RamaHuff(izq, dch) =>
        if (izq.contieneCaracter(caracter)) caminoParaCaracter(izq, caracter, camino :+ 0)
        else caminoParaCaracter(dch, caracter, camino :+ 1)
      case _ => List()
    }

    // Codificar cada carácter de la cadena
    def codificarLista(cadena: List[Char], acumulado: List[Bit]): List[Bit] = cadena match {
      case Nil => acumulado
      case cabeza :: cola =>
        codificarLista(cola, acumulado ++ caminoParaCaracter(this, cabeza, List()))
    }

    codificarLista(cadena.toList, List())
  }

  // Función para generar la tabla de códigos de Huffman
  def generarTablaDeCodigos: TablaCodigos = {
    def generarTabla(subarbol: ArbolHuffman, camino: List[Bit]): TablaCodigos = subarbol match {
      case HojaHuff(caracter, _) => List((caracter, camino))
      case RamaHuff(izq, dch) =>
        generarTabla(izq, camino :+ 0) ++ generarTabla(dch, camino :+ 1)
    }

    generarTabla(this, List()) // Inicia la generación de la tabla con un camino vacío
  }
}

// Definición de las hojas y ramas del árbol de Huffman
case class HojaHuff(caracter: Char, pesoHoja: Int) extends ArbolHuffman
case class RamaHuff(izq: ArbolHuffman, dch: ArbolHuffman) extends ArbolHuffman

// Funciones de conversión de cadenas a listas de caracteres y viceversa
def cadenaAListaChars(cadena: String): List[Char] = cadena.toList
def listaCharsACadena(listaCar: List[Char]): String = listaCar.mkString

// Función que calcula la frecuencia de cada carácter en una lista de caracteres
def ListaCharsADistFrec(listaChar: List[Char]): List[(Char, Int)] = {
  // Agrupa los caracteres por identidad y mapea cada grupo al par (caracter, tamaño del grupo)
  listaChar.groupBy(identity).map { case (caracter, ocurrencias) =>
    (caracter, ocurrencias.size) // Par (carácter, frecuencia)
  }.toList // Convierte el resultado final en una lista de pares
}

// Función que convierte la distribución de frecuencias en una lista de hojas de Huffman
def DistribFrecAListaHojas(frec: List[(Char, Int)]): List[HojaHuff] = {
  // Ordena la lista de frecuencias por peso y luego mapea cada par (caracter, peso) a una hoja de Huffman
  frec.sortBy(_._2).map { case (caracter, peso) =>
    HojaHuff(caracter, peso) // Crea una hoja para cada par
  }
}

// Función para crear una rama en el árbol de Huffman
def creaRamaHuff(izq: ArbolHuffman, dch: ArbolHuffman): RamaHuff = RamaHuff(izq, dch)

// Función para combinar nodos de una lista en una nueva rama
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


  def deArbolATabla(arbol: ArbolHuffman): TablaCodigos =
    def deArbolATablaAux(arbol: ArbolHuffman, bits: List[Bit]): TablaCodigos = arbol match
      case HojaHuff(char, weight) => List((char, bits)) // Si es hoja crea la lista de tuplas
      case RamaHuff(nodoIzq, nodoDch) => deArbolATablaAux(nodoIzq, bits :+ 0) ++ deArbolATablaAux(nodoDch, bits :+ 1) // Lo con nodo izquierdo añadiendo bit=0 y con la dcha con bit=1

    deArbolATablaAux(arbol, List.empty[Bit])

  def codificarTabla(tabla: TablaCodigos)(cadena: String): List[Bit] =

    def codificarCaracter(tabla: TablaCodigos)(char: Char): List[Bit] = tabla match // Para un caracter
      case Nil => List.empty[Bit] // Devuelve lista vacia de bits
      case (c, bits) :: tail if c == char => bits // Si encuentra ese caracter que devuelva los bits de la tupla
      case _ :: tail => codificarCaracter(tail)(char) // Si no lo encuentra sigue buscando

    def codificarCadena(cadena: List[Char]): List[Bit] = cadena match // Para toda la cadena
      case Nil => List.empty[Bit] // Si la cadena está vacía devuelve lista vacía de bits
      case char :: tail =>
        val bits = codificarCaracter(tabla)(char) // Obtiene los bits del carácter
        bits ++ codificarCadena(tail) // Concatena los bits y sigue con la parte de la cadena que queda

    codificarCadena(cadena.toList)

  def decodificarTabla(tabla: TablaCodigos)(bitsDados: List[Bit]): String =
    def decodificarCaracter(tabla: TablaCodigos)(bits: List[Bit]): (Char, List[Bit]) = tabla match
      case Nil => ('\u0000', bits) // Si llega al final no hace nada
      case (caracter, codigo) :: tail if bits.startsWith(codigo) => (caracter, bits.drop(codigo.length)) // Si bits empieza igual a la codificación del caracter lo devuelve
      case _ :: tail => decodificarCaracter(tail)(bits)

    @scala.annotation.tailrec
    def decodificarCadena(bits: List[Bit], resultado: String): String = bits match
      case Nil => resultado
      case _ =>
        val (caracter, rest) = decodificarCaracter(tabla)(bits) // Llamamos a decodificarCaracter
        decodificarCadena(rest, resultado + caracter) // Y luego continuamos con los bits sobrantes

    decodificarCadena(bitsDados, "")
}

// Clase principal MiPrograma con la integración de la tabla de códigos
class MiProgramaConPruebas {

  def ejecutar(): Unit = {
    println("=== Prueba de creación del Árbol de Huffman ===")
    val texto = "huffman"
    val arbolHuffman = try {
      ArbolHuffman(texto)
    } catch {
      case e: Exception =>
        println("Error en la creación del Árbol de Huffman: " + e.getMessage)
        return
    }
    println("Árbol de Huffman para '" + texto + "': " + arbolHuffman)
    println("Peso total del árbol: " + arbolHuffman.peso)
    println("Caracteres en el árbol: " + arbolHuffman.caracteres.mkString(", "))

    println("\n=== Prueba de codificación y decodificación ===")
    val mensaje = "man"
    val bitsCodificados = try {
      arbolHuffman.codificar(mensaje)
    } catch {
      case e: Exception =>
        println(s"Error al codificar el mensaje '$mensaje': " + e.getMessage)
        return
    }
    println(s"Bits codificados para '$mensaje': $bitsCodificados")

    val mensajeDecodificado = try {
      arbolHuffman.decodificar(bitsCodificados)
    } catch {
      case e: Exception =>
        println(s"Error al decodificar los bits '$bitsCodificados': " + e.getMessage)
        return
    }
    println(s"Mensaje decodificado desde los bits: $mensajeDecodificado")
    assert(mensaje == mensajeDecodificado, "Error en la decodificación")

    println("\n=== Prueba de tabla de códigos ===")
    val tablaCodigos = try {
      ArbolHuffman.deArbolATabla(arbolHuffman)
    } catch {
      case e: Exception =>
        println("Error al generar la tabla de códigos: " + e.getMessage)
        return
    }
    println("Tabla de códigos de Huffman:")
    tablaCodigos.foreach { case (caracter, codigo) =>
      println(s"Carácter: '$caracter' -> Código: ${codigo.mkString}")
    }

    println("\n=== Prueba de codificación usando la tabla de códigos ===")
    val bitsCodificadosConTabla = try {
      ArbolHuffman.codificarTabla(tablaCodigos)(mensaje)
    } catch {
      case e: Exception =>
        println(s"Error al codificar '$mensaje' usando la tabla de códigos: " + e.getMessage)
        return
    }
    println(s"Bits codificados para '$mensaje' usando la tabla: $bitsCodificadosConTabla")
    assert(bitsCodificados == bitsCodificadosConTabla, "Error en la codificación con la tabla")

    println("\n=== Prueba de decodificación usando la tabla de códigos ===")
    val mensajeDecodificadoConTabla = try {
      ArbolHuffman.decodificarTabla(tablaCodigos)(bitsCodificadosConTabla)
    } catch {
      case e: Exception =>
        println(s"Error al decodificar usando la tabla de códigos: " + e.getMessage)
        return
    }
    println(s"Mensaje decodificado usando la tabla: $mensajeDecodificadoConTabla")
    assert(mensaje == mensajeDecodificadoConTabla, "Error en la decodificación con la tabla")
  }
}

// Ejecutar el programa
object EjecutarPrograma extends App {
  val programa = new MiProgramaConPruebas()
  programa.ejecutar()
}

// Implementación de Decodificación con Árbol de Huffman manual
object DecodificacionApp extends App {
  // Definición de un árbol de Huffman manual
  val arbolHuffmanManual = RamaHuff(
    RamaHuff(HojaHuff('u', 1), RamaHuff(HojaHuff('m', 1), HojaHuff('h', 1))),
    RamaHuff(RamaHuff(HojaHuff('n', 1), HojaHuff('a', 1)), HojaHuff('f', 2))
  )

  // Lista de bits a decodificar que representarán "huffman"
  val bits: List[Bit] = List(0, 1, 1, 0, 0, 1, 1, 1, 1, 0, 1, 0, 1, 0, 1, 1, 0, 0)

  // Decodificación del mensaje
  val mensajeDecodificado = arbolHuffmanManual.decodificar(bits)

  // Impresión del mensaje decodificado
  println(s"Mensaje decodificado: $mensajeDecodificado")
}


