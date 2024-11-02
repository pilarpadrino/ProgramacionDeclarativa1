//Implementación árbol Huffman
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