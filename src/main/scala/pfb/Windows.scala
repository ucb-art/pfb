// See LICENSE for license details.

package pfb

/**
  * See [[https://en.wikipedia.org/wiki/Window_function#Blackman.E2.80.93Harris_window the wikipedia page]] for
  * more details
  */
object blackmanHarris {
  private val a0 = 0.35875
  private val a1 = 0.48829
  private val a2 = 0.14128
  private val a3 = 0.01168
  def apply(N: Int): Seq[Double] = Seq.tabulate(N) (i => {
    a0 -
      a1 * math.cos(2 * math.Pi * i.toDouble / (N - 1)) +
      a2 * math.cos(4 * math.Pi * i.toDouble / (N - 1)) -
      a3 * math.cos(6 * math.Pi * i.toDouble / (N - 1))
  })
  def apply(w: WindowConfig): Seq[Double] = blackmanHarris(w.outputWindowSize * w.numTaps)
}

/**
  * Copied (erroneously?) from SPLASH
  */
object sincHamming {
  def apply(size: Int): Seq[Double] = Seq.tabulate(size) (i=>{
    val term1 = 0.54 - 0.46 * breeze.numerics.cos(2 * scala.math.Pi * i.toDouble / (size.toDouble - 1))
    val sinc_periods = 1.0
    val scale = sinc_periods * 2.0 * scala.math.Pi * 2.0
    val term2 = breeze.numerics.sinc( (i.toDouble / size.toDouble - 0.5) * scale)
    term1 * term2
  })
  def apply(w: WindowConfig): Seq[Double] = sincHamming(w.outputWindowSize * w.numTaps)
}

object sincHanning {
  def apply(size: Int): Seq[Double] = Seq.tabulate(size) (i=>{
    val term1 = 0.50 - 0.50 * breeze.numerics.cos(2 * scala.math.Pi * i.toDouble / (size.toDouble - 1))
    val sinc_periods = 1.0
    val scale = sinc_periods * 2.0 * scala.math.Pi * 2.0
    val term2 = breeze.numerics.sinc( (i.toDouble / size.toDouble - 0.5) * scale)
    term1 * term2
  })
  def apply(w: WindowConfig): Seq[Double] = sincHanning(w.outputWindowSize * w.numTaps)
}


object userCoeff {
  def apply(w: WindowConfig): Seq[Double] = {
   Array(0.00091552734375000, -0.00003051757812500, -0.00003051757812500, -0.00003051757812500,
        -0.00003051757812500, -0.00003051757812500, -0.00003051757812500, -0.00003051757812500,
        -0.00003051757812500, -0.00003051757812500, -0.00003051757812500, -0.00003051757812500,
        -0.00003051757812500, -0.00003051757812500, -0.00003051757812500, -0.00003051757812500,
        -0.00003051757812500, -0.00003051757812500, -0.00003051757812500, -0.00003051757812500,
        -0.00003051757812500, -0.00003051757812500, -0.00003051757812500, -0.00003051757812500,
        -0.00003051757812500, -0.00003814697265625, -0.00003814697265625, -0.00003814697265625,
        -0.00003814697265625, -0.00003814697265625, -0.00003814697265625, -0.00003814697265625,
        -0.00003814697265625, -0.00003814697265625, -0.00003814697265625, -0.00003814697265625,
        -0.00003814697265625, -0.00004577636718750, -0.00004577636718750, -0.00004577636718750,
        -0.00004577636718750, -0.00004577636718750, -0.00004577636718750, -0.00004577636718750,
        -0.00004577636718750, -0.00004577636718750, -0.00004577636718750, -0.00004577636718750,
        -0.00005340576171875, -0.00005340576171875, -0.00005340576171875, -0.00005340576171875,
        -0.00005340576171875, -0.00005340576171875, -0.00005340576171875, -0.00005340576171875,
        -0.00005340576171875, -0.00005340576171875, -0.00005340576171875, -0.00006103515625000,
        -0.00006103515625000, -0.00006103515625000, -0.00006103515625000, -0.00006103515625000,
        -0.00006103515625000, -0.00006103515625000, -0.00006103515625000, -0.00006103515625000,
        -0.00006103515625000, -0.00006103515625000, -0.00006103515625000, -0.00006103515625000,
        -0.00006103515625000, -0.00006103515625000, -0.00006103515625000, -0.00006103515625000,
        -0.00006103515625000, -0.00006103515625000, -0.00006103515625000, -0.00006103515625000,
        -0.00006103515625000, -0.00006103515625000, -0.00006103515625000, -0.00006103515625000,
        -0.00006103515625000, -0.00006103515625000, -0.00006103515625000, -0.00006103515625000,
        -0.00006103515625000, -0.00006103515625000, -0.00006103515625000, -0.00006103515625000,
        -0.00006103515625000, -0.00005340576171875, -0.00005340576171875, -0.00005340576171875,
        -0.00005340576171875, -0.00005340576171875, -0.00005340576171875, -0.00005340576171875,
        -0.00005340576171875, -0.00004577636718750, -0.00004577636718750, -0.00004577636718750,
        -0.00004577636718750, -0.00004577636718750, -0.00004577636718750, -0.00003814697265625,
        -0.00003814697265625, -0.00003814697265625, -0.00003814697265625, -0.00003814697265625,
        -0.00003051757812500, -0.00003051757812500, -0.00003051757812500, -0.00003051757812500,
        -0.00002288818359375, -0.00002288818359375, -0.00002288818359375, -0.00002288818359375,
        -0.00001525878906250, -0.00001525878906250, -0.00001525878906250, -0.00001525878906250,
        -0.00000762939453125, -0.00000762939453125, -0.00000762939453125,  0.00000000000000000,
         0.00000000000000000,  0.00000000000000000,  0.00000762939453125,  0.00000762939453125,
         0.00000762939453125,  0.00001525878906250,  0.00001525878906250,  0.00001525878906250,
         0.00002288818359375,  0.00002288818359375,  0.00002288818359375,  0.00003051757812500,
         0.00003051757812500,  0.00003051757812500,  0.00003814697265625,  0.00003814697265625,
         0.00004577636718750,  0.00004577636718750,  0.00004577636718750,  0.00005340576171875,
         0.00005340576171875,  0.00005340576171875,  0.00006103515625000,  0.00006103515625000,
         0.00006866455078125,  0.00006866455078125,  0.00006866455078125,  0.00007629394531250,
         0.00007629394531250,  0.00008392333984375,  0.00008392333984375,  0.00008392333984375,
         0.00009155273437500,  0.00009155273437500,  0.00009155273437500,  0.00009918212890625,
         0.00009918212890625,  0.00009918212890625,  0.00010681152343750,  0.00010681152343750,
         0.00010681152343750,  0.00011444091796875,  0.00011444091796875,  0.00011444091796875,
         0.00012207031250000,  0.00012207031250000,  0.00012207031250000,  0.00012969970703125,
         0.00012969970703125,  0.00012969970703125,  0.00013732910156250,  0.00013732910156250,
         0.00013732910156250,  0.00013732910156250,  0.00014495849609375,  0.00014495849609375,
         0.00014495849609375,  0.00014495849609375,  0.00015258789062500,  0.00015258789062500,
         0.00015258789062500,  0.00015258789062500,  0.00015258789062500,  0.00015258789062500,
         0.00016021728515625,  0.00016021728515625,  0.00016021728515625,  0.00016021728515625,
         0.00016021728515625,  0.00016021728515625,  0.00016021728515625,  0.00016021728515625,
         0.00016021728515625,  0.00016021728515625,  0.00016021728515625,  0.00016021728515625,
         0.00016021728515625,  0.00016021728515625,  0.00016021728515625,  0.00016021728515625,
         0.00016021728515625,  0.00016021728515625,  0.00016021728515625,  0.00016021728515625,
         0.00016021728515625,  0.00016021728515625,  0.00015258789062500,  0.00015258789062500,
         0.00015258789062500,  0.00015258789062500,  0.00015258789062500,  0.00014495849609375,
         0.00014495849609375,  0.00014495849609375,  0.00014495849609375,  0.00013732910156250,
         0.00013732910156250,  0.00013732910156250,  0.00012969970703125,  0.00012969970703125,
         0.00012207031250000,  0.00012207031250000,  0.00012207031250000,  0.00011444091796875,
         0.00011444091796875,  0.00010681152343750,  0.00010681152343750,  0.00009918212890625,
         0.00009918212890625,  0.00009155273437500,  0.00009155273437500,  0.00008392333984375,
         0.00008392333984375,  0.00007629394531250,  0.00006866455078125,  0.00006866455078125,
         0.00006103515625000,  0.00006103515625000,  0.00005340576171875,  0.00004577636718750,
         0.00004577636718750,  0.00003814697265625,  0.00003051757812500,  0.00003051757812500,
         0.00002288818359375,  0.00001525878906250,  0.00000762939453125,  0.00000762939453125,
         0.00000000000000000, -0.00000762939453125, -0.00001525878906250, -0.00001525878906250,
        -0.00002288818359375, -0.00003051757812500, -0.00003814697265625, -0.00004577636718750,
        -0.00005340576171875, -0.00005340576171875, -0.00006103515625000, -0.00006866455078125,
        -0.00007629394531250, -0.00008392333984375, -0.00009155273437500, -0.00009155273437500,
        -0.00009918212890625, -0.00010681152343750, -0.00011444091796875, -0.00012207031250000,
        -0.00012969970703125, -0.00013732910156250, -0.00013732910156250, -0.00014495849609375,
        -0.00015258789062500, -0.00016021728515625, -0.00016784667968750, -0.00017547607421875,
        -0.00018310546875000, -0.00018310546875000, -0.00019073486328125, -0.00019836425781250,
        -0.00020599365234375, -0.00021362304687500, -0.00021362304687500, -0.00022125244140625,
        -0.00022888183593750, -0.00023651123046875, -0.00024414062500000, -0.00024414062500000,
        -0.00025177001953125, -0.00025939941406250, -0.00025939941406250, -0.00026702880859375,
        -0.00027465820312500, -0.00028228759765625, -0.00028228759765625, -0.00028991699218750,
        -0.00028991699218750, -0.00029754638671875, -0.00030517578125000, -0.00030517578125000,
        -0.00031280517578125, -0.00031280517578125, -0.00032043457031250, -0.00032043457031250,
        -0.00032806396484375, -0.00032806396484375, -0.00033569335937500, -0.00033569335937500,
        -0.00033569335937500, -0.00034332275390625, -0.00034332275390625, -0.00034332275390625,
        -0.00034332275390625, -0.00035095214843750, -0.00035095214843750, -0.00035095214843750,
        -0.00035095214843750, -0.00035095214843750, -0.00035095214843750, -0.00035858154296875,
        -0.00035858154296875, -0.00035858154296875, -0.00035858154296875, -0.00035858154296875,
        -0.00035095214843750, -0.00035095214843750, -0.00035095214843750, -0.00035095214843750,
        -0.00035095214843750, -0.00035095214843750, -0.00034332275390625, -0.00034332275390625,
        -0.00034332275390625, -0.00033569335937500, -0.00033569335937500, -0.00033569335937500,
        -0.00032806396484375, -0.00032806396484375, -0.00032043457031250, -0.00032043457031250,
        -0.00031280517578125, -0.00030517578125000, -0.00030517578125000, -0.00029754638671875,
        -0.00028991699218750, -0.00028228759765625, -0.00028228759765625, -0.00027465820312500,
        -0.00026702880859375, -0.00025939941406250, -0.00025177001953125, -0.00024414062500000,
        -0.00023651123046875, -0.00022888183593750, -0.00022125244140625, -0.00021362304687500,
        -0.00020599365234375, -0.00019836425781250, -0.00019073486328125, -0.00018310546875000,
        -0.00016784667968750, -0.00016021728515625, -0.00015258789062500, -0.00014495849609375,
        -0.00012969970703125, -0.00012207031250000, -0.00010681152343750, -0.00009918212890625,
        -0.00009155273437500, -0.00007629394531250, -0.00006866455078125, -0.00005340576171875,
        -0.00004577636718750, -0.00003051757812500, -0.00002288818359375, -0.00000762939453125,
         0.00000000000000000,  0.00001525878906250,  0.00003051757812500,  0.00003814697265625,
         0.00005340576171875,  0.00006866455078125,  0.00007629394531250,  0.00009155273437500,
         0.00010681152343750,  0.00011444091796875,  0.00012969970703125,  0.00014495849609375,
         0.00016021728515625,  0.00016784667968750,  0.00018310546875000,  0.00019836425781250,
         0.00021362304687500,  0.00022125244140625,  0.00023651123046875,  0.00025177001953125,
         0.00026702880859375,  0.00027465820312500,  0.00028991699218750,  0.00030517578125000,
         0.00032043457031250,  0.00032806396484375,  0.00034332275390625,  0.00035858154296875,
         0.00036621093750000,  0.00038146972656250,  0.00039672851562500,  0.00040435791015625,
         0.00041961669921875,  0.00043487548828125,  0.00044250488281250,  0.00045776367187500,
         0.00046539306640625,  0.00048065185546875,  0.00048828125000000,  0.00050354003906250,
         0.00051116943359375,  0.00052642822265625,  0.00053405761718750,  0.00054931640625000,
         0.00055694580078125,  0.00056457519531250,  0.00057983398437500,  0.00058746337890625,
         0.00059509277343750,  0.00060272216796875,  0.00061035156250000,  0.00061798095703125,
         0.00062561035156250,  0.00063323974609375,  0.00064086914062500,  0.00064849853515625,
         0.00065612792968750,  0.00066375732421875,  0.00067138671875000,  0.00067901611328125,
         0.00067901611328125,  0.00068664550781250,  0.00069427490234375,  0.00069427490234375,
         0.00070190429687500,  0.00070190429687500,  0.00070953369140625,  0.00070953369140625,
         0.00070953369140625,  0.00070953369140625,  0.00071716308593750,  0.00071716308593750,
         0.00071716308593750,  0.00071716308593750,  0.00071716308593750,  0.00071716308593750,
         0.00070953369140625,  0.00070953369140625,  0.00070953369140625,  0.00070953369140625,
         0.00070190429687500,  0.00070190429687500,  0.00069427490234375,  0.00068664550781250,
         0.00068664550781250,  0.00067901611328125,  0.00067138671875000,  0.00066375732421875,
         0.00065612792968750,  0.00064849853515625,  0.00064086914062500,  0.00063323974609375,
         0.00062561035156250,  0.00061798095703125,  0.00060272216796875,  0.00059509277343750,
         0.00058746337890625,  0.00057220458984375,  0.00055694580078125,  0.00054931640625000,
         0.00053405761718750,  0.00051879882812500,  0.00050354003906250,  0.00048828125000000,
         0.00048065185546875,  0.00046539306640625,  0.00044250488281250,  0.00042724609375000,
         0.00041198730468750,  0.00039672851562500,  0.00037384033203125,  0.00035858154296875,
         0.00034332275390625,  0.00032043457031250,  0.00030517578125000,  0.00028228759765625,
         0.00025939941406250,  0.00024414062500000,  0.00022125244140625,  0.00019836425781250,
         0.00017547607421875,  0.00015258789062500,  0.00012969970703125,  0.00010681152343750,
         0.00008392333984375,  0.00006103515625000,  0.00003814697265625,  0.00001525878906250,
        -0.00000762939453125, -0.00003051757812500, -0.00006103515625000, -0.00008392333984375,
        -0.00010681152343750, -0.00013732910156250, -0.00016021728515625, -0.00019073486328125,
        -0.00021362304687500, -0.00023651123046875, -0.00026702880859375, -0.00028991699218750,
        -0.00032043457031250, -0.00034332275390625, -0.00037384033203125, -0.00039672851562500,
        -0.00042724609375000, -0.00045776367187500, -0.00048065185546875, -0.00051116943359375,
        -0.00053405761718750, -0.00056457519531250, -0.00058746337890625, -0.00061798095703125,
        -0.00064849853515625, -0.00067138671875000, -0.00070190429687500, -0.00072479248046875,
        -0.00075531005859375, -0.00077819824218750, -0.00080871582031250, -0.00083160400390625,
        -0.00085449218750000, -0.00088500976562500, -0.00090789794921875, -0.00093078613281250,
        -0.00096130371093750, -0.00098419189453125, -0.00100708007812500, -0.00102996826171875,
        -0.00105285644531250, -0.00107574462890625, -0.00109863281250000, -0.00112152099609375,
        -0.00114440917968750, -0.00116729736328125, -0.00119018554687500, -0.00120544433593750,
        -0.00122833251953125, -0.00125122070312500, -0.00126647949218750, -0.00128173828125000,
        -0.00130462646484375, -0.00131988525390625, -0.00133514404296875, -0.00135040283203125,
        -0.00136566162109375, -0.00138092041015625, -0.00139617919921875, -0.00141143798828125,
        -0.00142669677734375, -0.00143432617187500, -0.00144958496093750, -0.00145721435546875,
        -0.00146484375000000, -0.00147247314453125, -0.00148010253906250, -0.00148773193359375,
        -0.00149536132812500, -0.00150299072265625, -0.00150299072265625, -0.00151062011718750,
        -0.00151062011718750, -0.00151824951171875, -0.00151824951171875, -0.00151824951171875,
        -0.00151824951171875, -0.00151062011718750, -0.00151062011718750, -0.00150299072265625,
        -0.00150299072265625, -0.00149536132812500, -0.00148773193359375, -0.00148010253906250,
        -0.00147247314453125, -0.00146484375000000, -0.00144958496093750, -0.00144195556640625,
        -0.00142669677734375, -0.00141143798828125, -0.00139617919921875, -0.00138092041015625,
        -0.00135803222656250, -0.00134277343750000, -0.00131988525390625, -0.00130462646484375,
        -0.00128173828125000, -0.00125885009765625, -0.00122833251953125, -0.00120544433593750,
        -0.00118255615234375, -0.00115203857421875, -0.00112152099609375, -0.00109100341796875,
        -0.00106048583984375, -0.00102996826171875, -0.00099182128906250, -0.00096130371093750,
        -0.00092315673828125, -0.00088500976562500, -0.00084686279296875, -0.00080871582031250,
        -0.00077056884765625, -0.00073242187500000, -0.00068664550781250, -0.00064086914062500,
        -0.00059509277343750, -0.00054931640625000, -0.00050354003906250, -0.00045776367187500,
        -0.00040435791015625, -0.00035858154296875, -0.00030517578125000, -0.00025177001953125,
        -0.00019836425781250, -0.00014495849609375, -0.00009155273437500, -0.00003051757812500,
         0.00002288818359375,  0.00008392333984375,  0.00014495849609375,  0.00020599365234375,
         0.00026702880859375,  0.00032806396484375,  0.00038909912109375,  0.00045776367187500,
         0.00051879882812500,  0.00058746337890625,  0.00065612792968750,  0.00072479248046875,
         0.00079345703125000,  0.00086212158203125,  0.00093078613281250,  0.00099945068359375,
         0.00107574462890625,  0.00114440917968750,  0.00122070312500000,  0.00129699707031250,
         0.00136566162109375,  0.00144195556640625,  0.00151824951171875,  0.00159454345703125,
         0.00167083740234375,  0.00174713134765625,  0.00183105468750000,  0.00190734863281250,
         0.00198364257812500,  0.00206756591796875,  0.00214385986328125,  0.00222778320312500,
         0.00230407714843750,  0.00238800048828125,  0.00247192382812500,  0.00254821777343750,
         0.00263214111328125,  0.00271606445312500,  0.00279998779296875,  0.00288391113281250,
         0.00296783447265625,  0.00305175781250000,  0.00312805175781250,  0.00321197509765625,
         0.00329589843750000,  0.00337982177734375,  0.00346374511718750,  0.00354766845703125,
         0.00363159179687500,  0.00371551513671875,  0.00379943847656250,  0.00388336181640625,
         0.00396728515625000,  0.00404357910156250,  0.00412750244140625,  0.00421142578125000,
         0.00429534912109375,  0.00437164306640625,  0.00445556640625000,  0.00453948974609375,
         0.00461578369140625,  0.00469970703125000,  0.00477600097656250,  0.00485992431640625,
         0.00493621826171875,  0.00501251220703125,  0.00508880615234375,  0.00516510009765625,
         0.00524139404296875,  0.00531768798828125,  0.00539398193359375,  0.00547027587890625,
         0.00554656982421875,  0.00561523437500000,  0.00569152832031250,  0.00576019287109375,
         0.00582885742187500,  0.00589752197265625,  0.00596618652343750,  0.00603485107421875,
         0.00610351562500000,  0.00616455078125000,  0.00623321533203125,  0.00629425048828125,
         0.00636291503906250,  0.00642395019531250,  0.00648498535156250,  0.00653839111328125,
         0.00659942626953125,  0.00665283203125000,  0.00671386718750000,  0.00676727294921875,
         0.00682067871093750,  0.00687408447265625,  0.00692749023437500,  0.00697326660156250,
         0.00701904296875000,  0.00707244873046875,  0.00711822509765625,  0.00715637207031250,
         0.00720214843750000,  0.00724792480468750,  0.00728607177734375,  0.00732421875000000,
         0.00736236572265625,  0.00740051269531250,  0.00743103027343750,  0.00746154785156250,
         0.00749969482421875,  0.00752258300781250,  0.00755310058593750,  0.00758361816406250,
         0.00760650634765625,  0.00762939453125000,  0.00765228271484375,  0.00767517089843750,
         0.00769042968750000,  0.00771331787109375,  0.00772857666015625,  0.00774383544921875,
         0.00775146484375000,  0.00776672363281250,  0.00777435302734375,  0.00778198242187500,
         0.00778961181640625,  0.00779724121093750,  0.00779724121093750,  0.00779724121093750,
         0.00779724121093750,  0.00779724121093750,  0.00779724121093750,  0.00778961181640625,
         0.00778198242187500,  0.00777435302734375,  0.00776672363281250,  0.00775146484375000,
         0.00774383544921875,  0.00772857666015625,  0.00771331787109375,  0.00769042968750000,
         0.00767517089843750,  0.00765228271484375,  0.00762939453125000,  0.00760650634765625,
         0.00758361816406250,  0.00755310058593750,  0.00752258300781250,  0.00749969482421875,
         0.00746154785156250,  0.00743103027343750,  0.00740051269531250,  0.00736236572265625,
         0.00732421875000000,  0.00728607177734375,  0.00724792480468750,  0.00720214843750000,
         0.00715637207031250,  0.00711822509765625,  0.00707244873046875,  0.00701904296875000,
         0.00697326660156250,  0.00692749023437500,  0.00687408447265625,  0.00682067871093750,
         0.00676727294921875,  0.00671386718750000,  0.00665283203125000,  0.00659942626953125,
         0.00653839111328125,  0.00648498535156250,  0.00642395019531250,  0.00636291503906250,
         0.00629425048828125,  0.00623321533203125,  0.00616455078125000,  0.00610351562500000,
         0.00603485107421875,  0.00596618652343750,  0.00589752197265625,  0.00582885742187500,
         0.00576019287109375,  0.00569152832031250,  0.00561523437500000,  0.00554656982421875,
         0.00547027587890625,  0.00539398193359375,  0.00531768798828125,  0.00524139404296875,
         0.00516510009765625,  0.00508880615234375,  0.00501251220703125,  0.00493621826171875,
         0.00485992431640625,  0.00477600097656250,  0.00469970703125000,  0.00461578369140625,
         0.00453948974609375,  0.00445556640625000,  0.00437164306640625,  0.00429534912109375,
         0.00421142578125000,  0.00412750244140625,  0.00404357910156250,  0.00396728515625000,
         0.00388336181640625,  0.00379943847656250,  0.00371551513671875,  0.00363159179687500,
         0.00354766845703125,  0.00346374511718750,  0.00337982177734375,  0.00329589843750000,
         0.00321197509765625,  0.00312805175781250,  0.00305175781250000,  0.00296783447265625,
         0.00288391113281250,  0.00279998779296875,  0.00271606445312500,  0.00263214111328125,
         0.00254821777343750,  0.00247192382812500,  0.00238800048828125,  0.00230407714843750,
         0.00222778320312500,  0.00214385986328125,  0.00206756591796875,  0.00198364257812500,
         0.00190734863281250,  0.00183105468750000,  0.00174713134765625,  0.00167083740234375,
         0.00159454345703125,  0.00151824951171875,  0.00144195556640625,  0.00136566162109375,
         0.00129699707031250,  0.00122070312500000,  0.00114440917968750,  0.00107574462890625,
         0.00099945068359375,  0.00093078613281250,  0.00086212158203125,  0.00079345703125000,
         0.00072479248046875,  0.00065612792968750,  0.00058746337890625,  0.00051879882812500,
         0.00045776367187500,  0.00038909912109375,  0.00032806396484375,  0.00026702880859375,
         0.00020599365234375,  0.00014495849609375,  0.00008392333984375,  0.00002288818359375,
        -0.00003051757812500, -0.00009155273437500, -0.00014495849609375, -0.00019836425781250,
        -0.00025177001953125, -0.00030517578125000, -0.00035858154296875, -0.00040435791015625,
        -0.00045776367187500, -0.00050354003906250, -0.00054931640625000, -0.00059509277343750,
        -0.00064086914062500, -0.00068664550781250, -0.00073242187500000, -0.00077056884765625,
        -0.00080871582031250, -0.00084686279296875, -0.00088500976562500, -0.00092315673828125,
        -0.00096130371093750, -0.00099182128906250, -0.00102996826171875, -0.00106048583984375,
        -0.00109100341796875, -0.00112152099609375, -0.00115203857421875, -0.00118255615234375,
        -0.00120544433593750, -0.00122833251953125, -0.00125885009765625, -0.00128173828125000,
        -0.00130462646484375, -0.00131988525390625, -0.00134277343750000, -0.00135803222656250,
        -0.00138092041015625, -0.00139617919921875, -0.00141143798828125, -0.00142669677734375,
        -0.00144195556640625, -0.00144958496093750, -0.00146484375000000, -0.00147247314453125,
        -0.00148010253906250, -0.00148773193359375, -0.00149536132812500, -0.00150299072265625,
        -0.00150299072265625, -0.00151062011718750, -0.00151062011718750, -0.00151824951171875,
        -0.00151824951171875, -0.00151824951171875, -0.00151824951171875, -0.00151062011718750,
        -0.00151062011718750, -0.00150299072265625, -0.00150299072265625, -0.00149536132812500,
        -0.00148773193359375, -0.00148010253906250, -0.00147247314453125, -0.00146484375000000,
        -0.00145721435546875, -0.00144958496093750, -0.00143432617187500, -0.00142669677734375,
        -0.00141143798828125, -0.00139617919921875, -0.00138092041015625, -0.00136566162109375,
        -0.00135040283203125, -0.00133514404296875, -0.00131988525390625, -0.00130462646484375,
        -0.00128173828125000, -0.00126647949218750, -0.00125122070312500, -0.00122833251953125,
        -0.00120544433593750, -0.00119018554687500, -0.00116729736328125, -0.00114440917968750,
        -0.00112152099609375, -0.00109863281250000, -0.00107574462890625, -0.00105285644531250,
        -0.00102996826171875, -0.00100708007812500, -0.00098419189453125, -0.00096130371093750,
        -0.00093078613281250, -0.00090789794921875, -0.00088500976562500, -0.00085449218750000,
        -0.00083160400390625, -0.00080871582031250, -0.00077819824218750, -0.00075531005859375,
        -0.00072479248046875, -0.00070190429687500, -0.00067138671875000, -0.00064849853515625,
        -0.00061798095703125, -0.00058746337890625, -0.00056457519531250, -0.00053405761718750,
        -0.00051116943359375, -0.00048065185546875, -0.00045776367187500, -0.00042724609375000,
        -0.00039672851562500, -0.00037384033203125, -0.00034332275390625, -0.00032043457031250,
        -0.00028991699218750, -0.00026702880859375, -0.00023651123046875, -0.00021362304687500,
        -0.00019073486328125, -0.00016021728515625, -0.00013732910156250, -0.00010681152343750,
        -0.00008392333984375, -0.00006103515625000, -0.00003051757812500, -0.00000762939453125,
         0.00001525878906250,  0.00003814697265625,  0.00006103515625000,  0.00008392333984375,
         0.00010681152343750,  0.00012969970703125,  0.00015258789062500,  0.00017547607421875,
         0.00019836425781250,  0.00022125244140625,  0.00024414062500000,  0.00025939941406250,
         0.00028228759765625,  0.00030517578125000,  0.00032043457031250,  0.00034332275390625,
         0.00035858154296875,  0.00037384033203125,  0.00039672851562500,  0.00041198730468750,
         0.00042724609375000,  0.00044250488281250,  0.00046539306640625,  0.00048065185546875,
         0.00048828125000000,  0.00050354003906250,  0.00051879882812500,  0.00053405761718750,
         0.00054931640625000,  0.00055694580078125,  0.00057220458984375,  0.00058746337890625,
         0.00059509277343750,  0.00060272216796875,  0.00061798095703125,  0.00062561035156250,
         0.00063323974609375,  0.00064086914062500,  0.00064849853515625,  0.00065612792968750,
         0.00066375732421875,  0.00067138671875000,  0.00067901611328125,  0.00068664550781250,
         0.00068664550781250,  0.00069427490234375,  0.00070190429687500,  0.00070190429687500,
         0.00070953369140625,  0.00070953369140625,  0.00070953369140625,  0.00070953369140625,
         0.00071716308593750,  0.00071716308593750,  0.00071716308593750,  0.00071716308593750,
         0.00071716308593750,  0.00071716308593750,  0.00070953369140625,  0.00070953369140625,
         0.00070953369140625,  0.00070953369140625,  0.00070190429687500,  0.00070190429687500,
         0.00069427490234375,  0.00069427490234375,  0.00068664550781250,  0.00067901611328125,
         0.00067901611328125,  0.00067138671875000,  0.00066375732421875,  0.00065612792968750,
         0.00064849853515625,  0.00064086914062500,  0.00063323974609375,  0.00062561035156250,
         0.00061798095703125,  0.00061035156250000,  0.00060272216796875,  0.00059509277343750,
         0.00058746337890625,  0.00057983398437500,  0.00056457519531250,  0.00055694580078125,
         0.00054931640625000,  0.00053405761718750,  0.00052642822265625,  0.00051116943359375,
         0.00050354003906250,  0.00048828125000000,  0.00048065185546875,  0.00046539306640625,
         0.00045776367187500,  0.00044250488281250,  0.00043487548828125,  0.00041961669921875,
         0.00040435791015625,  0.00039672851562500,  0.00038146972656250,  0.00036621093750000,
         0.00035858154296875,  0.00034332275390625,  0.00032806396484375,  0.00032043457031250,
         0.00030517578125000,  0.00028991699218750,  0.00027465820312500,  0.00026702880859375,
         0.00025177001953125,  0.00023651123046875,  0.00022125244140625,  0.00021362304687500,
         0.00019836425781250,  0.00018310546875000,  0.00016784667968750,  0.00016021728515625,
         0.00014495849609375,  0.00012969970703125,  0.00011444091796875,  0.00010681152343750,
         0.00009155273437500,  0.00007629394531250,  0.00006866455078125,  0.00005340576171875,
         0.00003814697265625,  0.00003051757812500,  0.00001525878906250,  0.00000000000000000,
        -0.00000762939453125, -0.00002288818359375, -0.00003051757812500, -0.00004577636718750,
        -0.00005340576171875, -0.00006866455078125, -0.00007629394531250, -0.00009155273437500,
        -0.00009918212890625, -0.00010681152343750, -0.00012207031250000, -0.00012969970703125,
        -0.00014495849609375, -0.00015258789062500, -0.00016021728515625, -0.00016784667968750,
        -0.00018310546875000, -0.00019073486328125, -0.00019836425781250, -0.00020599365234375,
        -0.00021362304687500, -0.00022125244140625, -0.00022888183593750, -0.00023651123046875,
        -0.00024414062500000, -0.00025177001953125, -0.00025939941406250, -0.00026702880859375,
        -0.00027465820312500, -0.00028228759765625, -0.00028228759765625, -0.00028991699218750,
        -0.00029754638671875, -0.00030517578125000, -0.00030517578125000, -0.00031280517578125,
        -0.00032043457031250, -0.00032043457031250, -0.00032806396484375, -0.00032806396484375,
        -0.00033569335937500, -0.00033569335937500, -0.00033569335937500, -0.00034332275390625,
        -0.00034332275390625, -0.00034332275390625, -0.00035095214843750, -0.00035095214843750,
        -0.00035095214843750, -0.00035095214843750, -0.00035095214843750, -0.00035095214843750,
        -0.00035858154296875, -0.00035858154296875, -0.00035858154296875, -0.00035858154296875,
        -0.00035858154296875, -0.00035095214843750, -0.00035095214843750, -0.00035095214843750,
        -0.00035095214843750, -0.00035095214843750, -0.00035095214843750, -0.00034332275390625,
        -0.00034332275390625, -0.00034332275390625, -0.00034332275390625, -0.00033569335937500,
        -0.00033569335937500, -0.00033569335937500, -0.00032806396484375, -0.00032806396484375,
        -0.00032043457031250, -0.00032043457031250, -0.00031280517578125, -0.00031280517578125,
        -0.00030517578125000, -0.00030517578125000, -0.00029754638671875, -0.00028991699218750,
        -0.00028991699218750, -0.00028228759765625, -0.00028228759765625, -0.00027465820312500,
        -0.00026702880859375, -0.00025939941406250, -0.00025939941406250, -0.00025177001953125,
        -0.00024414062500000, -0.00024414062500000, -0.00023651123046875, -0.00022888183593750,
        -0.00022125244140625, -0.00021362304687500, -0.00021362304687500, -0.00020599365234375,
        -0.00019836425781250, -0.00019073486328125, -0.00018310546875000, -0.00018310546875000,
        -0.00017547607421875, -0.00016784667968750, -0.00016021728515625, -0.00015258789062500,
        -0.00014495849609375, -0.00013732910156250, -0.00013732910156250, -0.00012969970703125,
        -0.00012207031250000, -0.00011444091796875, -0.00010681152343750, -0.00009918212890625,
        -0.00009155273437500, -0.00009155273437500, -0.00008392333984375, -0.00007629394531250,
        -0.00006866455078125, -0.00006103515625000, -0.00005340576171875, -0.00005340576171875,
        -0.00004577636718750, -0.00003814697265625, -0.00003051757812500, -0.00002288818359375,
        -0.00001525878906250, -0.00001525878906250, -0.00000762939453125,  0.00000000000000000,
         0.00000762939453125,  0.00000762939453125,  0.00001525878906250,  0.00002288818359375,
         0.00003051757812500,  0.00003051757812500,  0.00003814697265625,  0.00004577636718750,
         0.00004577636718750,  0.00005340576171875,  0.00006103515625000,  0.00006103515625000,
         0.00006866455078125,  0.00006866455078125,  0.00007629394531250,  0.00008392333984375,
         0.00008392333984375,  0.00009155273437500,  0.00009155273437500,  0.00009918212890625,
         0.00009918212890625,  0.00010681152343750,  0.00010681152343750,  0.00011444091796875,
         0.00011444091796875,  0.00012207031250000,  0.00012207031250000,  0.00012207031250000,
         0.00012969970703125,  0.00012969970703125,  0.00013732910156250,  0.00013732910156250,
         0.00013732910156250,  0.00014495849609375,  0.00014495849609375,  0.00014495849609375,
         0.00014495849609375,  0.00015258789062500,  0.00015258789062500,  0.00015258789062500,
         0.00015258789062500,  0.00015258789062500,  0.00016021728515625,  0.00016021728515625,
         0.00016021728515625,  0.00016021728515625,  0.00016021728515625,  0.00016021728515625,
         0.00016021728515625,  0.00016021728515625,  0.00016021728515625,  0.00016021728515625,
         0.00016021728515625,  0.00016021728515625,  0.00016021728515625,  0.00016021728515625,
         0.00016021728515625,  0.00016021728515625,  0.00016021728515625,  0.00016021728515625,
         0.00016021728515625,  0.00016021728515625,  0.00016021728515625,  0.00016021728515625,
         0.00015258789062500,  0.00015258789062500,  0.00015258789062500,  0.00015258789062500,
         0.00015258789062500,  0.00015258789062500,  0.00014495849609375,  0.00014495849609375,
         0.00014495849609375,  0.00014495849609375,  0.00013732910156250,  0.00013732910156250,
         0.00013732910156250,  0.00013732910156250,  0.00012969970703125,  0.00012969970703125,
         0.00012969970703125,  0.00012207031250000,  0.00012207031250000,  0.00012207031250000,
         0.00011444091796875,  0.00011444091796875,  0.00011444091796875,  0.00010681152343750,
         0.00010681152343750,  0.00010681152343750,  0.00009918212890625,  0.00009918212890625,
         0.00009918212890625,  0.00009155273437500,  0.00009155273437500,  0.00009155273437500,
         0.00008392333984375,  0.00008392333984375,  0.00008392333984375,  0.00007629394531250,
         0.00007629394531250,  0.00006866455078125,  0.00006866455078125,  0.00006866455078125,
         0.00006103515625000,  0.00006103515625000,  0.00005340576171875,  0.00005340576171875,
         0.00005340576171875,  0.00004577636718750,  0.00004577636718750,  0.00004577636718750,
         0.00003814697265625,  0.00003814697265625,  0.00003051757812500,  0.00003051757812500,
         0.00003051757812500,  0.00002288818359375,  0.00002288818359375,  0.00002288818359375,
         0.00001525878906250,  0.00001525878906250,  0.00001525878906250,  0.00000762939453125,
         0.00000762939453125,  0.00000762939453125,  0.00000000000000000,  0.00000000000000000,
         0.00000000000000000, -0.00000762939453125, -0.00000762939453125, -0.00000762939453125,
        -0.00001525878906250, -0.00001525878906250, -0.00001525878906250, -0.00001525878906250,
        -0.00002288818359375, -0.00002288818359375, -0.00002288818359375, -0.00002288818359375,
        -0.00003051757812500, -0.00003051757812500, -0.00003051757812500, -0.00003051757812500,
        -0.00003814697265625, -0.00003814697265625, -0.00003814697265625, -0.00003814697265625,
        -0.00003814697265625, -0.00004577636718750, -0.00004577636718750, -0.00004577636718750,
        -0.00004577636718750, -0.00004577636718750, -0.00004577636718750, -0.00005340576171875,
        -0.00005340576171875, -0.00005340576171875, -0.00005340576171875, -0.00005340576171875,
        -0.00005340576171875, -0.00005340576171875, -0.00005340576171875, -0.00006103515625000,
        -0.00006103515625000, -0.00006103515625000, -0.00006103515625000, -0.00006103515625000,
        -0.00006103515625000, -0.00006103515625000, -0.00006103515625000, -0.00006103515625000,
        -0.00006103515625000, -0.00006103515625000, -0.00006103515625000, -0.00006103515625000,
        -0.00006103515625000, -0.00006103515625000, -0.00006103515625000, -0.00006103515625000,
        -0.00006103515625000, -0.00006103515625000, -0.00006103515625000, -0.00006103515625000,
        -0.00006103515625000, -0.00006103515625000, -0.00006103515625000, -0.00006103515625000,
        -0.00006103515625000, -0.00006103515625000, -0.00006103515625000, -0.00006103515625000,
        -0.00006103515625000, -0.00006103515625000, -0.00006103515625000, -0.00006103515625000,
        -0.00006103515625000, -0.00005340576171875, -0.00005340576171875, -0.00005340576171875,
        -0.00005340576171875, -0.00005340576171875, -0.00005340576171875, -0.00005340576171875,
        -0.00005340576171875, -0.00005340576171875, -0.00005340576171875, -0.00005340576171875,
        -0.00004577636718750, -0.00004577636718750, -0.00004577636718750, -0.00004577636718750,
        -0.00004577636718750, -0.00004577636718750, -0.00004577636718750, -0.00004577636718750,
        -0.00004577636718750, -0.00004577636718750, -0.00004577636718750, -0.00003814697265625,
        -0.00003814697265625, -0.00003814697265625, -0.00003814697265625, -0.00003814697265625,
        -0.00003814697265625, -0.00003814697265625, -0.00003814697265625, -0.00003814697265625,
        -0.00003814697265625, -0.00003814697265625, -0.00003814697265625, -0.00003051757812500,
        -0.00003051757812500, -0.00003051757812500, -0.00003051757812500, -0.00003051757812500,
        -0.00003051757812500, -0.00003051757812500, -0.00003051757812500, -0.00003051757812500,
        -0.00003051757812500, -0.00003051757812500, -0.00003051757812500, -0.00003051757812500,
        -0.00003051757812500, -0.00003051757812500, -0.00003051757812500, -0.00003051757812500,
        -0.00003051757812500, -0.00003051757812500, -0.00003051757812500, -0.00003051757812500,
        -0.00003051757812500, -0.00003051757812500, -0.00003051757812500,  0.00091552734375000)
  }
}

