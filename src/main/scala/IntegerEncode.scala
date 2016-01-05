import me.lemire.integercompression._
import me.lemire.integercompression.differential._
import me.lemire.integercompression._
import java.math._
import java.nio.ByteOrder
import java.nio.ByteBuffer

object IntegerEncoder extends App {

  test()

  def test() = {

    val dataList = List(9, 7, 593, 910, 955, 13, 603, 706, 635, 921, 56, 55, 86, 711, 237, 159, 178, 188, 637, 58, 514, 13, 365, 550, 131, 151, 599, 905, 240, 317, 313, 565, 153, 35, 911, 959, 466, 324, 391, 518, 8, 391, 423, 878, 706, 79, 424, 375, 386, 462, 212, 671, 186, 246, 631, 790, 753, 522, 588, 505, 626, 469, 939, 218, 915, 220, 946, 497, 203, 792, 400, 513, 869, 223, 909, 176, 428, 125, 946, 389, 15, 181, 313, 945, 285, 45, 673, 602, 924, 723, 711, 385, 389, 756, 248, 1, 803, 724, 695, 716, 23, 853, 291, 815, 235, 310, 116, 871, 860, 885, 851, 645, 741, 881, 690, 662, 24, 231, 481, 371, 805, 524, 295, 705, 872, 927, 255, 355, 626, 43, 756, 371, 198, 588, 115, 474, 347, 646, 812, 734, 312, 327, 242, 345, 309, 636, 755, 467, 694, 128, 635, 39, 783, 404, 428, 443, 673, 389, 130, 171, 763, 633, 433, 857, 144, 696, 425, 463, 669, 546, 392, 697, 413, 472, 493, 619, 887, 925, 903, 516, 529, 712, 807, 501, 833, 97, 854, 11, 741, 738, 722, 675, 935, 342, 623, 422, 757, 549, 618, 609, 163, 474, 749, 902, 548, 539)
    .sorted
    // val dataList = List(3, 23, 45, 69, 120, 145, 204, 323, 83, 90, 132, 423, 242, 60, 32, 87).sorted

    val data = dataList.toArray
    println(data.toList)

    // get deltas
    Delta.delta(data)

    // composite codec, second codec compress remaining data from first pass
    val codec = new Composition(new Simple16(), new VariableByte())
    // val codec = new IntegratedComposition(new IntegratedBinaryPacking(), new IntegratedVariableByte())


    // compress
    val compressed = new Array[Int](data.length+1024);
    val inputoffset = new IntWrapper(0);
    val outputoffset = new IntWrapper(0);
    codec.compress(data, inputoffset, data.length, compressed, outputoffset);

    println("ints " + compressed.toList.take(outputoffset.intValue()).map(Integer.toBinaryString(_)).mkString("-"))

    // ints => bytes
    val b = getBytes(compressed.toList.take(outputoffset.intValue()).toArray)

    // bytes => bigint
    val big = new BigInteger(b)
    println("bigi " + big)
    // bigint => bytes
    val bytes = big.toByteArray()

    // bytes => ints
    val ints = getInts(bytes)

    // uncompress
    val recovered = new Array[Int](data.length);
    val recoffset = new IntWrapper(0);
    codec.uncompress(ints, new IntWrapper(0), ints.length, recovered, recoffset);

    // reconstruct from deltas
    Delta.fastinverseDelta(recovered)
    println(recovered.toList)

    println(s"data matches: ${recovered.toList == dataList}")
    println(s"compressed from ${data.length} to ${outputoffset.intValue()} ints (${bytes.length} bytes) (${big.toString.length} chars) (${data.length * 3} chars)")
  }

  def getInts(bytes: Array[Byte]) = {
    val intBuffer2 = ByteBuffer.wrap(bytes).order(ByteOrder.LITTLE_ENDIAN).asIntBuffer()
    val ints = new Array[Int](bytes.length / 4)
    intBuffer2.get(ints)
    ints
  }

  def getBytes(ints: Array[Int]) = {
    val byteBuffer = ByteBuffer.allocate(4 * ints.length).order(ByteOrder.LITTLE_ENDIAN)
    val intBuffer = byteBuffer.asIntBuffer()
    intBuffer.put(ints)
    byteBuffer.array()
  }
}