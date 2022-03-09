package CodeWars

object Comparator {
  def comp(seq1: Seq[Int], seq2: Seq[Int]): Boolean =
    if (seq1 == null || seq2 == null) false
    else
      seq1.map(x => x * x).sorted == seq2.sorted
}
