object SeqUtil {
  def sum[N](list: Seq[N])(implicit numeric: Numeric[N]): N =
    list.foldLeft(numeric.zero)(numeric.plus)
}
