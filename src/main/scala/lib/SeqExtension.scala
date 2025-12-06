object SeqExtension {
  extension[A](s: Seq[A]) {
    def split(p: A => Boolean): Seq[Seq[A]] = {
      def inner(
          s: Seq[A],
          p: A => Boolean,
          currentSeq: Seq[A],
          fullSeq: Seq[Seq[A]]
      ): Seq[Seq[A]] = {
        s match {
          case Nil => if (currentSeq.isEmpty) fullSeq else fullSeq :+ currentSeq
          case head :: rest =>
            if (p(head)) inner(rest, p, List(), fullSeq :+ currentSeq)
            else inner(rest, p, currentSeq :+ head, fullSeq)
        }
      }

      s match {
        case Nil => List()
        case _   => inner(s, p, List(), List())
      }
    }
  }
}
