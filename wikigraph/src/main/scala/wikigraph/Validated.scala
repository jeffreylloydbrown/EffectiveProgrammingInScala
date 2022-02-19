package wikigraph

type Errors[T] = Seq[T]

type Validated[T, A] = Either[Errors[T], A]

object Validated:

  def zip[T, A, B](validatedA: Validated[T, A], validatedB: Validated[T, B]): Validated[T, (A, B)] =
    (validatedA, validatedB) match {
      case (Right(a), Right(b)) => Right((a, b))
      case (Left(a), Right(_)) => Left(a)
      case (Right(_), Left(b)) => Left(b)
      case (Left(a), Left(b)) => Left(a ++ b)
    }
  end zip

  def traverse[T, A, B](as: Seq[A])(f: A => Validated[T, B]): Validated[T, Seq[B]] =
    as.foldLeft[Validated[T, Seq[B]]](Right(Seq.empty)){ (validatedBs, a) =>
      zip(validatedBs, f(a)).map( (bs, b) => bs :+ b )
    }
  end traverse

end Validated
