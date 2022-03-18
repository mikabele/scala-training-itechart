object TypeclassTask {

  // Why am I not a Typeclass?
  // TODO: Rework me so I am a typeclass
  trait HashCode[T] {
    def hash(value: T): Int
  }

  object HashCode {
    def apply[T](implicit instance: HashCode[T]): HashCode[T] = instance
  }

  implicit class HashCodeSyntax[A](x: A) {
    def hash(implicit instance: HashCode[A]): Int = instance.hash(x)
  }

  // TODO: make an instance for String
  // TODO: write "abc".hash to check everything

  object HashCodeInstances {
    implicit val stringHashCode: HashCode[String] = (value: String) => value.hashCode
  }

  def main(args: Array[String]): Unit = {
    import HashCodeInstances._

    println("abc".hash)
  }
}
