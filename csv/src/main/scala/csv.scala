


trait CsvEncoder[A] {
  def encode(value: A): List[String]
}



object CsvEncoder {
  def pure[A](func: A => List[String]): CsvEncoder[A] =
    new CsvEncoder[A] {
      def encode(value: A): List[String] =
        func(value)
    }

  // implicit val stringEnc: CsvEncoder[String] =
  //   pure(str => List(str))

  // implicit val intEnc: CsvEncoder[Int] =
  //   pure(num => List(num.toString))

  // implicit val booleanEnc: CsvEncoder[Boolean] =
  //   pure(bool => List(if(bool) "yes" else "no"))
}



object Main extends Demo {
  def encodeCsv[A](value: A)(implicit enc: CsvEncoder[A]): List[String] =
    enc.encode(value)

  // println(encodeCsv("Dave"))
  // println(encodeCsv(123))
  // println(encodeCsv(true))
}

/**
  * book pages 27 - 28
  */
import shapeless.{::, HList, HNil}

def createCsvEncoder[A](f: A ⇒ List[String]) =
  new CsvEncoder[A] {
    override def encode(value: A) = f(value)
  }

implicit val hnilEncoder : CsvEncoder[HNil] =
  createCsvEncoder(hnil ⇒ Nil)

implicit def hlistEncoder[H, T <: HList](
  implicit
    hEnc : CsvEncoder[H],
    tEnc : CsvEncoder[T]

) : CsvEncoder[H :: T] = {
    createCsvEncoder {
      case h :: t ⇒ hEnc.encode(h) ++ tEnc.encode(t)
  }
}
