package fpspeedrun


/** cпециальный тип для паттерн-матчинга, гарантирующий непустой результат */
final case class Matched[T](get: T) extends AnyVal {
  def isEmpty = false
}
