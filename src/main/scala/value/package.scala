package object value {
  sealed trait Value
  final case class CategoricalValue(value: String) extends Value
  final case class NumericalValue(value: Double) extends Value
}
