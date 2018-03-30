package simplesetpredicate

object SimpleSetPredicate {
  def setFromXml(xml_predicate: scala.xml.Elem): SimpleSetPredicate = {
    val field: String = (xml_predicate \ "@field").text
    val operator: String = (xml_predicate \ "@operator").text
    val values: Array[String] = formatValuesArray((xml_predicate \ "Array").text)
    new SimpleSetPredicate(field, operator, values)
  }

  def formatValuesArray(valuesInput: String): Array[String] = {
    valuesInput.split(" ")
  }
}

sealed trait Value 
final case class CategoricalValue(b: String) extends Value
final case class NumericalValue(a: Double) extends Value

class SimpleSetPredicate(field: String, operator: String, values: Array[String]) {
  val IS_IN = "is_in"

  def isTrue(features: Map[String, Value]): Either[String, Boolean] = {
    val inputValue = features.get(field)
    inputValue match {
      case Some(NumericalValue(numericalValue)) =>
        Right(false)
      case Some(CategoricalValue(categoricalValue)) =>
        Right(values.contains(categoricalValue))
    }
  }
}
