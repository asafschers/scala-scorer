package simplesetpredicate
import predicate.Predicate
import value._

import scala.util.matching._

object SimpleSetPredicate {
  def fromXml(xml_predicate: scala.xml.Node): SimpleSetPredicate = {
    val field: String = (xml_predicate \ "@field").text
    val operator: String = (xml_predicate \ "@operator").text
    val values: Array[String] = formatValuesArray((xml_predicate \ "Array").text)
    new SimpleSetPredicate(field, operator, values)
  }

  def formatValuesArray(valuesInput: String): Array[String] = {
    ("\"(.*?)\"|([^\\s]+)").r.findAllIn(valuesInput).toArray.map(value => value.replace("\"",""))
  }
}

class SimpleSetPredicate(field: String, operator: String, values: Array[String]) extends Predicate {
  val IS_IN = "is_in"

  def isTrue(features: Map[String, Value]): Either[String, Boolean] = {
    val inputValue = features.get(field)
    inputValue match {
      case Some(NumericalValue(numericalValue)) =>
        Right(false)
      case Some(CategoricalValue(categoricalValue)) =>
        Right(values.contains(categoricalValue))
      case None =>
        Left("Missing categorical feature")
    }
  }
}
