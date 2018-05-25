package simplesetpredicate
import simplesetpredicate.SimpleSetPredicate._
import predicate.Predicate
import value._

import scala.util.matching._

object SimpleSetPredicate {
  val IsIN = "is_in"

  def fromXml(xmlPredicate: scala.xml.Node): SimpleSetPredicate = {
    val field: String = (xmlPredicate \ "@field").text
    val operator: String = (xmlPredicate \ "@operator").text
    val values: List[String] = formatValues((xmlPredicate \ "Array").text)
    new SimpleSetPredicate(field, operator, values)
  }

  def formatValues(valuesInput: String): List[String] = {
    ("\"(.*?)\"|([^\\s]+)").r.findAllIn(valuesInput).toList.map(value => value.replace("\"",""))
  }
}

class SimpleSetPredicate(field: String, operator: String, values: List[String]) extends Predicate {

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
