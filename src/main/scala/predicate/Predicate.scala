package predicate

import value._

object Predicate {
  def apply(xml: scala.xml.Node): Predicate = {
    xml.label match {
      case "SimplePredicate" => SimplePredicate.fromXml(xml)
      case "SimpleSetPredicate" => SimpleSetPredicate.fromXml(xml)
      case "True" => new TruePredicate
    }
  }
}

abstract class Predicate {
  def isTrue(features: Map[String, Value]): Either[String, Boolean]
}
