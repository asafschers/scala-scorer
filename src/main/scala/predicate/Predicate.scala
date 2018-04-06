package predicate

import simplepredicate._
import simplesetpredicate._
import value._

object Predicate {
  def apply(xml: scala.xml.Node): Predicate = {
    xml.text match {
      case "SimplePredicate" => SimplePredicate.fromXml(xml)
      case "SimpleSetPredicate" => SimpleSetPredicate.fromXml(xml)
    }
  }
}

abstract class Predicate {
  def isTrue(features: Map[String, Value]): Either[String, Boolean]
}
