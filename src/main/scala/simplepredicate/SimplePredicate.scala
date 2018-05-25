package simplepredicate
import simplepredicate.SimplePredicate._
import predicate.Predicate
import value._

object SimplePredicate {
  val GreaterThan     = "greaterThan"
  val LessThan        = "lessThan"
  val LessOrEqual     = "lessOrEqual"
  val GreaterOrEqual  = "greaterOrEqual"
  val Equals          = "equal"
  val IsMissing       = "isMissing"
  val MathOps = Array(GreaterThan, LessThan, LessOrEqual, GreaterOrEqual)

  def fromXml(xmlPredicate: scala.xml.Node): SimplePredicate = {
    val field: String = (xmlPredicate \ "@field").text
    val operator: String = (xmlPredicate \ "@operator").text
    val stringValue: String = (xmlPredicate \ "@value").text
    val predicateValue = util.Try { stringValue.toDouble } match {
      case util.Success(numericalValue) => NumericalValue(numericalValue)
      case util.Failure(_) => CategoricalValue(stringValue)
    }
    new SimplePredicate(field, operator, predicateValue)
  }
}

class SimplePredicate(field: String, operator: String, predicateValue: Value) extends Predicate {

  def isTrue(features: Map[String, Value]): Either[String, Boolean] = {
    val inputValue = features.get(field)
    predicateValue match {
      case NumericalValue(numericalValue) =>
        isTrueNum(inputValue, numericalValue)
      case CategoricalValue(categoricalValue) =>
        isTrueStr(inputValue, categoricalValue)
    }
  }

  private def isTrueNum(inputValue: Option[Value], predicateValue: Double) = {
    inputValue match {
      case Some(NumericalValue(numericalValue)) =>
        operator match {
          case GreaterThan =>
            Right(numericalValue > predicateValue)
          case LessThan =>
            Right(numericalValue < predicateValue)
          case GreaterOrEqual =>
            Right(numericalValue >= predicateValue)
          case LessOrEqual =>
            Right(numericalValue <= predicateValue)
          case Equals =>
            Right(numericalValue == predicateValue)
        }
      case Some(CategoricalValue(_)) =>
        Left("Expected Numerical Feature")
      case None =>
        Left("Missing Numerical Value")
    }
  }

  private def isTrueStr(inputValue: Option[Value], predicateValue: String) = {
    inputValue match {
      case Some(CategoricalValue(categoricalValue)) =>
        operator match {
          case IsMissing =>
            Right(categoricalValue == "")
          case Equals =>
            Right(categoricalValue == predicateValue)
        }
      case Some(NumericalValue(_)) =>
        Right(false)
      case None =>
        operator match {
          case IsMissing =>
            Right(inputValue.isEmpty)
        }
    }
  }
}
