package simplepredicate

object SimplePredicate {
  def setFromXml(xml_predicate: scala.xml.Elem): SimplePredicate = {
    val field: String = (xml_predicate \ "@field").text
    val operator: String = (xml_predicate \ "@operator").text
    val stringValue: String = (xml_predicate \ "@value").text
    val value = try { Left(stringValue.toDouble) } catch { case _ => Right(stringValue) }
    new SimplePredicate(field, operator, value)
  }
}

class SimplePredicate(field: String, operator: String, value: Either[Double,String]) {
  val GREATER_THAN     = "greaterThan"
  val LESS_THAN        = "lessThan"
  val LESS_OR_EQUAL    = "lessOrEqual"
  val GREATER_OR_EQUAL = "greaterOrEqual"
  val EQUALS           = "equal"
  val IS_MISSING       = "isMissing"
  val MATH_OPS = Array(GREATER_THAN, LESS_THAN, LESS_OR_EQUAL, GREATER_OR_EQUAL)

  def isTrue(features: Map[String, Either[Double, String]]): Either[Boolean, String] = {
    val featureValue = features.get(field)
    value match {
      case Left(numValue) =>
        isTrueNum(featureValue, numValue)
      case Right(strValue) =>
        isTrueStr(featureValue, strValue)
    }
  }

  private def isTrueStr(featureValue: Option[Either[Double, String]], strValue: String) = {
    featureValue match {
      case Some(Left(_)) =>
        Left(false)
      case Some(Right(featureStrValue)) =>
        operator match {
          case IS_MISSING =>
            Left(featureStrValue == "")
          case EQUALS =>
            Left(featureStrValue == strValue)
        }
      case None =>
        operator match {
          case IS_MISSING =>
            Left(featureValue.isEmpty)
        }
    }
  }

  private def isTrueNum(featureValue: Option[Either[Double, String]], numValue: Double) = {
    featureValue match {
      case Some(Left(featureNumValue)) =>
        operator match {
          case GREATER_THAN =>
            Left(featureNumValue > numValue)
          case LESS_THAN =>
            Left(featureNumValue < numValue)
          case GREATER_OR_EQUAL =>
            Left(featureNumValue >= numValue)
          case LESS_OR_EQUAL =>
            Left(featureNumValue <= numValue)
          case EQUALS =>
            Left(featureNumValue == numValue)
        }
      case Some(Right(_)) =>
        Right("Expected Numerical feature")
    }
  }
}
