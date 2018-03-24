package simplepredicate

class SimplePredicate(field: String, operator: String, value: Double) {
  val GREATER_THAN     = "greaterThan"
  val LESS_THAN        = "lessThan"
  val LESS_OR_EQUAL    = "lessOrEqual"
  val GREATER_OR_EQUAL = "greaterOrEqual"
  val EQUALS           = "equal"
  val IS_MISSING       = "isMissing"
  val MATH_OPS = Array(GREATER_THAN, LESS_THAN, LESS_OR_EQUAL, GREATER_OR_EQUAL)

  def isTrue(features: Map[String, Option[Double]]): Either[Boolean, String] = {
    features.get(field).flatten match {
      case Some(featureValue) =>
        operator match {
          case GREATER_THAN =>
            Left(featureValue > value)
          case LESS_THAN =>
            Left(featureValue < value)
          case GREATER_OR_EQUAL =>
            Left(featureValue >= value)
          case LESS_OR_EQUAL =>
            Left(featureValue <= value)
          case EQUALS =>
             Left(featureValue == value)
          case _ =>
            Right("Unknown Operator")
        }
      case None =>
        operator match {
          case IS_MISSING =>
            Left(features.get(field).flatten.isEmpty)
          case _ =>
            Right("Missing Feature")
        }
    }
  }
}


object SimplePredicate {
  def setFromXml(xml_predicate: scala.xml.Elem): SimplePredicate = {
    val field: String = (xml_predicate \ "@field").text
    val operator: String = (xml_predicate \ "@operator").text
    val value: Double = (xml_predicate \ "@value").text.toFloat
    new SimplePredicate(field, operator, value)
  }
}

