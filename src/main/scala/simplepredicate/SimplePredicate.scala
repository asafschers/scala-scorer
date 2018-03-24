package simplepredicate


class SimplePredicate(field: String, operator: String, value: Either[Double,String]) {
  val GREATER_THAN     = "greaterThan"
  val LESS_THAN        = "lessThan"
  val LESS_OR_EQUAL    = "lessOrEqual"
  val GREATER_OR_EQUAL = "greaterOrEqual"
  val EQUALS           = "equal"
  val IS_MISSING       = "isMissing"
  val MATH_OPS = Array(GREATER_THAN, LESS_THAN, LESS_OR_EQUAL, GREATER_OR_EQUAL)

  def isTrue(features: Map[String, Option[Double]]): Either[Boolean, String] = {
    value match {
      case Left(numValue) =>
        features.get(field).flatten match {
          case Some(featureValue) =>
            operator match {
              case GREATER_THAN =>
                Left(featureValue > numValue)
              case LESS_THAN =>
                Left(featureValue < numValue)
              case GREATER_OR_EQUAL =>
                Left(featureValue >= numValue)
              case LESS_OR_EQUAL =>
                Left(featureValue <= numValue)
              case EQUALS =>
                Left(featureValue == numValue)
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
//      case Right(numValue) =>
    }
  }
}

object SimplePredicate {
  def setFromXml(xml_predicate: scala.xml.Elem): SimplePredicate = {
    val field: String = (xml_predicate \ "@field").text
    val operator: String = (xml_predicate \ "@operator").text
    val stringValue: String = (xml_predicate \ "@value").text
    val value = try { Left(stringValue.toDouble) } catch { case _ => Right(stringValue) }
    new SimplePredicate(field, operator, value)
  }
}

