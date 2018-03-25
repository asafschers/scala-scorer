package simplepredicate
import org.scalatest._

class SimplePredicateSpec extends FlatSpec with Matchers {

  val lessOrEqualPredicate = SimplePredicate.setFromXml(<SimplePredicate field="f1" operator="lessOrEqual" value="2.0"/>)

  it should "return true for smaller value" in {
    lessOrEqualPredicate.isTrue(Map("f1" -> Left(1.9))) shouldEqual Right(true)
  }

  it should "return false for greater value" in {
    lessOrEqualPredicate.isTrue(Map("f1" -> Left(2.1))) shouldEqual Right(false)
  }

  it should "error for no double value" in {
    lessOrEqualPredicate.isTrue(Map("f1" -> Right("a"))) shouldEqual Left("Expected Numerical feature")
  }

  val isMissingPredicate = SimplePredicate.setFromXml(<SimplePredicate field="f1" operator="isMissing"/>)

  it should "return true for no key" in {
    isMissingPredicate.isTrue(Map()) shouldEqual Right(true)
  }

  it should "return true for no right value" in {
    isMissingPredicate.isTrue(Map("f1" -> Right(""))) shouldEqual Right(true)
  }

  it should "return false for existing num" in {
    isMissingPredicate.isTrue(Map("f1" -> Left(2.1))) shouldEqual Right(false)
  }

  it should "return false for existing string" in {
    isMissingPredicate.isTrue(Map("f1" -> Right("a"))) shouldEqual Right(false)
  }

  val equalsPredicate = SimplePredicate.setFromXml(<SimplePredicate field="f1" operator="equal" value="3"/>)

  it should "return true for other value" in {
    equalsPredicate.isTrue(Map("f1" -> Left(1.9))) shouldEqual Right(false)
  }

  it should "return false for equal value" in {
    equalsPredicate.isTrue(Map("f1" -> Left(3))) shouldEqual Right(true)
  }

  val equalsStrPredicate = SimplePredicate.setFromXml(<SimplePredicate field="f1" operator="equal" value="v3"/>)

  it should "return false for num value" in {
    equalsStrPredicate.isTrue(Map("f1" -> Left(3))) shouldEqual Right(false)
  }

  it should "return true for equal str value" in {
    equalsStrPredicate.isTrue(Map("f1" -> Right("v3"))) shouldEqual Right(true)
  }
}