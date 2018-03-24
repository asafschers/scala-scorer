package simplepredicate
import org.scalatest._

class SimplePredicateSpec extends FlatSpec with Matchers {

  val lessOrEqualPredicate = SimplePredicate.setFromXml(<SimplePredicate field="f1" operator="lessOrEqual" value="2.0"/>)

  it should "return true for smaller value" in {
    lessOrEqualPredicate.isTrue(Map("f1" -> Left(1.9))) shouldEqual Left(true)
  }

  it should "return false for greater value" in {
    lessOrEqualPredicate.isTrue(Map("f1" -> Left(2.1))) shouldEqual Left(false)
  }

  it should "error for no value" in {
    lessOrEqualPredicate.isTrue(Map("f1" -> Right(""))) shouldEqual Right("Missing Feature")
  }

//  val isMissingPredicate = SimplePredicate.setFromXml(<SimplePredicate field="f1" operator="isMissing"/>)

//  it should "return true for no key" in {
//    isMissingPredicate.isTrue(Map()) shouldEqual true
//  }

//  it should "return true for n value" in {
//    isMissingPredicate.isTrue(Map("f1" -> None)) shouldEqual true
//  }
//
//  it should "return false for existing value" in {
//    isMissingPredicate.isTrue(Map("f1" -> Some(2.1))) shouldEqual false
//  }
//
  val equalsPredicate = SimplePredicate.setFromXml(<SimplePredicate field="f1" operator="equal" value="3"/>)

  it should "return true for other value" in {
    equalsPredicate.isTrue(Map("f1" -> Left(1.9))) shouldEqual Left(false)
  }

  it should "return false for equal value" in {
    equalsPredicate.isTrue(Map("f1" -> Left(3))) shouldEqual Left(true)
  }

  // TODO: spec equals on string value
}