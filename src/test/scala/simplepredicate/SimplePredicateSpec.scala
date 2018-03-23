package simplepredicate
import org.scalatest._

class SimplePredicateSpec extends FlatSpec with Matchers {

  val lessOrEqualPredicate = new SimplePredicate(<SimplePredicate field="f1" operator="lessOrEqual" value="2.0"/>)

  it should "return true for smaller value" in {
    lessOrEqualPredicate.isTrue(Map("f1" -> Some(1.9))) shouldEqual Left(true)
  }

  it should "return false for greater value" in {
    lessOrEqualPredicate.isTrue(Map("f1" -> Some(2.1))) shouldEqual Left(false)
  }

  it should "error for no value" in {
    lessOrEqualPredicate.isTrue(Map("f1" -> None)) shouldEqual Right("Missing Feature")
  }

  // TODO: companion object builds the predicate - class should receive field, operator, value
//  val isMissingPredicate = new SimplePredicate(<SimplePredicate field="f1" operator="isMissing"/>)

//  it should "return true for no key" in {
//    isMissingPredicate.isTrue(Map()) shouldEqual true
//  }
//
//  it should "return true for n value" in {
//    isMissingPredicate.isTrue(Map("f1" -> None)) shouldEqual true
//  }
//
//  it should "return false for existing value" in {
//    isMissingPredicate.isTrue(Map("f1" -> Some(2.1))) shouldEqual false
//  }

//  val equalsPredicate = new SimplePredicate(<SimplePredicate field="1" operator="equal" value="3/>)

//  it should "return true for comparison predicate" in {
//    equalsPredicate.isTrue(Map("f1" -> Some(1.9))) shouldEqual true
//  }
//
//  it should "return false for comparison predicate" in {
//    equalsPredicate.isTrue(Map("f1" -> Some(2.1))) shouldEqual false
//  }
}