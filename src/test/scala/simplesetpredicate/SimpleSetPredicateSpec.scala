package simplesetpredicate
import org.scalatest._

class SimpleSetPredicateSpec extends FlatSpec with Matchers {

  val noQuotesPredicate: SimpleSetPredicate = SimpleSetPredicate.setFromXml(
    <SimpleSetPredicate field="f1" booleanOperator="isIn">
      <Array n="3" type="string">f2v1 f2v2 f2v3</Array>
    </SimpleSetPredicate>)

  it should "return true for value in array" in {
    noQuotesPredicate.isTrue(Map("f1" -> CategoricalValue("f2v2"))) shouldEqual Right(true)
  }

  it should "return false for value not in array" in {
    noQuotesPredicate.isTrue(Map("f1" -> NumericalValue(1.9))) shouldEqual Right(false)
    noQuotesPredicate.isTrue(Map("f1" -> CategoricalValue("f2v4"))) shouldEqual Right(false)
  }

}
