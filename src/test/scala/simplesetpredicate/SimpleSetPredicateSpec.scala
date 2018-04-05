package simplesetpredicate
import value._
import org.scalatest._

class SimpleSetPredicateSpec extends FlatSpec with Matchers {

  val noQuotesPredicate: SimpleSetPredicate = SimpleSetPredicate.fromXml(
    <SimpleSetPredicate field="f1" booleanOperator="isIn">
      <Array n="3" type="string">f2v1 f2v2 f2v3</Array>
    </SimpleSetPredicate>
  )

  it should "return true when value in array" in {
    noQuotesPredicate.isTrue(Map("f1" -> CategoricalValue("f2v2"))) shouldEqual Right(true)
  }

  it should "return false when value not in array" in {
    noQuotesPredicate.isTrue(Map("f1" -> NumericalValue(1.9))) shouldEqual Right(false)
  }

  it should "return false when numerical" in {
    noQuotesPredicate.isTrue(Map("f1" -> NumericalValue(1.9))) shouldEqual Right(false)
  }

  val QuotesPredicate: SimpleSetPredicate = SimpleSetPredicate.fromXml(
    <SimpleSetPredicate field="f1" booleanOperator="isIn">
      <Array n="6" type="string">
        &quot;Missing&quot; &quot;No Match&quot;
      </Array>
    </SimpleSetPredicate>
  )

  it should "return true when value in array with quotes" in {
    QuotesPredicate.isTrue(Map("f1" -> CategoricalValue("No Match"))) shouldEqual Right(true)
  }

  it should "return false when value not in array with quotes" in {
    QuotesPredicate.isTrue(Map("f1" -> CategoricalValue("Match"))) shouldEqual Right(false)
  }
}
