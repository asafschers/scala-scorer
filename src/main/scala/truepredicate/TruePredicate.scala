package truepredicate

import predicate.Predicate
import value.Value

class TruePredicate extends Predicate {
  def isTrue(features: Map[String, Value]): Either[String, Boolean] = {
    Right(true)
  }
}
