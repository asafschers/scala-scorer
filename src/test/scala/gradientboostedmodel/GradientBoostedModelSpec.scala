package gradientboostedmodel

import org.scalatest.{FlatSpec, Matchers}
import value.{CategoricalValue, NumericalValue}

import scala.io.{BufferedSource, Source}
import scala.xml.{Elem, Node}

class GradientBoostedModelSpec extends FlatSpec with Matchers {
  val source: BufferedSource = Source.fromResource("titanic_gbm.pmml")
  val modelContent: String = source.mkString; source.close()
  val modelPmml: Elem = scala.xml.XML.loadString(modelContent)
  val trimmedModelPmml: Node = scala.xml.Utility.trim(modelPmml)
  val model: GradientBoostedModel = GradientBoostedModel.fromXml(trimmedModelPmml)

  it should "score one result" in {
    val features = Map("Sex" -> CategoricalValue("male"),
      "Parch" -> NumericalValue(0),
      "Age" -> NumericalValue(30),
      "Fare" -> NumericalValue(9.6875),
      "Pclass" -> NumericalValue(2),
      "SibSp" -> NumericalValue(0),
      "Embarked" -> CategoricalValue("Q"))
    model.score(features) shouldEqual Some(0.7990679530500985)
  }

  it should "score other result" in {
    val features = Map("Sex" -> CategoricalValue("female"),
      "Parch" -> NumericalValue(0),
      "Age" -> NumericalValue(38),
      "Fare" -> NumericalValue(71.2833),
      "Pclass" -> NumericalValue(2),
      "SibSp" -> NumericalValue(1),
      "Embarked" -> CategoricalValue("C"))
    model.score(features) shouldEqual Some(0.8009413525080109)
  }
}
