package treemodel

import org.scalatest._
import value._

import scala.io.{BufferedSource, Source}
import scala.xml.{Elem, Node}


class TreeModelSpec extends FlatSpec with Matchers {
  val source: BufferedSource = Source.fromResource("tree_model.pmml")
  val treeContent: String = source.mkString; source.close()
  val treePmml: Elem = scala.xml.XML.loadString(treeContent)
  val trimmedTreePmml: Node = scala.xml.Utility.trim(treePmml)
  val treeModel: TreeModel = TreeModel.fromXml(trimmedTreePmml)

  it should "score for no features" in {
    treeModel.decide(Map()) shouldEqual Some(4.3463944950723456E-4)
  }

  it should "f2 first true" in {
    treeModel.decide(Map("f2" -> CategoricalValue("f2v1"))) shouldEqual Some(-1.8361380219689046E-4)
  }

  it should "f1, f2 first true" in {
    val features = Map("f2" -> CategoricalValue("f2v1"), "f1" -> CategoricalValue("f1v3"))
    treeModel.decide(features) shouldEqual Some(-6.237581139073701E-4)
  }

  it should "f1, f2, f4  first true" in {
    val features = Map("f2" -> CategoricalValue("f2v1"), "f1" -> CategoricalValue("f1v3"), "f4" -> NumericalValue(0.08))
    treeModel.decide(features) shouldEqual Some(0.0021968294712358194)
  }

  it should "f1, f2 first true, f4 second true" in {
    val features = Map("f2" -> CategoricalValue("f2v1"), "f1" -> CategoricalValue("f1v3"), "f4" -> NumericalValue(0.09))
    treeModel.decide(features) shouldEqual Some(-9.198573460887271E-4)
  }

  it should "f2 none are true" in {
    val features = Map("f2" -> CategoricalValue("f2v7"))
    treeModel.decide(features) shouldEqual None
  }
}
