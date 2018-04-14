package treemodel

import org.scalatest._
import value._

import scala.io.{BufferedSource, Source}
import scala.xml.{Elem, Node}


class TreeModelSpec extends FlatSpec with Matchers {
  val source: BufferedSource = Source.fromResource("tree_model.pmml")
  val treeContent: String = try source.mkString finally source.close()
  val treePmml: Elem = scala.xml.XML.loadString(treeContent)
  val trimmedTreePmml: Node = scala.xml.Utility.trim(treePmml)
  val treeModel: TreeModel = TreeModel.fromXml(trimmedTreePmml)

  it should "score for no features" in {
//    treeModel.decide(Map()) shouldEqual 4
  }
}
