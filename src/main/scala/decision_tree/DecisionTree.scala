package decision_tree

import node._
import value._

object DecisionTree {
  def fromXml(xmlPredicate: scala.xml.Node): DecisionTree = {
    val id: String = (xmlPredicate \ "@id").text
    val root: Node = Node.fromXml(xmlPredicate)
    new DecisionTree(id, root)
  }
}

class DecisionTree(id: String, root: Node) {

  def decide(features: Map[String, Value]): Double = {
    val current: Node = root

    current.score
  }
}
