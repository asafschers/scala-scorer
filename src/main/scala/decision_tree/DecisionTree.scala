package decision_tree

import node._

object DecisionTree {
  def fromXml(xmlPredicate: scala.xml.Node): DecisionTree = {
    val id: String = (xmlPredicate \ "@id").text
    val root: Node = Node.fromXml(xmlPredicate)
    new DecisionTree(id, root)
  }
}

class DecisionTree(id: String, root: Node) {

}
