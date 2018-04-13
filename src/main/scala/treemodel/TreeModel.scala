package treemodel

import node._
import value._

object TreeModel {
  def fromXml(xmlPredicate: scala.xml.Node): TreeModel = {
    val id: String = (xmlPredicate \ "@id").text
    val root: Node = Node.fromXml(xmlPredicate)
    new TreeModel(id, root)
  }
}

class TreeModel(id: String, root: Node) {

  def decide(features: Map[String, Value]): Double = {
    val current: Node = root

    current.score
  }
}
