package treemodel

import node._
import value._

object TreeModel {
  def fromXml(xmlPredicate: scala.xml.Node): TreeModel = {
    val id: String = (xmlPredicate \ "@id").text
    val node = (xmlPredicate \ "Node").head
    val root: Node = Node.fromXml(node)
    new TreeModel(id, root)
  }
}

class TreeModel(id: String, root: Node) {

  def decide(features: Map[String, Value]): Option[Double] = {
    var current: Node = root // TODO: change to val
    while (current.children.nonEmpty) {
      val next: Option[Node] = step(current, features)
      current = next match {
        case None => return None
        case Some(node) => node
      }
    }
    current.score
  }

  def step(current: Node, features: Map[String, Value]): Option[Node] = {
    current.children.find(_.isTrue(features) == Right(true))
  }
}
