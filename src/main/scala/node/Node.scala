package node
import predicate._
import value._

object Node {
  def fromXml(xmlPredicate: scala.xml.Node): Node = {
    val score: Double = (xmlPredicate \ "@score").text.toDouble
    val predicate: Predicate = Predicate.apply(xmlPredicate)
    val children: Seq[Node] = xmlPredicate.child.map(xml => fromXml(xml))
    new Node(predicate, score, children)
  }
}

class Node(predicate: Predicate, score: Double, children: Seq[Node]) {
  def isTrue(features: Map[String, Value]): Either[String, Boolean] = {
    predicate.isTrue(features)
  }
}
