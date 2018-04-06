package node
import predicate._
import value._

object Node {
  def fromXml(xml_predicate: scala.xml.Node): Node = {
    val score: Double = (xml_predicate \ "@score").text.toDouble
    val predicate: Predicate = Predicate.apply(xml_predicate)
    val children: Seq[Node] = xml_predicate.child.map(xml => fromXml(xml))
    new Node(predicate, score, children)
  }
}

class Node(predicate: Predicate, score: Double, children: Seq[Node]) {
  def isTrue(features: Map[String, Value]): Either[String, Boolean] = {
    predicate.isTrue(features)
  }
}
