package node
import predicate._
import value._

object Node {
  def setFromXml(xml_predicate: scala.xml.Elem): Node = {
    val score: Double = (xml_predicate \ "@score").text.toDouble
    val predicate: Predicate = apply(xml_predicate)
    new Node(predicate, score, xml_predicate.child)
  }
}

class Node(predicate: Predicate, score: Double, children: Seq[scala.xml.Node]) {
  def isTrue(features: Map[String, Value]) = {
    true
  }
}
