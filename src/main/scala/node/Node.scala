package node
import predicate._
import value._

object Node {
  def fromXml(xmlPredicate: scala.xml.Node): Node = {
    val score: Option[Double] = try { Some((xmlPredicate \ "@score").text.toDouble) } catch { case _ => None }
    val predicate: Predicate = Predicate.apply(xmlPredicate.child(0))
    val children: Seq[Node] = xmlPredicate.child.drop(1).map(xml => fromXml(xml))
    new Node(predicate, score, children)
  }
}

class Node(predicate: Predicate, inputScore: Option[Double], inputChildren: Seq[Node]) {
  val score = inputScore
  val children = inputChildren

  def isTrue(features: Map[String, Value]): Either[String, Boolean] = {
    predicate.isTrue(features)
  }
}
