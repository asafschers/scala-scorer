package node
import predicate._
import value._

object Node {
  def fromXml(xmlPredicate: scala.xml.Node): Node = {
    val score: Option[Double] = util.Try { Some((xmlPredicate \ "@score").text.toDouble) } match {
      case util.Success(numericalScore) => numericalScore
      case util.Failure(_) => None
    }
    val predicate: Predicate = Predicate(xmlPredicate.child.head)
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
