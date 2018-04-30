package gradientboostedmodel

import treemodel.TreeModel
import value.Value
import scala.math._

object GradientBoostedModel {
  def fromXml(xmlPredicate: scala.xml.Node): GradientBoostedModel = {
    val xmlTrees = xmlPredicate \\ "TreeModel"
    val const = (xmlPredicate \\ "Target" \ "@rescaleConstant").toString.toDouble
    val treeModels: List[TreeModel] = xmlTrees.map(TreeModel.fromXml).toList
    new GradientBoostedModel(treeModels, const)
  }
}

class GradientBoostedModel(treeModels: List[TreeModel], const: Double) {
  def score(features: Map[String, Value]): Option[Double] = {
    val treesSum: Double = treeModels.flatMap(_.decide(features)).sum + const
    Some(exp(treesSum) / (1 + exp(treesSum)))
  }
}
