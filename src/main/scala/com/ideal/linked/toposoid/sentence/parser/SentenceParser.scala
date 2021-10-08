/*
 * Copyright 2021 Linked Ideal LLC.[https://linked-ideal.com/]
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.ideal.linked.toposoid.sentence.parser

import com.enjapan.juman.models.Morpheme
import com.enjapan.knp.KNPCli
import com.enjapan.knp.models.{BList, Bunsetsu, Tag}
import com.ideal.linked.toposoid.common.{CLAIM, PREMISE}

import scala.util.{Failure, Success, Try}
import com.ideal.linked.toposoid.knowledgebase.model.KnowledgeBaseNode
import com.ideal.linked.toposoid.knowledgebase.model.KnowledgeBaseEdge
import com.typesafe.scalalogging.LazyLogging
import io.jvm.uuid.UUID

/**
 * This module takes a sentence as input and returns the result of predicate argument structure analysis.
 */
object SentenceParser extends LazyLogging {

  var index:Int = 1
  var nodes: Map[String, KnowledgeBaseNode] = Map.empty[String,KnowledgeBaseNode]
  var edges: List[KnowledgeBaseEdge]= List.empty[KnowledgeBaseEdge]
  var bunsetsuNum = 0
  val knp = new KNPCli()

  /**
   * Main function of this module　
   * @param sentence
   * @return (nodes:Map[String:KnowledgeBaseNode], edges:List[KnowledgeBaseEdge]
   *         The key for nodes is the nodeId of KnowledgeBaseNode.
   *         For KnowledgeBaseNode and KnowledgeBaseEdge, see the com.ideal.linked.toposoid.knowledgebase.model package.
   */
  def parse(sentence:String):(Map[String, KnowledgeBaseNode], List[KnowledgeBaseEdge]) =Try {

    clear()
    val propositionId:String = UUID.random.toString
    val blist = knp(sentence)
    val knpResult = blist.getOrElse("").asInstanceOf[BList]
    bunsetsuNum = knpResult.bunsetsuList.size
    knpResult.root.traverse(analyze(_, propositionId))
    classify()
    (nodes, edges)
  }match {
    case Success(s) => s
    case Failure(e) => throw e
  }

  /**
   * Initialization of variables that store results
   */
  private def clear():Unit = Try{
    nodes = Map.empty[String,KnowledgeBaseNode]
    edges = List.empty[KnowledgeBaseEdge]
    index = 1
    bunsetsuNum =0
  }match {
    case Success(s) => s
    case Failure(e) => throw e
  }

  /**
   * Determination of node type (premise/claim)
   */
  private def classify(): Unit ={
    //ノードに一つもisConditionalConnection=Trueがなければ、全てClaimNode。一旦、全てClaimノードにしてしまう。
    for(node <- nodes){
      replaceKnowledgeBaseNode(node._2, CLAIM.index)
    }
    val conditionalConnectionNodes = nodes.filter(_._2.isConditionalConnection)
    //複数の節が前提となる場合も考慮する。
    if(conditionalConnectionNodes.size > 0){
      //ノードにisConditionalConnection=Trueがあれば、そこから有効グラフが逆向きに向かって末端まではPremiseNode
      for(conditionalConnectionNode <- conditionalConnectionNodes.values){
        val selectedInitialEdges:List[KnowledgeBaseEdge] = edges.filter(_.destinationId == conditionalConnectionNode.nodeId)
        //PremiseNode群の起点となるノードを置換
        replaceKnowledgeBaseNode(conditionalConnectionNode, PREMISE.index)
        selectedInitialEdges.map(replacePremiseNode(_))
      }
    }
  }

  /**
   * Determination of premiseEdge.
   * This function is a recursive function
   * @param premiseEdge
   */
  private def replacePremiseNode(premiseEdge:KnowledgeBaseEdge): Unit ={
    replaceKnowledgeBaseNode(nodes.get(premiseEdge.sourceId).head, PREMISE.index)
    val selectedPremiseEdges:List[KnowledgeBaseEdge] = edges.filter(_.destinationId == premiseEdge.sourceId)
    if(selectedPremiseEdges.size > 0){
      selectedPremiseEdges.map(replacePremiseNode(_))
    }
  }

  /**
   * Setting of node information
   * @param node
   * @param nodeType
   */
  private def replaceKnowledgeBaseNode(node:KnowledgeBaseNode, nodeType:Int): Unit ={
    val replaceNode =  KnowledgeBaseNode(
      node.nodeId,
      node.propositionId,
      node.currentId,
      node.parentId,
      node.isMainSection,
      node.surface,
      node.normalizedName,
      node.dependType,
      node.caseType,
      node.namedEntity,
      node.rangeExpressions,
      node.categories,
      node.domains,
      node.isDenial,
      node.isConditionalConnection,
      node.normalizedNameYomi,
      node.surfaceYomi,
      node.modalityType,
      node.logicType,
      nodeType)
    nodes = nodes.updated(node.nodeId,replaceNode)
  }


  /**
   * Setting of category information and domain information
   * @param tag
   * @param attr
   * @return
   */
  private def getCategoryOrDomain(tag:Tag, attr:String):(String, String) = Try {

    if(tag.morphemes.filter(x => x.imis.contains(attr)).size == 0) return ("", "")
    val target = tag.morphemes.filter(x => x.imis.contains(attr)).head
    val name = target.genkei
    val content = target.imis.split(' ').toList.filter(_.indexOf(attr) != -1).head.split(":")(1).replace("\"", "")
    (name, content)

  } match {
    case Success(s) => s
    case Failure(e) => throw e
  }

  /**
   * Setting of quantity representation
   * @param tag
   * @return
   */
  private def findQuantity(tag:Tag): String= {
    if(tag.features.get("NE").getOrElse("").startsWith("DATE") || tag.features.get("NE").getOrElse("").startsWith("TIME")){
      tag.features.get("NE").getOrElse("").split(":")(1)
    }else if(tag.morphemes.filter(_.bunrui == "数詞").size > 0) {
      tag.morphemes.filter(_.bunrui == "数詞").head.genkei
    }else if(tag.features.isDefinedAt("数量")){
      tag.features.get("数量").getOrElse("")
    }else{
      ""
    }
  }

  /**
   * Setting of unit representation
   * @param tag
   * @return
   */
  private def findUnit(tag:Tag): String = {
    if(tag.features.isDefinedAt("カウンタ")){
      return tag.features.get("カウンタ").getOrElse("")
    }
    ""
  }

  /**
   * Setting of range representation
   * @param tag
   * @return
   */
  private def findRange(tag:Tag): String = {
    if(tag.morphemes.filter(( m :Morpheme ) =>List("以上", "以下", "未満", "超過", "以内").contains(m.genkei)).size > 0){
      tag.morphemes.filter(( m :Morpheme ) =>List("以上", "以下", "未満", "超過", "以内").contains(m.genkei)).head.genkei
    }else if(tag.morphemes.filter(( m :Morpheme ) =>List("から", "より", "まで").contains(m.genkei) && m.bunrui.equals("格助詞")).size > 0){
      tag.morphemes.filter(( m :Morpheme ) =>List("から", "より", "まで").contains(m.genkei)).head.genkei
    }else{
      ""
    }
  }

  /**
   * Setting of range expressions
   * @param tag
   * @return
   */
  private def getRangeExpressions(tag: Tag): (String, Map[String, String]) = Try{
    //カウンタがなく、数量だけもある
    if(!tag.features.isDefinedAt("数量")) return ("", Map.empty[String, String])
    val quantity = this.findQuantity(tag)
    val unit = this.findUnit(tag)
    val range = this.findRange(tag)
    (tag.features.get("正規化代表表記").getOrElse("-").split("/")(0), Map("quantity" -> quantity, "unit" -> unit, "range" -> range))

  }match {
    case Success(s) => s
    case Failure(e) => throw e
  }

  /**
   * Setting of modality expression
   * @param features
   * @return
   */
  private def getModality(features:Map[String,String]): String = Try{
    if(features.filter(_._1.indexOf("モダリティ") != -1).size > 0){
      features.filter(_._1.indexOf("モダリティ") != -1).head._1
    }else{
      "-"
    }
  }match {
    case Success(s) => s
    case Failure(e) => throw e
  }

  /**
   * Setting of phonetic characters
   * @param x
   * @param surfaceYomi
   * @return
   */
  private def checkConditionalClause(x: Bunsetsu, surfaceYomi:String):Boolean = Try {
    //特定の表層があった場合、詳細を解析して区切るかどうか決める。
    //https://www.anlp.jp/proceedings/annual_meeting/2016/pdf_dir/P20-3.pdf
    //読点などが最後になるケースもあるので、表層で見るときは部分一致で判定。

    if (x.tags.filter(_.features.isDefinedAt("節機能-条件")).size > 0) {
      //条件節を判定
      return true
    } else if ((surfaceYomi.contains("ばあい") || surfaceYomi.contains("ところ") || surfaceYomi.contains("かぎり") || surfaceYomi.contains("けっか") || surfaceYomi.contains("ものの"))
      && x.tags.filter(y => y.features.isDefinedAt("連用節") || y.features.isDefinedAt("連体節")).size > 0) {
      //条件節を判定
      return true
    } else if (surfaceYomi.contains("ばあい") && x.features.isDefinedAt("格要素") && x.features.isDefinedAt("外の関係")){
      //条件節を判定
      return true
    }else if(x.tags.filter(_.features.isDefinedAt("節機能-理由")).size > 0 || x.tags.filter(_.features.isDefinedAt("節機能-目的")).size > 0) {
      //理由節、目的節
      return true
    }else if(x.tags.filter(_.features.isDefinedAt("節機能-時間経過")).size > 0) {
      //時節を判定
      return true
    }else if(x.tags.filter(_.features.isDefinedAt("時間")).size > 0
      && x.tags.filter(y =>  y.features.isDefinedAt("連用節") || y.features.isDefinedAt("連体節")).size > 0){
      //時節を判定
      return true
    }else{
      false
    }

  }match {
    case Success(s) => s
    case Failure(e) => throw e
  }

  /**
   * Predicate argument structure analysis.
   * @param x
   * @param propositionId
   */
  private def analyze(x: Bunsetsu, propositionId :String): Unit = Try{

    val currentId = bunsetsuNum - index
    index += 1
    val nodeId = propositionId + "-" + currentId.toString
    val surface = x.tags.foldLeft(""){(acc, x) => acc + x.surface}
    val surfaceYomi = x.tags.foldLeft(""){(acc, x) => acc + x.morphemes.foldLeft(""){(acc2, y) => acc2 + y.yomi }}
    val normalizedName = x.features.get("正規化代表表記").getOrElse("-").split("/")(0)
    val isMainSection = x.features.isDefinedAt("主節")
    val caseType = x.features.get("係").getOrElse("-")
    val namedEntity = x.tags.foldLeft(""){(acc, x)=> acc + x.features.get("NE").getOrElse("")}
    val rangeExpressions = x.tags.map(getRangeExpressions).map(arr => arr._1 -> arr._2).toMap
    val categories = x.tags.map(getCategoryOrDomain(_, "カテゴリ")).map(arr => arr._1 -> arr._2).toMap
    val domains = x.tags.map(getCategoryOrDomain(_, "ドメイン")).map(arr => arr._1 -> arr._2).toMap
    val isDenial:Boolean = x.features.isDefinedAt("否定表現")
    val isConditionalConnection:Boolean = checkConditionalClause(x, surfaceYomi)
    val normalizedNameInfo =  x.features.get("正規化代表表記").getOrElse("-")
    val normalizedNameYomi = normalizedNameInfo match {
      case "-" => ""
      case _ => normalizedNameInfo.split('+').map(_.split('/')(1)).toList.mkString("")
    }
    val modalityType:String = getModality(x.features)
    val logicType:String = isConditionalConnection match {
      case true => "IMP"
      case false => x.features.get("並列タイプ").getOrElse("-")
    }

    //nodeTypeは全てのノードが確定するまで決められないので、一旦-1をセットしておく
    val node = KnowledgeBaseNode(nodeId, propositionId, currentId, x.parentId, isMainSection, surface, normalizedName, x.dpndtype, caseType, namedEntity, rangeExpressions, categories, domains, isDenial, isConditionalConnection, normalizedNameYomi, surfaceYomi, modalityType, logicType, -1)
    val sourceId = nodeId
    val destinationId = propositionId + "-" + x.parentId.toString
    val edge = KnowledgeBaseEdge(sourceId, destinationId, caseType, x.dpndtype, logicType)
    nodes  = nodes.updated(nodeId, node)
    //述語項構造解析の結果として文章が区切れる場合（文末から文末への関係がある場合）は、エッジを作成しない。
    if(x.parentId != -1 && caseType != "文末") edges :+=  edge

  }match {
    case Success(s) => s
    case Failure(e) => throw e
  }

  for(node <- nodes) {
    println(node)
  }
  for(edge <- edges) {
    println(edge)
  }

}
