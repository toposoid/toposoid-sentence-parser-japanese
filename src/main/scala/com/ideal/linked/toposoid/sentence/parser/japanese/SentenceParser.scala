/*
 * Copyright (C) 2025  Linked Ideal LLC.[https://linked-ideal.com/]
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package com.ideal.linked.toposoid.sentence.parser.japanese

import com.enjapan.juman.models.Morpheme
import com.enjapan.knp.KNPCli
import com.enjapan.knp.models.{BList, Bunsetsu, Tag}
import com.ibm.icu.text.Transliterator
import com.ideal.linked.toposoid.common.{CLAIM, PREMISE}
import com.ideal.linked.toposoid.knowledgebase.model.{KnowledgeBaseEdge, KnowledgeBaseNode, KnowledgeFeatureReference, LocalContext, PredicateArgumentStructure}
import com.ideal.linked.toposoid.protocol.model.parser.KnowledgeForParser
import com.typesafe.scalalogging.LazyLogging

import scala.util.{Failure, Success, Try}


case class SentenceParserResult(nodes:Map[String, KnowledgeBaseNode], edges:List[KnowledgeBaseEdge], index:Int, bunsetsuNum:Int)

/**
 * This module takes a japanese sentence as input and returns the result of predicate argument structure analysis.
 */
object SentenceParser extends LazyLogging {

  val knp = new KNPCli()
  /**
   * Main function of this module　
   * @param knowledgeForParser
   * @return (nodes:Map[String:KnowledgeBaseNode], edges:List[KnowledgeBaseEdge]
   *         The key for nodes is the nodeId of KnowledgeBaseNode.
   *         For KnowledgeBaseNode and KnowledgeBaseEdge, see the com.ideal.linked.toposoid.knowledgebase.model package.
   */
  def parse(knowledgeForParser:KnowledgeForParser):(Map[String, KnowledgeBaseNode], List[KnowledgeBaseEdge]) =Try {

    val propositionId:String = knowledgeForParser.propositionId
    val sentenceId:String = knowledgeForParser.sentenceId
    val lang:String = knowledgeForParser.knowledge.lang
    val transliterator = Transliterator.getInstance("Halfwidth-Fullwidth")
    val blist = knp(transliterator.transliterate(knowledgeForParser.knowledge.sentence))
    val knpResult = blist.getOrElse("").asInstanceOf[BList]

    val initSentenceParserResult = SentenceParserResult(Map.empty[String, KnowledgeBaseNode], List.empty[KnowledgeBaseEdge], 1, bunsetsuNum = knpResult.bunsetsuList.size)
    val sentenceParserResult:SentenceParserResult = knpResult.bunsetsuList.reverse.foldLeft(initSentenceParserResult) {
      (acc, x) => analyze(x, propositionId, sentenceId, lang, acc)
    }
    (classify(sentenceParserResult), sentenceParserResult.edges)
  }match {
    case Success(s) => s
    case Failure(e) => throw e
  }

  /**
   * Determination of node type (premise/claim)
   */
  private def classify(spr:SentenceParserResult): Map[String, KnowledgeBaseNode] ={
    //ノードに一つもisConditionalConnection=Trueがなければ、全てClaimNode。一旦、全てClaimノードにしてしまう。
    val tmpNodes:Map[String, KnowledgeBaseNode] = spr.nodes.foldLeft(spr.nodes){
      (acc, x) => {
        replaceKnowledgeBaseNode(x._2, CLAIM.index, acc)
      }
    }
    val updateSpr = SentenceParserResult(tmpNodes, spr.edges, spr.index, spr.bunsetsuNum)
    val conditionalConnectionNodes = tmpNodes.filter(_._2.predicateArgumentStructure.isConditionalConnection)
    //複数の節が前提となる場合も考慮する。
    conditionalConnectionNodes.size > 0 match {
      case true => {
        conditionalConnectionNodes.values.foldLeft(tmpNodes) {
          (acc, x) => {
            val selectedInitialEdges: List[KnowledgeBaseEdge] = updateSpr.edges.filter(_.destinationId == x.nodeId)
            val tmpNodes2:Map[String, KnowledgeBaseNode] = replaceKnowledgeBaseNode(x, PREMISE.index, acc)
            val tmpNode3 = selectedInitialEdges.foldLeft(tmpNodes2){
              (acc2, y) =>{
                val updateSpr2 = SentenceParserResult(acc2, spr.edges, spr.index, spr.bunsetsuNum)
                replacePremiseNode(y, updateSpr2)
              }
            }
            acc ++ tmpNode3
          }
        }
      }
      case _ => tmpNodes
    }
    //classifiedNodes
  }

  /**
   * Determination of premiseEdge.
   * This function is a recursive function
   * @param premiseEdge
   */
  private def replacePremiseNode(premiseEdge:KnowledgeBaseEdge, spr:SentenceParserResult): Map[String, KnowledgeBaseNode] ={
    val replaceNodes = replaceKnowledgeBaseNode(spr.nodes.get(premiseEdge.sourceId).head, PREMISE.index, spr.nodes)
    val selectedPremiseEdges:List[KnowledgeBaseEdge] = spr.edges.filter(_.destinationId == premiseEdge.sourceId)

    selectedPremiseEdges.size > 0 match {
      case true => {
        val updatedSentenceParserResult = SentenceParserResult(replaceNodes, spr.edges, spr.index, spr.bunsetsuNum)
        selectedPremiseEdges.foldLeft(replaceNodes){ (acc, x) => {
          val premiseNodes :Map[String, KnowledgeBaseNode] = replacePremiseNode(x, updatedSentenceParserResult).filter(_._2.predicateArgumentStructure.nodeType == 0)
          premiseNodes.foldLeft(acc){(acc2, y) => {
            acc2 ++ Map(y._1 -> y._2)
          }}
        }}
      }
      case _ => replaceNodes
    }

  }

  /**
   * Setting of node information
   * @param node
   * @param nodeType
   */
  private def replaceKnowledgeBaseNode(node:KnowledgeBaseNode, nodeType:Int, nodes:Map[String, KnowledgeBaseNode]): Map[String, KnowledgeBaseNode] ={

    val localContext = LocalContext(
      "ja_JP",
      node.localContext.namedEntity,
      node.localContext.rangeExpressions,
      node.localContext.categories,
      node.localContext.domains,
      node.localContext.knowledgeFeatureReferences
    )

    val predicateArgumentStructure = PredicateArgumentStructure(
      node.predicateArgumentStructure.currentId,
      node.predicateArgumentStructure.parentId,
      node.predicateArgumentStructure.isMainSection,
      node.predicateArgumentStructure.surface,
      node.predicateArgumentStructure.normalizedName,
      node.predicateArgumentStructure.dependType,
      node.predicateArgumentStructure.caseType,
      node.predicateArgumentStructure.isDenialWord,
      node.predicateArgumentStructure.isConditionalConnection,
      node.predicateArgumentStructure.normalizedNameYomi,
      node.predicateArgumentStructure.surfaceYomi,
      node.predicateArgumentStructure.modalityType,
      node.predicateArgumentStructure.parallelType,
      nodeType,
      node.predicateArgumentStructure.morphemes
    )

    val replaceNode =  KnowledgeBaseNode(
      node.nodeId,
      node.propositionId,
      node.sentenceId,
      predicateArgumentStructure,
      localContext
      )
    nodes.updated(node.nodeId,replaceNode)
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
   * Care for cases where numbers should be included in the normalized expression
   * @param morphemes
   * @param normalizeName
   * @return
   */
  private def getNormalizeName(morphemes: List[Morpheme], normalizeName:String): String ={
    if(morphemes.filter(_.bunrui == "数詞").size == 0) {
      normalizeName
    }else{
      morphemes.filter(x => x.hinsi=="名詞").foldLeft(""){ (acc, x) => acc +  x.genkei}
    }
  }

  private def getMorphemes(morphemes: List[Morpheme]): List[String] = {
    morphemes.foldLeft(List.empty[String]) { (acc, x) => {
          acc ++ List(x.hinsi + "," + x.bunrui + "," + x.katuyou1 + "," + x.katuyou2)
      }
    }
  }


  /**
   * Predicate argument structure analysis.
   * @param x
   * @param propositionId
   */
  private def analyze(x: Bunsetsu, propositionId :String, sentenceId:String, lang:String, spr:SentenceParserResult): SentenceParserResult = Try{

    val currentId = spr.bunsetsuNum - spr.index
    //index += 1
    val nodeId = sentenceId + "-" + currentId.toString
    val surface = x.tags.foldLeft(""){(acc, x) => acc + x.surface}
    val surfaceYomi = x.tags.foldLeft(""){(acc, x) => acc + x.morphemes.foldLeft(""){(acc2, y) => acc2 + y.yomi }}
    val normalizedName = this.getNormalizeName(x.tags.map(_.morphemes).head, x.features.get("正規化代表表記").getOrElse("-").split("/")(0))
    val isMainSection = x.features.isDefinedAt("主節")
    val caseType = x.features.get("係").getOrElse("-")
    val namedEntity = x.tags.foldLeft(""){(acc, x)=> acc + x.features.get("NE").getOrElse("").split(":").head}
    val rangeExpressions = QuantityAnalyzer.getRangeExpression(x.tags, namedEntity)
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
    /*
    val logicType:String = isConditionalConnection match {
      case true => "IMP"
      case false => "-"
    }
    */
    val parallelType = x.features.get("並列タイプ").getOrElse("-")
    val morphemes = getMorphemes(x.tags.map(_.morphemes).head)
    //nodeTypeは全てのノードが確定するまで決められないので、一旦-1をセットしておく
    val predicateArgumentStructure = PredicateArgumentStructure(currentId, x.parentId, isMainSection, surface, normalizedName, x.dpndtype, caseType,isDenial, isConditionalConnection, normalizedNameYomi, surfaceYomi, modalityType, parallelType, -1, morphemes)
    val localContext = LocalContext(lang, namedEntity, rangeExpressions, categories, domains, List.empty[KnowledgeFeatureReference])

    val node = KnowledgeBaseNode(nodeId, propositionId,  sentenceId, predicateArgumentStructure, localContext)
    val sourceId = nodeId
    val destinationId = sentenceId + "-" + x.parentId.toString
    val edge = KnowledgeBaseEdge(sourceId, destinationId, caseType, x.dpndtype, parallelType, isConditionalConnection, "-")
    val nodes  = spr.nodes.updated(nodeId, node)
    //述語項構造解析の結果として文章が区切れる場合（文末から文末への関係がある場合）は、エッジを作成しない。
    val edges:List[KnowledgeBaseEdge] = x.parentId != -1 && caseType != "文末" match  {
      case true => spr.edges :+ edge
      case _ => spr.edges
    }
    //if(x.parentId != -1 && caseType != "文末") edges :+=  edge
    SentenceParserResult(nodes, edges, spr.index + 1, spr.bunsetsuNum)
  }match {
    case Success(s) => s
    case Failure(e) => throw e
  }


}
