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
import com.enjapan.knp.models.Tag

import scala.util.{Failure, Success, Try}

/**
 * Range expression Parser
 */
object QuantityAnalyzer {
  /**
   * Setting of quantity representation
   * @param tag
   * @return
   */
  //TODO: Support for decimal points
  private def findQuantity(tag:Tag): String= {
    if(tag.features.get("NE").getOrElse("").startsWith("DATE") || tag.features.get("NE").getOrElse("").startsWith("TIME")){
      tag.features.get("NE").getOrElse("").split(":")(1)
    }else if(tag.morphemes.filter(_.bunrui == "数詞").size > 0) {
      val quantity = tag.morphemes.filter(_.bunrui == "数詞").head.genkei
      //negative number care
      if(tag.children.size == 1 && tag.children.head.morphemes.head.genkei.last.equals('－')) {
        "-" +  quantity
      }else{
        quantity
      }
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
    val rangeWords = List("以上", "以下", "以内", "未満", "まで", "から", "超過", "以後", "以前")
    if(tag.features.isDefinedAt("カウンタ") && !rangeWords.contains(tag.features.get("カウンタ").get)){
      return tag.features.get("カウンタ").getOrElse("")
    }else if (tag.parent.isDefined && tag.parent.filter(_.features.isDefinedAt("カウンタ")).size > 0){
      return tag.parent.get.features.get("カウンタ").get
    }else if (tag.children.size > 0 && tag.children.filter(_.morphemes.filter(_.genkei.equals("￥")).size > 0).size > 0){
      return "￥"
    }
    ""
  }

  /**
   * Setting of range representation
   * @param tag
   * @return
   */
  private def findRange(tag:Tag): String = {

    val greaterRep = List("多い", "高い", "大きい", "でかい", "重い", "重たい", "長い", "遠い", "速い", "強い", "後")
    val lessRep = List("少ない", "低い", "安い", "小さい", "軽い", "短い", "近い", "遅い", "前", "弱い")

    if(tag.morphemes.filter(( m :Morpheme ) =>List("以上", "以後").contains(m.genkei)).size > 0) {
      ">="
    }else if(tag.morphemes.filter(( m :Morpheme ) =>List("以下", "以内", "以前").contains(m.genkei)).size > 0){
      "<="
    }else if(tag.morphemes.filter(( m :Morpheme ) =>List("未満", "まで").contains(m.genkei)).size > 0){
      "<"
    }else if(tag.morphemes.filter(( m :Morpheme ) =>List("から", "超過").contains(m.genkei)).size > 0){
      ">"
    }else if(tag.parent.isDefined) {
      if (tag.parent.get.morphemes.filter(( m :Morpheme ) =>List("以上", "以後").contains(m.genkei)).size > 0){
        ">="
      }else if(tag.parent.get.morphemes.filter(( m :Morpheme ) =>List("以下", "以内", "以前").contains(m.genkei)).size > 0){
        "<="
      }else if(tag.parent.get.morphemes.filter(( m :Morpheme ) =>List("未満", "まで").contains(m.genkei)).size > 0){
        "<"
      }else if(tag.parent.get.morphemes.filter(( m :Morpheme ) =>List("から", "超過").contains(m.genkei)).size > 0){
        ">"
      }else if(tag.morphemes.filter(( m :Morpheme ) =>List("より").contains(m.genkei)).size > 0){
        if(tag.parent.get.morphemes.filter(( m :Morpheme ) => greaterRep.contains(m.genkei)).size > 0){
          ">"
        } else if(tag.parent.get.morphemes.filter(( m :Morpheme ) => lessRep.contains(m.genkei)).size > 0) {
          "<"
        } else{
          ""
        }
      }else{
        ""
      }
    }else{
      ""
    }
  }

  /**
   *
   * @param tag
   * @return (prefix, quantity, unit)
   */
  private def findPrefix(quantity:String, unit:String): (String,String,String) = {
    //Prefixは、存在する場合はquantityかunitどちらかに存在している想定
    val prefixQuantity = UnitPrefixConverter.splitPrefix(quantity)
    if(prefixQuantity._1.equals("")){
      val prefixUnit = UnitPrefixConverter.splitPrefix(unit)
      (prefixUnit._1, quantity, prefixUnit._2)
    }else{
      (prefixQuantity._1, prefixQuantity._2, unit)
    }
  }

  /**
   * Setting of range expressions
   * @param tag
   * @return
   */
  private def findRangeExpression(tag: Tag, namedEntity: String): (String, Map[String, String]) = Try{
    //カウンタがなく、数量だけもある
    if(!tag.features.isDefinedAt("数量") || tag.morphemes.filter(_.bunrui.equals("数詞")).size == 0) return ("", Map.empty[String, String])
    val quantityTarget = this.findQuantity(tag)
    val unitTarget = this.findUnit(tag)
    val prefixTarget = this.findPrefix(quantityTarget, unitTarget)

    val quantity = QuantityConverter.convertNumericStr(prefixTarget._2, namedEntity)
    val unit = UnitConverter.getUnitSymbol(prefixTarget._3)
    val prefix = UnitPrefixConverter.getPrefixSymbol(prefixTarget._1)

    val range = this.findRange(tag) + quantity
    (tag.features.get("正規化代表表記").getOrElse("-").split("/")(0), Map("prefix" -> prefix, "quantity" -> quantity, "unit" -> unit, "range" -> range ))
  }match {
    case Success(s) => s
    case Failure(e) => throw e
  }


  /**
   *
   * @param tags
   * @param namedEntity
   * @return
   */
  def getRangeExpression(tags:List[Tag], namedEntity:String):Map[String, Map[String, String]] = Try{
    tags.size match {
      case 1 => tags.map(findRangeExpression(_, namedEntity)).map(arr => arr._1 -> arr._2).toMap
      case _ => {
        val rangeExpressions = tags.map(findRangeExpression(_, namedEntity)).filterNot(_._1.equals("")).map(arr => arr._1 -> arr._2).toMap
        rangeExpressions.isEmpty match {
          case true => Map("" -> Map.empty[String,String])
          case _ =>  rangeExpressions
        }
      }
    }
  }match {
    case Success(s) => s
    case Failure(e) => throw e
  }

}
