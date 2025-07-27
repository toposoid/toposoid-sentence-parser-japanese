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

import javax.measure.MetricPrefix
import scala.util.matching.Regex
import scala.util.{Failure, Success, Try}


/**
 * This module contains a set of functions that convert UnitPrefix expressions(only Japanese) into a uniform format.
 */

//ref. https://ja.wikipedia.org/wiki/%E3%83%A8%E3%82%BF
object UnitPrefixConverter {

  val pattern:Regex = """^(ｙ|ｚ|ａ|ｆ|ｐ|ｎ|μ|ｍ|ｃ|ｄ|ｄａ|ｈ|ｋ|Ｍ|Ｇ|Ｔ|Ｐ|Ｅ|Ｚ|Ｙ)(ｇ|ｇｒ|ｄｒ|ｏｚ|ｌｂ|ｓｔ|ｔ|ｍ|ｉｎ|ｆｔ|ｙｄ|ｍｉｌｅ|\u212B|ｍ\u00B2|ａ|ｈａ|\u33A5|Ｌ|l|ｇａｌ|ｑｔ|ｐｔ|ｂｂｌ|Ｇ|ｂｉｔ|Ｂ)$""".r
  val pattern2:Regex = """(.*)(ヨクト|ゼプト|アット|フェムト|ピコ|マイクロ|ミリ|センチ|デシ|デカ|ヘクト|キロ|メガ|ギガ|テラ|ペタ|エクサ|ゼタ|ヨタ)$""".r
  val pattern3:Regex = """^(ヨクト|ゼプト|アット|フェムト|ピコ|マイクロ|ミリ|センチ|デシ|デカ|ヘクト|キロ|メガ|ギガ|テラ|ペタ|エクサ|ゼタ|ヨタ)(.*)$""".r

  /**
   *
    * @param str
   * @return (prefix, other)
   */
  def splitPrefix(str:String): (String, String) = Try{
    if(str matches(pattern.toString())){
      val patternMatch = pattern.findFirstMatchIn(str).head
      (patternMatch.group(1), patternMatch.group(2))
    }else if(str matches(pattern2.toString())){
      val patternMatch = pattern2.findFirstMatchIn(str).head
      (patternMatch.group(2), patternMatch.group(1))
    }else if(str matches(pattern3.toString())){
      val patternMatch = pattern3.findFirstMatchIn(str).head
      (patternMatch.group(1), patternMatch.group(2))
    } else{
      ("", str)
    }
  }match {
    case Success(s) => s
    case Failure(e) => throw e
  }

  /**
   *
   * @param str
   * @return
   */
  def getPrefixSymbol(prefix:String): String = Try{

    if(prefix.endsWith("ヨクト") || prefix.startsWith("ｙ")){
      MetricPrefix.YOCTO.name()
    }else if(prefix.endsWith("ゼプト") || prefix.startsWith("ｚ")) {
      MetricPrefix.ZEPTO.name()
    }else if(prefix.endsWith("アット") || prefix.startsWith("ａ")){
      MetricPrefix.ATTO.name()
    }else if(prefix.endsWith("フェムト") || prefix.startsWith("ｆ")){
      MetricPrefix.FEMTO.name()
    }else if(prefix.endsWith("ピコ") || prefix.startsWith("ｐ")){
      MetricPrefix.PICO.name()
    }else if(prefix.endsWith("ナノ") || prefix.startsWith("ｎ")){
      MetricPrefix.NANO.name()
    }else if(prefix.endsWith("マイクロ") || prefix.startsWith("μ")){
      MetricPrefix.MICRO.name()
    }else if(prefix.endsWith("ミリ") || prefix.startsWith("ｍ")){
      MetricPrefix.MILLI.name()
    }else if(prefix.endsWith("センチ") || prefix.startsWith("ｃ")){
      MetricPrefix.CENTI.name()
    }else if(prefix.endsWith("デシ") || prefix.startsWith("ｄ")){
      MetricPrefix.DECI.name()
    }else if(prefix.endsWith("デカ") || prefix.startsWith("ｄａ")){
      MetricPrefix.DEKA.name()
    }else if(prefix.endsWith("ヘクト") || prefix.startsWith("ｈ")){
      MetricPrefix.HECTO.name()
    }else if(prefix.endsWith("キロ") || prefix.startsWith("ｋ")){
      MetricPrefix.KILO.name()
    }else if(prefix.endsWith("メガ") || prefix.startsWith("Ｍ")){
      MetricPrefix.MEGA.name()
    }else if(prefix.endsWith("ギガ") || prefix.startsWith("Ｇ")){
      MetricPrefix.GIGA.name()
    }else if(prefix.endsWith("テラ") || prefix.startsWith("Ｔ")){
      MetricPrefix.TERA.name()
    }else if(prefix.endsWith("ペタ") || prefix.startsWith("Ｐ")){
      MetricPrefix.PETA.name()
    }else if(prefix.endsWith("エクサ") || prefix.startsWith("Ｅ")){
      MetricPrefix.EXA.name()
    }else if(prefix.startsWith("ゼタ") || prefix.startsWith("Ｚ")){
      MetricPrefix.ZETTA.name()
    }else if(prefix.startsWith("ヨタ") || prefix.startsWith("Ｙ")){
      MetricPrefix.YOTTA.name()
    }else{
      prefix
    }
  }match {
    case Success(s) => s
    case Failure(e) => throw e
  }

}
