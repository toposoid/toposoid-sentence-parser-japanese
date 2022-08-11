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

package com.ideal.linked.toposoid.sentence.parser.japanese

import org.apache.commons.lang.math.NumberUtils

import java.time.chrono.{JapaneseDate, JapaneseEra}
import java.time.format.DateTimeFormatter

/**
 * This module contains a set of functions that convert date expressions(only Japanese) into a uniform format.
 */
object DateConverter {

  val dateYear = """(令和|平成|昭和|大正|明治|)(元|[0-9]+|[一二三四五六七八九十壱弐参拾百千万萬億兆〇]+)[-\/\.／－ー．年]{1}""".r
  val dateYearMonth = """(令和|平成|昭和|大正|明治|)(元|[0-9]+|[一二三四五六七八九十壱弐参拾百千万萬億兆〇]+)[-\/\.／－ー．年]{1}(\d{1,2}|[一二三四五六七八九十壱弐参拾百千万萬億兆〇]+)[-\/\.／－ー．月]{1}""".r
  val dateYearMonthDay = """(令和|平成|昭和|大正|明治|)(元|[0-9]+|[一二三四五六七八九十壱弐参拾百千万萬億兆〇]+)[-\/\.／－ー．年]{1}(\d{1,2}|[一二三四五六七八九十壱弐参拾百千万萬億兆〇]+)[-\/\.／－ー．月]{1}(\d{1,2}|[一二三四五六七八九十壱弐参拾百千万萬億兆〇]+)[．日]{0,1}""".r
  val dateMonth = """(\d{1,2}|[一二三四五六七八九十壱弐参拾百千万萬億兆〇]+)[-\/\.／－ー．月]{1}""".r
  val dateMonthDay = """(\d{1,2}|[一二三四五六七八九十壱弐参拾百千万萬億兆〇]+)[-\/\.／－ー．月]{1}(\d{1,2}|[一二三四五六七八九十壱弐参拾百千万萬億兆〇]+)[．日]{0,1}""".r
  val dateDay = """(\d{1,2}|[一二三四五六七八九十壱弐参拾百千万萬億兆〇]+)日""".r
  val termYear = """^([0-9]+|[一二三四五六七八九十壱弐参拾百千万萬億兆〇]+)年$""".r
  val termMonth = """^([0-9]+|[一二三四五六七八九十壱弐参拾百千万萬億兆〇]+)[かヶヵ]{1}月$""".r
  val termDay = """^([0-9]+|[一二三四五六七八九十壱弐参拾百千万萬億兆〇]+)日$""".r

  /**
   * 西暦に変換して文字列として返す
   * @param dateStr
   * @return
   */
  def convert(dateStr:String): String = {

    val dateTuple = dateStr match {
      case dateYear(g, y) => (g, y,  "", "")
      case dateMonth(m) => ("", "", m, "")
      case dateDay(d) => ("", "", "", d)
      case dateYearMonth(g, y, m) => (g, y, m, "")
      case dateMonthDay(m, d) => ("", "", m, d)
      case dateYearMonthDay(g, y, m, d) => (g, y, m, d)
      case _ => ("", "", "", "")
    }
    val year = KansujiConverter.toArabicNumerals(dateTuple._2) match {
      case "0" => ""
      case x => x
    }
    val month = KansujiConverter.toArabicNumerals(dateTuple._3) match {
      case "0" => ""
      case x => x
    }
    val day = KansujiConverter.toArabicNumerals(dateTuple._4) match{
      case "0" => ""
      case x => x
    }
    this.convertYMD(dateTuple._1, year, month, day)
  }

  /**
   *
   * @param gengou
   * @param yearStr
   * @param monthStr
   * @param dayStr
   * @return
   */
  private def convertYMD(gengou:String, yearStr:String, monthStr:String, dayStr:String): String = {

    //年がないパターン
    if(!NumberUtils.isNumber(yearStr) && !yearStr.equals("元") ){
      if(NumberUtils.isNumber(monthStr)){
          if(NumberUtils.isNumber(dayStr)){
            //MM-DD
            return "%02d-%02d".format(monthStr.toInt, dayStr.toInt)
          }else{
            //MM
            return "%02d".format(monthStr.toInt)
          }
      }else{
        if(NumberUtils.isNumber(dayStr)){
          //DD
          return "%02d".format(dayStr.toInt)
        }else{
          return ""
        }
      }
    }

    //年があるパターン
    val era = gengou match {
      case "令和" => JapaneseEra.valueOf("Reiwa")
      case "平成" => JapaneseEra.HEISEI
      case "昭和" => JapaneseEra.SHOWA
      case "大正" => JapaneseEra.TAISHO
      case "明治" => JapaneseEra.MEIJI
      case _ => return "%s%s-%s-%s".format(gengou, yearStr, monthStr, dayStr)
    }

    val year = yearStr match {
      case "元" => "1"
      case _  => yearStr
    }
    val month = monthStr match {
      case "" =>  "1"
      case _ => monthStr
    }
    val day = dayStr match {
      case "" =>  "1"
      case _ => dayStr
    }

    val convertYear = JapaneseDate.of(era, year.toInt, month.toInt, day.toInt).format(DateTimeFormatter.ISO_DATE).split("-")(0)
    if(NumberUtils.isNumber(monthStr)){
      if(NumberUtils.isNumber(dayStr)){
        //YYYY-MM-DD
        "%04d-%02d-%02d".format(convertYear.toInt, monthStr.toInt, dayStr.toInt)
      }else{
        //YYYY-MM-
        "%04d-%02d".format(convertYear.toInt, monthStr.toInt)
      }
    }else{
      //YYYY
      "%04d".format(convertYear.toInt)
    }
  }
}
