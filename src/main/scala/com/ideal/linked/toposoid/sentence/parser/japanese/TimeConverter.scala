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

/**
 * This module contains a set of functions that convert time expressions(only Japanese) into a uniform format.
 */
object TimeConverter {

  val timeHour = """(午前|午後|AM|PM|am|pm|)([0-9]+)[:-．時]""".r
  val timeHourMinute = """(午前|午後|AM|PM|am|pm|)([0-9]+|[一二三四五六七八九十壱弐参拾百千万萬億兆〇]+)[-:．時]([0-9]+|[一二三四五六七八九十壱弐参拾百千万萬億兆〇]+)[-:．分]{1}""".r
  val timeHourMinuteSecond = """(午前|午後|AM|PM|am|pm|)([0-9]+|[一二三四五六七八九十壱弐参拾百千万萬億兆〇]+)[-:．時]{1}([0-9]+|[一二三四五六七八九十壱弐参拾百千万萬億兆〇]+)[-:．分]{1}([0-9]+|[一二三四五六七八九十壱弐参拾百千万萬億兆〇]+)[秒]{0,1}""".r
  val timeMinute = """([0-9]+|[一二三四五六七八九十壱弐参拾百千万萬億兆〇]+)[-:．分]{1}""".r
  val timeMinuteSecond = """([0-9]+|[一二三四五六七八九十壱弐参拾百千万萬億兆〇]+)[-:．分]{1}([0-9]+|[一二三四五六七八九十壱弐参拾百千万萬億兆〇]+)[秒]{0,1}""".r
  val timeSecond = """([0-9]+|[一二三四五六七八九十壱弐参拾百千万萬億兆〇]+)秒""".r
  val termHour = """^([0-9]+|[一二三四五六七八九十壱弐参拾百千万萬億兆〇]+)時間$""".r
  val termMinute = """^([0-9]+|[一二三四五六七八九十壱弐参拾百千万萬億兆〇]+)分$""".r
  val termSecond = """^([0-9]+|[一二三四五六七八九十壱弐参拾百千万萬億兆〇]+)秒$""".r

  /**
   *
   * @param timeStr
   * @return
   */
  def convert(timeStr:String): String ={
    val timeTuple = timeStr match {
      case timeHour(k, h) => (k, h, "", "")
      case timeMinute(m) => ("", "", m, "")
      case timeSecond(s) => ("", "", "", s)
      case timeHourMinute(k, h, m) => (k, h, m, "")
      case timeMinuteSecond(m, s) => ("", "", m, s)
      case timeHourMinuteSecond(k, h, m, s) => (k, h, m, s)
      case _ => ("", "", "", "")
    }

    val ampm = timeTuple._1
    val hour = KansujiConverter.toArabicNumerals(timeTuple._2)
    val min = KansujiConverter.toArabicNumerals(timeTuple._3)
    val sec = KansujiConverter.toArabicNumerals(timeTuple._4)

    val convertHour = hour match {
      case "24" => "0"
      case x => List("午後", "PM", "pm").contains(ampm) && x.toInt < 12 match {
        case true =>  (hour.toInt + 12).toString //２４時制で返却する
        case _ => hour
      }
    }
    "%02d:%02d:%02d".format(convertHour.toInt, min.toInt, sec.toInt)
  }
}


