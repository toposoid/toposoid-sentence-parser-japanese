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

import scala.util.{Failure, Success, Try}
import tech.units.indriya.unit.Units
import com.ideal.linked.common.measurement.ExUnits


/**
 * This module contains a set of functions that convert unit expressions(only Japanese) into a uniform format.
 */
object UnitConverter {

  //TODO: The pound is both a unit of money and a unit of weight.
  //TODO: Fix of electrical units
  //TODO: Notational variation care

  /**
   *
   * @param unit
   * @return
   */
  def getUnitSymbol(unit:String): String  = Try{
    if(unit.equals("グラム") || unit.equals("ｇ")) {
      ExUnits.GRAM.getName
    }else if (unit.equals("グレーン") || unit.equals("ｇｒ")) {
      ExUnits.GRAIN.getName
    }else if (unit.equals("ドラム") || unit.equals("ｄｒ")) {
      ExUnits.DRUM.getName
    }else if (unit.equals("オンス") || unit.equals("ｏｚ")) {
      ExUnits.OUNCE.getName
    }else if (unit.equals("ポンド") || unit.equals("パウンド") || unit.equals("ｌｂ")) {
      ExUnits.POUND.getName
    }else if (unit.equals("ストーン") || unit.equals("ｓｔ")) {
      ExUnits.STONE.getName
    }else if (unit.equals("トン") || unit.equals("ｔ")) {
      ExUnits.TONNE.getName
    }else if (unit.equals("メートル") || unit.equals("ｍ")) {
      Units.METRE.getName
    }else if (unit.equals("インチ") || unit.equals("ｉｎ")) {
      ExUnits.INCH.getName
    }else if (unit.equals("フィート") || unit.equals("ｆｔ")) {
      ExUnits.FEET.getName
    }else if (unit.equals("ヤード") || unit.equals("ｙｄ")) {
      ExUnits.YARD.getName
    }else if (unit.equals("マイル") || unit.equals("ｍｉｌｅ")) {
      ExUnits.MILE.getName
    }else if (unit.equals("オングストローム") || unit.equals("\u212B")) {
      ExUnits.ANGSTROEM.getName
    }else if (unit.equals("平方メートル") || unit.equals("平米") || unit.equals("ｍ\u00B2")) {
      ExUnits.SQUARE_METRE.getName
    }else if (unit.equals("アール") || unit.equals("ａ")) {
      ExUnits.ARE.getName
    }else if (unit.equals("ヘクタール") || unit.equals("ｈａ")) {
      ExUnits.HECTARE.getName
    }else if (unit.equals("エーカー") || unit.equals("ａｃ")) {
      ExUnits.ACRE.getName
    }else if (unit.equals("坪")) {
      ExUnits.TSUBO.getName
    }else if (unit.equals("立方メートル") || unit.equals("立米") || unit.equals("\u33A5")) {
      ExUnits.CUBIC_METRE.getName
    }else if (unit.equals("リットル") || unit.equals("ｌ") || unit.equals("Ｌ") || unit.equals("l")) {
      Units.LITRE.getName
    }else if (unit.equals("ガロン") || unit.equals("ｇａｌ")) {
      ExUnits.GALLON.getName
    }else if (unit.equals("クウォート") || unit.equals("ｑｔ")) {
      ExUnits.QUART.getName
    }else if (unit.equals("パイント") || unit.equals("ｐｔ")) {
      ExUnits.PINT.getName
    }else if (unit.equals("バレル") || unit.equals("ｂｂｌ")) {
      ExUnits.BARREL.getName
    }else if (unit.equals("ガウス")|| unit.equals("Ｇ")) {
      ExUnits.GAUSS.getName
    }else if (unit.equals("ビット")|| unit.equals("ｂｉｔ")) {
      ExUnits.BIT.getName
    }else if (unit.equals("バイト")|| unit.equals("Ｂ")) {
      ExUnits.BYTE.getName
    }else if (unit.equals("年") || unit.equals("年間")) {
      Units.YEAR.getName
    }else if (unit.equals("週") || unit.equals("週間")) {
      Units.WEEK.getName
    }else if (unit.equals("月") || unit.equals("月間") || unit.equals("ヶ月") || unit.equals("カ月") || unit.equals("ヶ月間") || unit.equals("カ月間")) {
      Units.MONTH.getName
    }else if (unit.equals("日") || unit.equals("日間")) {
      Units.DAY.getName
    }else if (unit.equals("時") || unit.equals("時間")) {
      Units.HOUR.getName
    }else if (unit.equals("分") || unit.equals("分間")) {
      Units.MINUTE.getName
    }else if (unit.equals("秒") || unit.equals("秒間")) {
      Units.SECOND.getName
    }else if (unit.equals("円") || unit.equals("ＹＥＮ") || unit.equals("￥")) {
      ExUnits.YEN.getName
    }else if (unit.equals("ドル")) {
      ExUnits.DOLLER.getName
    }else if (unit.equals("ユーロ")) {
      ExUnits.EURO.getName
    }else if (unit.equals("パーセント") || unit.equals("％"))
      Units.PERCENT.getName
    else{
      unit
    }
  }match {
    case Success(s) => s
    case Failure(e) => throw e
  }
}
