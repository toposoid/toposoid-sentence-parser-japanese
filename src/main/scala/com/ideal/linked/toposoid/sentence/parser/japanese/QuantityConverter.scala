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

import com.ibm.icu.text.Transliterator
import org.apache.commons.lang.math.NumberUtils

import scala.util.{Failure, Success, Try}

/**
 * Number representation Converter(only Japanese)
 */
object QuantityConverter {

  /**
   *
   * @param quantity
   * @param namedEntity
   * @return
   */
  def convertNumericStr(quantity:String, namedEntity:String):String = Try {
    //全角数字を半角数字に変換
    val transliterator = Transliterator.getInstance("Fullwidth-Halfwidth")
    val quantityHalfChar = transliterator.transliterate(quantity)
    if(namedEntity.equals("DATE")){
      DateConverter.convert(quantityHalfChar)
    }else if(namedEntity.equals("TIME")){
      TimeConverter.convert(quantityHalfChar)
    }else{
      if(NumberUtils.isNumber(quantityHalfChar)) return quantityHalfChar
      val quantityKansuji = KansujiConverter.toArabicNumerals(quantityHalfChar)
      quantityKansuji
    }
  }match {
    case Success(s) => s
    //エラーの場合は、元の表現をそのまま返却
    case Failure(e) => quantity
  }

}
