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
