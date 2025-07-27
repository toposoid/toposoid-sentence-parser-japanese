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
import scala.collection.mutable.HashMap
import scala.util.parsing.combinator.RegexParsers

/**
 * Kanji Numeral to Ordinary Number Converter(only Japanese)
 */

//ref. http://www.ewavesolutions.com/blog/?p=719
object KansujiConverter extends RegexParsers {

    // 変換規則
    val ji = List[(Char,Long,Char)](('〇',0L,'A'),('一',1L,'A'),('二',2L,'A')
      ,('三',3L,'A'),('四',4L,'A'),('五',5L,'A'),('六',6L,'A'),('七',7L,'A')
      ,('八',8L,'A'),('九',9L,'A'),('零',0L,'A'),('壱',1L,'A'),('弐',2L,'A')
      ,('参',3L,'A'),('肆',4L,'A'),('伍',5L,'A'),('陸',6L,'A'),('質',7L,'A')
      ,('捌',8L,'A'),('玖',9L,'A'),('零',0L,'A'),('壹',1L,'A'),('貳',2L,'A'),('參',3L,'A')
      ,('０',0L,'A'),('１',1L,'A'),('２',2L,'A'),('３',3L,'A'),('４',4L,'A')
      ,('５',5L,'A'),('６',6L,'A'),('７',7L,'A'),('８',8L,'A'),('９',9L,'A')
      ,('0',0L,'A'),('1',1L,'A'),('2',2L,'A'),('3',3L,'A'),('4',4L,'A')
      ,('5',5L,'A'),('6',6L,'A'),('7',7L,'A'),('8',8L,'A'),('9',9L,'A')
      // 桁
      ,('十',10L,'T'),('百',100L,'T'),('千',1000L,'T'),('拾',10L,'T'),('佰',100L,'T')
      ,('仟',1000L,'T'),('十',10L,'T'),('陌',100L,'T'),('阡',1000L,'T')
      // 桁
      ,('万',10000L,'M'),('億',100000000L,'M'),('兆',1000000000000L,'M')
      ,('萬',10000L,'M'),('京',10000000000000000L,'M')
      ,(',',0L,'U'),('，',0L,'U'))

    val dic = new HashMap[Char,(Char,Long,Char)]	// 漢数字の辞書
    ji.foreach(v => dic.put(v._1,v))
    val keta4 = Array("", "十", "百", "千")
    val keta = Array("", "万", "億", "兆", "京")
    val ketaOld4 = Array("", "拾", "佰", "仟")	// (旧漢字)
    val ketaOld = Array("", "萬", "億", "兆", "京") // (旧漢字)

    // 漢数字を整数に変換
    def toArabicNumerals (num:String) : String = {
      var result = 0L		// 変換結果
      var resultArabic = 0L	// アラビア数字表記の漢数字の変換結果
      var format = 0		// 0は未確定状態、アラビア数字表記の漢数字の場合は1、2は漢数字、3は万・億・兆・京と〇から九までの漢数字
      var backSuji = -1L	// 1つ前の数字
      var back4 = 0L		// 千・万・十の時のバッファ
      var backKeta = 0L		// 万・億・兆・京の1つ前の桁
      var backKeta4 = 0L	// 千・百・十の１つ前の桁
      var four = 0L		// 〇から九までの漢数字が四桁以下の連続の場合のバッファ
      var current = -1L
      for(i <- 0 to num.length - 1){
        val c = num.charAt(i)
        //	    println(" #" + i + " [" + c + "] format=" + format + " backSuji="+backSuji+ " back4="+back4+ " four="+ four + " result=" + result )
        val r = dic.get(c) match {
          case Some(f) => f
          case _ => ('0',0L,'X')	// 漢数字を構成する文字列がない
        }
        current = r._2
        r._3 match {
          case 'A' => {
            resultArabic = current + (resultArabic * 10L)
            four = current + (four * 10L)
            if(format != 1 && four > 9999){
              //return -8L	// ４桁以上は構文エラー
              return num
            }
            if(i > 0){
              if(format == 0 && backKeta == 0L){
                format = 1	// フォーマット確定
              }
            }
            backSuji =current // 一つ前として記録
          }
          case 'T' => {	// 千・百・十　漢数字の4桁バッファの処理
            format = 2
            if(backKeta4 != 0L && backKeta4 <= current){
              //return -5L	// 桁が１つ前の桁よりも大きいか同じなら構文エラー
              return num
            }
            if(backSuji > 0L){
              back4 += backSuji * current // 千・百・十の前に数字がある
            } else {
              back4 += current // 千・百・十が単独
            }
            backKeta4 = current
            backSuji = -1L	// 初期化
            four = 0L		// 初期化 〇から九までの漢数字が四桁以下の連続の場合のバッファ
          }
          case 'M' => {	// 万・億・兆・京
            if(backSuji == -1L && back4 == 0L){
              //return -7L	// 1つ前が数字でないか、千・百・十でないときは構文エラー
              return num
            }
            if(backKeta != 0L && backKeta <= current){
              //return -4L	// 現在の桁が１つ前の桁よりも大きいか同じなら構文エラー
              return num
            }
            if(format == 2){
              if(backSuji > 0L){
                back4 += backSuji
              }
              result += (back4 * current)
            } else if(four > 0L){
              if(four > 9999L){	// ４桁以上なら構文エラー
                //return -8L
                return num
              }
              result += (four * current)
              format = 3	// 書式が確定
            }
            backKeta = current	// 一つ前の文字として保存
            backKeta4 = 0L	// 初期化
            backSuji = -1L	// 初期化
            back4 = 0L		// 初期化
            four = 0L		// 初期化 〇から九までの漢数字が四桁以下の連続の場合のバッファ
          }
          case 'U' =>
          case _ => return num//return -9L	// 漢数字ではない
        }
      }
      if(format == 2){// 千・万・十の４桁後処理
        if(backSuji > 0L)
          back4 += backSuji
        result += back4
      } else {
        result += four
      }
      if(format == 1)	// アラビア数字表記の漢数字の変換結果
        return resultArabic.toString
      result.toString
    }
}
