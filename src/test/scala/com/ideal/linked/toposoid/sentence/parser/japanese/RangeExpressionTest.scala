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
import org.scalatest.{BeforeAndAfter, BeforeAndAfterAll, DiagrammedAssertions, FlatSpec}

class RangeExpressionTest extends FlatSpec with DiagrammedAssertions with BeforeAndAfter with BeforeAndAfterAll{

  "彼の体重は推定、60kgから八十キログラムの間です。"should "analyze correctly" in {
    val o = SentenceParser.parse("彼の体重は推定、60kgから八十キログラム未満です。")
    val rangeExceptionList =  o._1.map(x => x._2.rangeExpressions.head._2).filter(_.size != 0)
    rangeExceptionList.foreach(x => {
      x.get("quantity").get.toInt match  {
        case 60 => {
          assert(x.get("prefix").get.equals("KILO"))
          assert(x.get("unit").get.equals("GRAM"))
          assert(x.get("range").get.equals(">60"))
        }
        case 80 => {
          assert(x.get("prefix").get.equals("KILO"))
          assert(x.get("unit").get.equals("GRAM"))
          assert(x.get("range").get.equals("<80"))
        }
        case _ => assert(false)
      }
    })
  }

  "彼の体重は60kgより多く八十キログラムより少ないはずです。"should "analyze correctly" in {
    val o = SentenceParser.parse("彼の体重は60kgより多く八十キログラムより少ないはずです。")
    val rangeExceptionList =  o._1.map(x => x._2.rangeExpressions.head._2).filter(_.size != 0)
    rangeExceptionList.foreach(x => {
      x.get("quantity").get.toInt match  {
        case 60 => {
          assert(x.get("prefix").get.equals("KILO"))
          assert(x.get("unit").get.equals("GRAM"))
          assert(x.get("range").get.equals(">60"))
        }
        case 80 => {
          assert(x.get("prefix").get.equals("KILO"))
          assert(x.get("unit").get.equals("GRAM"))
          assert(x.get("range").get.equals("<80"))
        }
        case _ => assert(false)
      }
    })
  }


  "彼の身長は推定、170cmから180センチメートルの間です。"should "analyze correctly" in {
    val o = SentenceParser.parse("彼の身長は推定、170cmから百八十センチメートル未満です。")
    val rangeExceptionList =  o._1.map(x => x._2.rangeExpressions.head._2).filter(_.size != 0)
    rangeExceptionList.foreach(x => {
      x.get("quantity").get.toInt match  {
        case 170 => {
          assert(x.get("prefix").get.equals("CENTI"))
          assert(x.get("unit").get.equals("Metre"))
          assert(x.get("range").get.equals(">170"))
        }
        case 180 => {
          assert(x.get("prefix").get.equals("CENTI"))
          assert(x.get("unit").get.equals("Metre"))
          assert(x.get("range").get.equals("<180"))
        }
        case _ => assert(false)
      }
    })
  }


  "その期限は、平成十年三月三十一日から令和元年5月1日までです。"should "analyze correctly" in {
    val o = SentenceParser.parse("その期限は、平成十年三月三十一日から令和元年5月1日までです。")
    val hoge = o._1.map(x => x._2.rangeExpressions)

    val rangeExceptionList =  o._1.map(x => x._2.rangeExpressions.head._2).filter(_.size != 0)
    rangeExceptionList.foreach(x => {
      x.get("quantity").get match  {
        case "1998-3-31" => {
          assert(x.get("prefix").get.equals(""))
          assert(x.get("unit").get.equals("Day"))
          assert(x.get("range").get.equals(">1998-3-31"))
        }
        case "2019-5-1" => {
          assert(x.get("prefix").get.equals(""))
          assert(x.get("unit").get.equals("Day"))
          assert(x.get("range").get.equals("<2019-5-1"))
        }
        case _ => assert(true)
      }
    })
  }

  "そのイベントは、AM１０時３０分から午後八時五十九分までです。"should "analyze correctly" in {
    val o = SentenceParser.parse("そのイベントは、AM１０時３０分から午後八時五十九分までです。")
    val rangeExceptionList =  o._1.map(x => x._2.rangeExpressions.head._2).filter(_.size != 0)
    rangeExceptionList.foreach(x => {
      x.get("quantity").get match  {
        case "10:30:0" => {
          assert(x.get("prefix").get.equals(""))
          assert(x.get("unit").get.equals("Minute"))
          assert(x.get("range").get.equals(">10:30:0"))
        }
        case "20:59:0" => {
          assert(x.get("prefix").get.equals(""))
          assert(x.get("unit").get.equals("Minute"))
          assert(x.get("range").get.equals("<20:59:0"))
        }
        case _ => assert(true)
      }
    })
  }

  "その案件は、四億五千万円以上、10億円以下で取り引きされるだろう。"should "analyze correctly" in {
    val o = SentenceParser.parse("その案件は、四億五千万円以上、10億円以下で取り引きされるだろう。")
    val rangeExceptionList =  o._1.map(x => x._2.rangeExpressions.head._2).filter(_.size != 0)
    rangeExceptionList.foreach(x => {
      x.get("quantity").get match  {
        case "450000000" => {
          assert(x.get("prefix").get.equals(""))
          assert(x.get("unit").get.equals("YEN"))
          assert(x.get("range").get.equals(">=450000000"))
        }
        case "1000000000" => {
          assert(x.get("prefix").get.equals(""))
          assert(x.get("unit").get.equals("YEN"))
          assert(x.get("range").get.equals("<=1000000000"))
        }
        case _ => assert(false)
      }
    })
  }

  "その案件は、￥450,000,000以上、1000000000YEN未満で取り引きされるだろう。"should "analyze correctly" in {
    val o = SentenceParser.parse("その案件は、￥450,000,000以上、1000000000YEN未満で取り引きされるだろう。")
    val rangeExceptionList =  o._1.map(x => x._2.rangeExpressions.head._2).filter(_.size != 0)
    rangeExceptionList.foreach(x => {
      x.get("quantity").get match  {
        case "450000000" => {
          assert(x.get("prefix").get.equals(""))
          assert(x.get("unit").get.equals("YEN"))
          assert(x.get("range").get.equals(">=450000000"))
        }
        case "1000000000" => {
          assert(x.get("prefix").get.equals(""))
          assert(x.get("unit").get.equals("YEN"))
          assert(x.get("range").get.equals("<1000000000"))
        }
        case _ => assert(false)
      }
    })
  }

  "その指標は、-100以上、100以下で定義される。"should "analyze correctly" in {
    val o = SentenceParser.parse("その指標は、-100以上、100以下で定義される。")
    val rangeExceptionList =  o._1.map(x => x._2.rangeExpressions.head._2).filter(_.size != 0)
    rangeExceptionList.foreach(x => {
      x.get("quantity").get match  {
        case "-100" => {
          assert(x.get("prefix").get.equals(""))
          assert(x.get("unit").get.equals(""))
          assert(x.get("range").get.equals(">=-100"))
        }
        case "100" => {
          assert(x.get("prefix").get.equals(""))
          assert(x.get("unit").get.equals(""))
          assert(x.get("range").get.equals("<=100"))
        }
        case _ => assert(false)
      }
    })
  }


}