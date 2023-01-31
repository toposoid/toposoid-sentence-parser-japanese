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

import com.ideal.linked.toposoid.knowledgebase.model.KnowledgeBaseNode
import org.scalatest.{BeforeAndAfter, BeforeAndAfterAll, DiagrammedAssertions, FlatSpec}
import com.ideal.linked.toposoid.common.{CLAIM, PREMISE}
import com.ideal.linked.toposoid.knowledgebase.regist.model.Knowledge
import com.ideal.linked.toposoid.protocol.model.parser.KnowledgeForParser
import io.jvm.uuid.UUID

class SentenceParserTest extends FlatSpec with DiagrammedAssertions with BeforeAndAfter with BeforeAndAfterAll{

  "太郎は花子に借りた本を返した。" should "analyze correctly" in {
    //主張の中の基本的な格構造（ 主語、目的語、補語）を認識できているか？
    val knowledgeForParser = KnowledgeForParser(UUID.random.toString, UUID.random.toString, Knowledge("太郎は花子に借りた本を返した。", "ja_JP", "{}") )
    val o = SentenceParser.parse(knowledgeForParser)
    val sentence:String = o._1.map(x => x._2.currentId -> x._2).toSeq.sortBy(_._1).foldLeft("") { (acc, x) => acc + x._2.surface }
    assert(sentence.equals("太郎は花子に借りた本を返した。"))
    val caseCheckMap = o._2.map(x =>  x.caseStr -> o._1.get(x.sourceId)).toMap[String, Option[KnowledgeBaseNode]]
    val subjectivePart:String = caseCheckMap.get("未格") match {
      case Some(x) => x.get.surface
      case _ => ""
    }
    assert(subjectivePart.equals("太郎は"))
    val objectivePart:String = caseCheckMap.get("ヲ格") match {
      case Some(x) => x.get.surface
      case _ => ""
    }
    assert(objectivePart.equals("本を"))
    val complementPart:String = caseCheckMap.get("ニ格") match {
      case Some(x) => x.get.surface
      case _ => ""
    }
    assert(complementPart.equals("花子に"))
  }

  "太郎は花子に借りた本を返さなかった。" should "analyze correctly" in {
    val knowledgeForParser = KnowledgeForParser(UUID.random.toString, UUID.random.toString, Knowledge("太郎は花子に借りた本を返さなかった。", "ja_JP", "{}") )
    //否定文を認識できるか
    val o = SentenceParser.parse(knowledgeForParser)
    val denialExpression = o._1.filter(x => x._2.isDenialWord).head._2.surface
    assert(denialExpression.equals("返さなかった。"))
    //主張の中の基本的な格構造（ 主語、目的語、補語）を認識できているか？
    val sentence:String = o._1.map(x => x._2.currentId -> x._2).toSeq.sortBy(_._1).foldLeft("") { (acc, x) => acc + x._2.surface }
    assert(sentence.equals("太郎は花子に借りた本を返さなかった。"))
    val caseCheckMap = o._2.map(x =>  x.caseStr -> o._1.get(x.sourceId)).toMap[String, Option[KnowledgeBaseNode]]
    val subjectivePart:String = caseCheckMap.get("未格") match {
      case Some(x) => x.get.surface
      case _ => ""
    }
    assert(subjectivePart.equals("太郎は"))
    val objectivePart:String = caseCheckMap.get("ヲ格") match {
      case Some(x) => x.get.surface
      case _ => ""
    }
    assert(objectivePart.equals("本を"))
    val complementPart:String = caseCheckMap.get("ニ格") match {
      case Some(x) => x.get.surface
      case _ => ""
    }
    assert(complementPart.equals("花子に"))
  }


  "もし明日の天気が雨ならば、太郎は映画を見る予定です。" should "analyze correctly" in {
    val knowledgeForParser = KnowledgeForParser(UUID.random.toString, UUID.random.toString, Knowledge("もし明日の天気が雨ならば、太郎は映画を見る予定です。", "ja_JP", "{}") )
    val o = SentenceParser.parse(knowledgeForParser)
    val sentence:String = o._1.map(x => x._2.currentId -> x._2).toSeq.sortBy(_._1).foldLeft("") { (acc, x) => acc + x._2.surface }
    assert(sentence.equals("もし明日の天気が雨ならば、太郎は映画を見る予定です。"))

    val conditionalConnection = o._1.filter(x => x._2.isConditionalConnection).head._2.surface
    assert(conditionalConnection.equals("雨ならば、"))

    val conditionalConnectionIndex = o._1.filter(x => x._2.isConditionalConnection).head._2.currentId
    val premiseNodes = o._1.toSeq.sortBy(_._1).filter(_._2.currentId <= conditionalConnectionIndex).toMap[String, KnowledgeBaseNode]
    for(node <- premiseNodes){
      assert(node._2.nodeType == PREMISE.index)
    }
    val claimNodes = o._1.toSeq.sortBy(_._1).filter(_._2.currentId > conditionalConnectionIndex).toMap[String, KnowledgeBaseNode]
    for(node <- claimNodes){
      assert(node._2.nodeType == CLAIM.index)
    }
    //前提と主張の間のエッジは、logicNodeの関係になっているか？
    val logicEdge = o._2.filter(_.logicType.equals("IMP")).head
    val premiseNode =  o._1.get(logicEdge.sourceId) match {
      case Some(x) => x.surface
      case _ => ""
    }
    val claimNode =  o._1.get(logicEdge.destinationId) match {
      case Some(x) => x.surface
      case _ => ""
    }
    assert(premiseNode.equals("雨ならば、"))
    assert(claimNode.equals("予定です。"))
  }

  "太郎が留学経験者である場合、もしくは太郎が幼児教育に携わった経験があるならば、太郎は採用されるかもしれない。" should "analyze correctly" in {
    //前提に複数の並列関係がある場合、前提内のORを適切に認識できるか
    val knowledgeForParser = KnowledgeForParser(UUID.random.toString, UUID.random.toString, Knowledge("太郎が留学経験者である場合、もしくは太郎が幼児教育に携わった経験があるならば、太郎は採用されるかもしれない。", "ja_JP", "{}") )
    val o = SentenceParser.parse(knowledgeForParser)
    val sentence:String = o._1.map(x => x._2.currentId -> x._2).toSeq.sortBy(_._1).foldLeft("") { (acc, x) => acc + x._2.surface }
    assert(sentence.equals("太郎が留学経験者である場合、もしくは太郎が幼児教育に携わった経験があるならば、太郎は採用されるかもしれない。"))

    val conditionalConnection = o._1.filter(x => x._2.isConditionalConnection).head._2.surface
    assert(conditionalConnection.equals("あるならば、"))

    val logicEdge = o._2.filter(_.logicType.equals("IMP")).head
    val premiseNode =  o._1.get(logicEdge.sourceId) match {
      case Some(x) => x.surface
      case _ => ""
    }
    val claimNode =  o._1.get(logicEdge.destinationId) match {
      case Some(x) => x.surface
      case _ => ""
    }
    assert(premiseNode.equals("あるならば、"))
    assert(claimNode.equals("採用されるかもしれない。"))

    val orEdge = o._2.filter(_.logicType.equals("OR")).head
    val node1 =  o._1.get(orEdge.sourceId) match {
      case Some(x) => x.surface
      case _ => ""
    }
    val node2 =  o._1.get(orEdge.destinationId) match {
      case Some(x) => x.surface
      case _ => ""
    }
    assert(node1.equals("場合、もしくは"))
    assert(node2.equals("経験が"))
  }

  "太郎が留学経験者である場合、あるいは太郎が幼児教育に携わった経験があるならば、太郎は採用されるかもしれない。" should "analyze correctly" in {
    //前提に複数の並列関係がある場合、前提内のORを適切に認識できるか
    val knowledgeForParser = KnowledgeForParser(UUID.random.toString, UUID.random.toString, Knowledge("太郎が留学経験者である場合、あるいは太郎が幼児教育に携わった経験があるならば、太郎は採用されるかもしれない。", "ja_JP", "{}") )
    val o = SentenceParser.parse(knowledgeForParser)
    val orEdge = o._2.filter(_.logicType.equals("OR")).head
    val node1 =  o._1.get(orEdge.sourceId) match {
      case Some(x) => x.surface
      case _ => ""
    }
    val node2 =  o._1.get(orEdge.destinationId) match {
      case Some(x) => x.surface
      case _ => ""
    }
    assert(node1.equals("場合、あるいは"))
    assert(node2.equals("経験が"))
  }

  "太郎が留学経験者である場合、または太郎が幼児教育に携わった経験があるならば、太郎は採用されるかもしれない。" should "analyze correctly" in {
    //前提に複数の並列関係がある場合、前提内のORを適切に認識できるか
    val knowledgeForParser = KnowledgeForParser(UUID.random.toString, UUID.random.toString, Knowledge("太郎が留学経験者である場合、または太郎が幼児教育に携わった経験があるならば、太郎は採用されるかもしれない。", "ja_JP", "{}") )
    val o = SentenceParser.parse(knowledgeForParser)
    val orEdge = o._2.filter(_.logicType.equals("OR")).head
    val node1 =  o._1.get(orEdge.sourceId) match {
      case Some(x) => x.surface
      case _ => ""
    }
    val node2 =  o._1.get(orEdge.destinationId) match {
      case Some(x) => x.surface
      case _ => ""
    }
    assert(node1.equals("場合、または"))
    assert(node2.equals("経験が"))
  }


  "太郎が留学経験者である場合、かつ太郎が幼児教育に携わった経験があるならば、太郎は採用されるかもしれない。" should "analyze correctly" in {
    //前提に複数の並列関係がある場合、前提内のANDを適切に認識できるか
    val knowledgeForParser = KnowledgeForParser(UUID.random.toString, UUID.random.toString, Knowledge("太郎が留学経験者である場合、かつ太郎が幼児教育に携わった経験があるならば、太郎は採用されるかもしれない。", "ja_JP", "{}") )
    val o = SentenceParser.parse(knowledgeForParser)
    val sentence:String = o._1.map(x => x._2.currentId -> x._2).toSeq.sortBy(_._1).foldLeft("") { (acc, x) => acc + x._2.surface }
    assert(sentence.equals("太郎が留学経験者である場合、かつ太郎が幼児教育に携わった経験があるならば、太郎は採用されるかもしれない。"))

    val conditionalConnection = o._1.filter(x => x._2.isConditionalConnection).head._2.surface
    assert(conditionalConnection.equals("あるならば、"))

    val logicEdge = o._2.filter(_.logicType.equals("IMP")).head
    val premiseNode =  o._1.get(logicEdge.sourceId) match {
      case Some(x) => x.surface
      case _ => ""
    }
    val claimNode =  o._1.get(logicEdge.destinationId) match {
      case Some(x) => x.surface
      case _ => ""
    }
    assert(premiseNode.equals("あるならば、"))
    assert(claimNode.equals("採用されるかもしれない。"))

    val andEdge = o._2.filter(_.logicType.equals("AND")).head
    val node1 =  o._1.get(andEdge.sourceId) match {
      case Some(x) => x.surface
      case _ => ""
    }
    val node2 =  o._1.get(andEdge.destinationId) match {
      case Some(x) => x.surface
      case _ => ""
    }
    assert(node1.equals("場合、かつ"))
    assert(node2.equals("経験が"))
  }

  "太郎が留学経験者である場合、及び太郎が幼児教育に携わった経験があるならば、太郎は採用されるかもしれない。" should "analyze correctly" in {
    //前提に複数の並列関係がある場合、前提内のANDを適切に認識できるか
    val knowledgeForParser = KnowledgeForParser(UUID.random.toString, UUID.random.toString, Knowledge("太郎が留学経験者である場合、及び太郎が幼児教育に携わった経験があるならば、太郎は採用されるかもしれない。", "ja_JP", "{}") )
    val o = SentenceParser.parse(knowledgeForParser)
    val andEdge = o._2.filter(_.logicType.equals("AND")).head
    val node1 =  o._1.get(andEdge.sourceId) match {
      case Some(x) => x.surface
      case _ => ""
    }
    val node2 =  o._1.get(andEdge.destinationId) match {
      case Some(x) => x.surface
      case _ => ""
    }
    assert(node1.equals("場合、及び"))
    assert(node2.equals("経験が"))
  }

  "太郎が留学経験者である場合、太郎は採用され、かつ太郎の給料は今より上がるだろう。" should "analyze correctly" in {
    //主張に複数の並列関係がある場合、主張内のANDを適切に認識できるか
    val knowledgeForParser = KnowledgeForParser(UUID.random.toString, UUID.random.toString, Knowledge("太郎が留学経験者である場合、太郎は採用され、かつ太郎の給料は今より上がるだろう。", "ja_JP", "{}") )
    val o = SentenceParser.parse(knowledgeForParser)
    val andEdge = o._2.filter(_.logicType.equals("AND")).head
    val node1 =  o._1.get(andEdge.sourceId) match {
      case Some(x) => x.surface
      case _ => ""
    }
    val node2 =  o._1.get(andEdge.destinationId) match {
      case Some(x) => x.surface
      case _ => ""
    }
    assert(node1.equals("採用され、かつ"))
    assert(node2.equals("上がるだろう。"))
  }

  "太郎が留学経験者である場合、太郎は採用され、もしくは太郎の給料は今より上がるだろう。" should "analyze correctly" in {
    //主張に複数の並列関係がある場合、主張内のORを適切に認識できるか
    val knowledgeForParser = KnowledgeForParser(UUID.random.toString, UUID.random.toString, Knowledge("太郎が留学経験者である場合、太郎は採用され、もしくは太郎の給料は今より上がるだろう。", "ja_JP", "{}") )
    val o = SentenceParser.parse(knowledgeForParser)
    val orEdge = o._2.filter(_.logicType.equals("OR")).head
    val node1 =  o._1.get(orEdge.sourceId) match {
      case Some(x) => x.surface
      case _ => ""
    }
    val node2 =  o._1.get(orEdge.destinationId) match {
      case Some(x) => x.surface
      case _ => ""
    }
    assert(node1.equals("採用され、もしくは"))
    assert(node2.equals("上がるだろう。"))
  }

  "太郎の趣味はピアノです。花子の趣味はガーデニングです。" should "analyze correctly" in {
    //複数文書を処理できるか？
    val knowledgeForParser = KnowledgeForParser(UUID.random.toString, UUID.random.toString, Knowledge("太郎の趣味はピアノです。花子の趣味はガーデニングです。", "ja_JP", "{}") )
    val o = SentenceParser.parse(knowledgeForParser)
    val sentence: String = o._1.map(x => x._2.currentId -> x._2).toSeq.sortBy(_._1).foldLeft("") { (acc, x) => acc + x._2.surface }
    assert(sentence.equals("太郎の趣味はピアノです。花子の趣味はガーデニングです。"))

    //文末同士の関係がないことを確認
    assert(o._2.filter(_.caseStr.equals("文末")).size == 0)

  }

  "株式会社ｱｲｳｴｵは２０００年４月１５日に４０００万円をある企業に支払った。"should "analyze correctly" in {
    //NERのチェック
    val knowledgeForParser = KnowledgeForParser(UUID.random.toString, UUID.random.toString, Knowledge("株式会社ｱｲｳｴｵは２０００年４月１５日に４０００万円をある企業に支払った。", "ja_JP", "{}") )
    val o = SentenceParser.parse(knowledgeForParser)
    val ne: List[String] = o._1.map(x => x._2.currentId -> x._2).toSeq.sortBy(_._1).foldLeft(List.empty[String]) { (acc, x) => x._2.namedEntity match {
      case "" => acc
      case _ => x._2.namedEntity + ":" + x._2.surface :: acc
    }}
    assert(ne.mkString(",").equals("MONEY:４０００万円を,DATE:１５日に,ORGANIZATION:株式会社アイウエオは"))
  }

  "主張１はファクト１２３４より正しい。"should "analyze correctly" in {
    //正規化表現の特別な場合のチェック
    val knowledgeForParser = KnowledgeForParser(UUID.random.toString, UUID.random.toString, Knowledge("主張１はファクト１２３４より正しい。", "ja_JP", "{}") )
    val o = SentenceParser.parse(knowledgeForParser)
    assert(o._1.filter(x =>  x._2.normalizedName == "主張１" || x._2.normalizedName == "ファクト１２３４").size == 2)
  }

}
