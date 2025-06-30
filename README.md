# toposoid-sentence-parser-japanese
This component performs predicate argument structure analysis when a Japanese sentence is given as input.
Then, it outputs the information necessary for converting to a knowledge graph.
This library is mainly used by Toposoid developer in Toposoid projects.
Toposoid is a knowledge base construction platform.(see [Toposoid　Root Project](https://github.com/toposoid/toposoid.git))

[![Unit Test](https://github.com/toposoid/toposoid-sentence-parser-japanese/actions/workflows/action.yml/badge.svg)](https://github.com/toposoid/toposoid-sentence-parser-japanese/actions/workflows/action.yml)

## Requirements
Scala version 2.13.x,   
Sbt version 1.9.0
[KNP 4.19](https://nlp.ist.i.kyoto-u.ac.jp/?KNP)

## Recommended environment
* Required: at least 2GB of RAM
* Required: 10G or higher　of HDD
* ref. https://nlp.ist.i.kyoto-u.ac.jp/?KNP#t6a63e5c

## Setup
sbt publishLocal

## Usage
For example
For more information on the output, see https://github.com/toposoid/toposoid-knowledgebase-model.
Especially with KnowledgeBaseNode And KnowledgeBase Edge are important

```scala
import com.ideal.linked.toposoid.knowledgebase.regist.model.Knowledge
import com.ideal.linked.toposoid.protocol.model.parser.KnowledgeForParser
import io.jvm.uuid.UUID

object Test extends App {
  val sentence = "太郎は本を買いました。"
  val knowledgeForParser = KnowledgeForParser(UUID.random.toString, UUID.random.toString, Knowledge(sentence, "ja_JP", "{}") )
  val o = SentenceParser.parse(knowledgeForParser)
  for (element <- o._1) {
    println(element)
  }
  for (element <- o._2) {
    println(element)
  }
}
```

## Note

## License
This program is offered under a commercial and under the AGPL license.
For commercial licensing, contact us at https://toposoid.com/contact.  For AGPL licensing, see below.

AGPL licensing:
This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.


## Author
* Makoto Kubodera([Linked Ideal LLC.](https://linked-ideal.com/))

Thank you!
