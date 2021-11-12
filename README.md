# toposoid-sentence-parser-japanese
This component performs predicate argument structure analysis when a Japanese sentence is given as input.
Then, it outputs the information necessary for converting to a knowledge graph.
This library is mainly used by Toposoid developer in Toposoid projects.
Toposoid is a knowledge base construction platform.(see [Toposoid　Root Project](https://github.com/toposoid/toposoid.git))

[![Unit Test](https://github.com/toposoid/toposoid-sentence-parser-japanese/actions/workflows/action.yml/badge.svg)](https://github.com/toposoid/toposoid-sentence-parser-japanese/actions/workflows/action.yml)

## Requirements
Scala version 2.12.x,   
Sbt version 1.2.8
[KNP 4.19](https://nlp.ist.i.kyoto-u.ac.jp/?KNP)

## Recommended environment
* Required: at least 6GB of RAM
* Required: 10G or higher　of HDD

## Setup
sbt publishLocal

## Usage
For example
For more information on the output, see https://github.com/toposoid/toposoid-knowledgebase-model.
Especially with KnowledgeBaseNode And KnowledgeBase Edge are important

```scala

val o = SentenceParser.parse("太郎は本を買いました")
for (element <- o._1) {
  println(element)
}

for (element <- o._2) {
  println(element)
}
```

## Note

## License
toposoid/toposoid-sentence-parser is Open Source software released under the [Apache 2.0 license](https://www.apache.org/licenses/LICENSE-2.0.html).

## Author
* Makoto Kubodera([Linked Ideal LLC.](https://linked-ideal.com/))

Thank you!
