package yml

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import com.github.peterzeller.prettyprint.PrettyPrintDoc._
import isabelle.XML.Body
import isabelle.{XML, YXML}

object YmlParser {


  sealed abstract class Elem

  case class Keyword(num: Int, kind: String, contents: List[Elem]) extends Elem

  case class Comment(contents: List[Elem]) extends Elem

  case class Delete(contents: List[Elem]) extends Elem

  case class Accepted(contents: List[Elem]) extends Elem

  case class Running(contents: List[Elem]) extends Elem

  case class Finished(contents: List[Elem]) extends Elem

  case class Cartouche(contents: List[Elem]) extends Elem

  case class Antiquoted(contents: List[Elem]) extends Elem

  case class Language(contents: List[Elem]) extends Elem

  case class PlainText(contents: List[Elem]) extends Elem

  case class Words(contents: List[Elem]) extends Elem

  case class Initialized(contents: List[Elem]) extends Elem

  case class Empty(contents: List[Elem]) extends Elem

  case class Other(name: String, contents: List[Elem]) extends Elem

  case class Ignored(name: String, contents: Body) extends Elem

  case class Def(id: Int, contents: List[Elem]) extends Elem

  case class Ref(id: Int, def_line: Int, def_offset: Int, def_file: String, name: String, contents: List[Elem]) extends Elem

  case class Ref2(id: Int, contents: List[Elem]) extends Elem

  case class Word(t: String, value: String) extends Elem

  case class MarkdownParagraph(contents: List[Elem]) extends Elem

  case class MarkdownItem(contents: List[Elem]) extends Elem

  case class MarkdownList(contents: List[Elem]) extends Elem


  def parseElem(elem: XML.Tree): Elem =
    elem match {
      case XML.Elem(markup, body) =>
        lazy val props = markup.properties.toMap
        markup.name match {
          case "comment" =>
            assert(markup.properties.isEmpty, s"Properties: ${markup.properties}")
            Comment(parseList(body))
          case "delete" =>
            assert(markup.properties.isEmpty, s"Properties: ${markup.properties}")
            Delete(parseList(body))
          case "accepted" =>
            assert(markup.properties.isEmpty, s"Properties: ${markup.properties}")
            Accepted(parseList(body))
          case "running" =>
            assert(markup.properties.isEmpty, s"Properties: ${markup.properties}")
            Running(parseList(body))
          case "finished" =>
            assert(markup.properties.isEmpty, s"Properties: ${markup.properties}")
            Finished(parseList(body))
          case "keyword1" =>
            Keyword(1, props("kind"), parseList(body))
          case "keyword2" =>
            Keyword(2, props("kind"), parseList(body))
          case "entity" =>
            (for {
              id <- props.get("ref").map(_.toInt)
              line <- props.get("def_line").map(_.toInt)
              offset <- props.get("def_offset").map(_.toInt)
              file <- props.get("def_file")
              name <- props.get("name")
            } yield Ref(id, line, offset, file, name, parseList(body)))
              .getOrElse {
                (for {
                  id <- props.get("def").map(_.toInt)
                } yield Def(id, parseList(body)))
                  .getOrElse {
                    (for {
                      id <- props.get("ref").map(_.toInt)
                    } yield Ref2(id, parseList(body)))
                      .getOrElse(
                        Other("entity", parseList(body))
                      )
                  }
              }
          case "cartouche" =>
            Cartouche(parseList(body))
          case "antiquoted" =>
            Antiquoted(parseList(body))
          case "language" =>
            Language(parseList(body))
          case "plain_text" =>
            PlainText(parseList(body))
          case "words" =>
            Words(parseList(body))
          case "initialized" =>
            Initialized(parseList(body))
          case "timing" =>
            Empty(parseList(body))
          case "raw_text" =>
            Other("raw_text", parseList(body))
          case "operator" =>
            Other("operator", parseList(body))
          case "free" =>
            Other("free", parseList(body))
          case "delimiter" =>
            Other("delimiter", parseList(body))
          case "no_completion" =>
            Other("no_completion", parseList(body))
          case "string" =>
            Other("string", parseList(body))
          case "markdown_paragraph" =>
            MarkdownParagraph(parseList(body))
          case "markdown_list" =>
            MarkdownList(parseList(body))
          case "markdown_item" =>
            MarkdownItem(parseList(body))
          case "markdown_bullet" =>
            Other("markdown_bullet", parseList(body))
          case xml_elem =>
            Ignored("xml_elem", body)
          case x => throw new Exception(s"Unhandled: $x // $elem")
        }
      case XML.Text(content) =>
        Word("text", content)
    }

  def parseList(xml: Body): List[Elem] =
    for (elem <- xml) yield parseElem(elem)


  def generateHtmlL(r: List[Elem]): Doc = {
    sep("", r.map(generateHtml))
  }

  def parseText(s: String): Doc = {
    var res: Doc = ""
    val sb = new StringBuilder

    var pos = 0
    while (pos < s.length) {
      val c = s(pos)
      if (c == '\\') {
        pos += 1
        s(pos) match {
          case '<' =>
          val endPos = s.indexOf('>', pos)
          val text = s.substring(pos - 1, endPos + 1)
          Symbols.symbols.get(text) match {
            case Some(u) =>
              sb.append(u.asInstanceOf[Char])
            case None =>
              sb.append(text)
          }
          pos = endPos + 1
          case 'r' =>
          case 'n' =>
          case c2 =>
            throw new Exception(s"Unexpected escape \\$c2")
        }
      } else {
        pos += 1
        if (c == '\n') {
          res = res <> sb.toString() <> "<br/>" <> line
          sb.clear()
        } else {
          sb.append(c)
        }
      }

    }
    res <> sb.toString()
  }

  def generateHtml(r: Elem): Doc = {
    r match {
      case Keyword(num, kind, contents) =>
        xml("strong", "class" -> s"keyword$num")(contents)
      case Comment(contents) => generateHtmlL(contents)
      case Delete(contents) => generateHtmlL(contents)
      case Accepted(contents) => generateHtmlL(contents)
      case Running(contents) => generateHtmlL(contents)
      case Finished(contents) => generateHtmlL(contents)
      case Cartouche(contents) => generateHtmlL(contents)
      case Antiquoted(contents) => generateHtmlL(contents)
      case Language(contents) => generateHtmlL(contents)
      case PlainText(contents) => generateHtmlL(contents)
      case Words(contents) => generateHtmlL(contents)
      case Initialized(contents) => generateHtmlL(contents)
      case Empty(contents) => generateHtmlL(contents)
      case Other(name, contents) => generateHtmlL(contents)
      case Ignored(name, contents) => ""
      case Def(id, contents) => generateHtmlL(contents)
      case Ref(id, def_line, def_offset, def_file, name, contents) => generateHtmlL(contents)
      case Ref2(id, contents) => generateHtmlL(contents)
      case Word(t, value) => parseText(value)
      case MarkdownParagraph(contents) =>
        xml("p")(contents)
      case MarkdownItem(contents) => generateHtmlL(contents)
        xml("li")(contents)
      case MarkdownList(contents) => generateHtmlL(contents)
        xml("ul")(contents)
    }
  }

  def xml(tag: String, args: (String, String)*)(contents: List[Elem]): Doc =
    group(nested(2, "<" <> tag <> sep("", args.map(e => " " <> e._1 <> "=\"" <> e._2 <> "\"")) <> ">" </>
      generateHtmlL(contents)) </>
      "</" <> tag <> ">")


  def main(args: Array[String]): Unit = {
    val file = Paths.get("/home/peter/work/isabelle-by-example/dump/isabelle-by-example.rule_application/markup.yxml")
    val input = new String(Files.readAllBytes(file), StandardCharsets.UTF_8)
    val xml: Body = YXML.parse_body(input)

    val r = parseList(xml)

    val s = generateHtmlL(r)
    val s2 =
      s"""<!DOCTYPE html>
        |<html lang="en">
        |  <head>
        |    <meta charset="utf-8">
        |    <title>title</title>
        |  </head>
        |  <body>
        |    ${s.prettyStr(120)}
        |  </body>
        |</html>
        |""".stripMargin

    Files.write(Paths.get("input.xml"), s"<body>\n${xml.mkString("\n")}</body>".getBytes(StandardCharsets.UTF_8))
    Files.write(Paths.get("output.html"), s2.prettyStr(120).getBytes(StandardCharsets.UTF_8))

    // TODO print this to nice html (syntax highlighted, linked, markdown rendered, swap literal and rendered syntax)
  }


}
