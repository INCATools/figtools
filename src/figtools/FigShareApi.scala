package figtools

import better.files._
import org.tsers.zeison.Zeison
import org.tsers.zeison.Zeison.{JUndefined, JValue}
import de.sciss.equal.Implicits._

import scala.annotation.tailrec
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

class FigShareApi
(url: String = "http://api.figshare.com/v1",
 raw: Boolean = false, timeout:
 Int = 10000)
{
  def get(id: String): JValue = {
    val figshareId = id.replaceAll("^.*/", "")
    val response = requests.get(s"$url/articles/$figshareId")
    val data = Zeison.parse(response.text)
    if (data("count").toInt === 0) throw new RuntimeException(s"Article not found with id $id")
    if (data("count").toInt > 1) throw new RuntimeException(s"More than one article found with id $id")
    data.items(0)
  }

  def dataPackage(article: JValue): JValue = {
    article.copy(
      "name"->article.title,
      "homepage"->article.figshare_url,
      "figshare_url"->JUndefined,
      "resources"->article.files.map{resource=>
        resource.copy(
          "url"->resource.download_url,
          "mediatype"->article.mime_type,
          "mime_type"->JUndefined
        )},
      "files"->JUndefined,
      "contributors"->article.authors.map{author=>
        Map("name"->
          (if(author.full_name.isDefined) author.full_name.toStr
           else s"${author.first_name.toStr} ${author.last_name.toStr}"))},
      "authors"->JUndefined,
      "author"->article.owner,
      "owner"->JUndefined)
  }

  def download(id: String, outDir: String = "."): Unit = {
    for (article <- get(id)) yield {
      val dir = outDir.toFile / id
      if (!dir.exists) dir.createDirectories()
      Console.err.println(s"Downloading article ${article.article_id.toInt} to folder $dir")
      val (name, metadata) = if (raw)
        (dir / "article.json", article)
      else (dir / "datapackage.json", dataPackage(article))
      name.writeText(Zeison.renderPretty(metadata))

      for (f <- article.files.toList.par) {
        requests.get.stream(s"${f.download_url.toStr}")(
          onDownload = inputStream => {
            (dir/f.name.toStr).writeBytes(inputStream.bytes)
          })
      }
    }
  }

  @tailrec final def search(query: String, page: Int = 1, pageSize: Int = 1000): Unit = {
    val response = requests.get("$url/articles/search",
      params=Map(
        "search_for"->query,
        "page"->page.toString,
        "page_size"->pageSize.toString))
    val data = Zeison.parse(response.text)
    if (data.error.isDefined) throw new RuntimeException(data.error.toStr)
    for (item <- data.items) {
      println(s"${item.article_id.toInt} ${item.title.toStr.replaceAll("(?i)</?p>","")}")
    }
    if (data.items.nonEmpty) {
      search(query, page + 1)
    }
  }

  @tailrec final def list(page: Int = 1, pageSize: Int = 1000): Unit = {
    val response = requests.get(s"$url/articles",
      params=Map(
        "page"->page.toString,
        "page_size"->pageSize.toString))
    val data = Zeison.parse(response.text)
    if (data.error.isDefined) throw new RuntimeException(data.error.toStr)
    for (result <- data.items) {
      println(s"${result.article_id.toInt} ${result.title.toStr.replaceAll("(?i)</?p>","")}")
    }
    list(page+1)
  }

  @tailrec final def downloadAll(outDir: String = ".", page: Int = 1, pageSize: Int = 1000): Unit = {
    val response = requests.get(s"$url/articles", params=Map(
      "page"->page.toString,
      "page_size"->pageSize.toString))
    val data = Zeison.parse(response.text)
    if (data.error.isDefined) throw new RuntimeException(data.error.toStr)
    for (result <- data.items.toList.par) {
      download(result.article_id.toInt.toString, outDir)
    }
    if (data.items.nonEmpty) {
      downloadAll(outDir, page+1)
    }
  }
}
