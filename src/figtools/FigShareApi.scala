package figtools

import better.files._
import com.softwaremill.sttp._
import com.softwaremill.sttp.HttpURLConnectionBackend
import org.tsers.zeison.Zeison
import org.tsers.zeison.Zeison.{JUndefined, JValue}

import scala.annotation.tailrec

class FigShareApi(url: String = "http://api.figshare.com/v1", raw: Boolean = false) {
  implicit val backend = HttpURLConnectionBackend()

  def get(id: String): JValue = {
    val figshareId = id.replaceAll("^.*/","")
    val response = sttp.get(uri"$url/articles/$figshareId").send()
    if (response.body.isLeft) throw new RuntimeException(response.body.left.get)
    val body = response.body.right.get
    val data = Zeison.parse(body)
    if (data("count").toInt == 0) throw new RuntimeException(s"Article not found with id $id")
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
    val article = get(id)
    val dir = outDir.toFile / id
    if (!dir.exists) dir.createDirectories()
    Console.err.println(s"Downloading article ${article.article_id.toInt} to folder $dir")
    (Seq(()=>{
      val (name, metadata) = if (raw)
        (dir / "article.json", article)
      else (dir / "datapackage.json", dataPackage(article))
     name.writeText(Zeison.renderPretty(metadata))
    }) ++ article.files.map{f=>()=>{
      val response = sttp.get(uri"${f.download_url.toStr}").response(asByteArray).send()
      if (response.body.isLeft) throw new RuntimeException(response.body.left.get)
      val bytes = response.body.right.get.toIterator
      (dir / f.name.toStr).writeBytes(bytes)
    }}).toList.par.foreach{f=>f()}
  }

  @tailrec final def search(query: String, page: Int = 1, pageSize: Int = 1000): Unit = {
    val response = sttp.get(uri"$url/articles/search?search_for=$query&page=$page&page_size=$pageSize").send()
    if (response.body.isLeft) throw new RuntimeException(response.body.left.get)
    val data = Zeison.parse(response.body.right.get)
    if (data.error.isDefined) throw new RuntimeException(data.error.toStr)
    for (result <- data.items) {
      println(s"${result.article_id.toInt} ${result.title.toStr.replaceAll("(?i)</?p>","")}")
    }
    if (data.items.toList.isEmpty) {}
    else search(query, page+1)
  }

  @tailrec final def list(page: Int = 1, pageSize: Int = 1000): Unit = {
    val response = sttp.get(uri"$url/articles?page=$page&page_size=$pageSize").send()
    if (response.body.isLeft) throw new RuntimeException(response.body.left.get)
    val data = Zeison.parse(response.body.right.get)
    if (data.error.isDefined) throw new RuntimeException(data.error.toStr)
    for (result <- data.items) {
      println(s"${result.article_id.toInt} ${result.title.toStr.replaceAll("(?i)</?p>","")}")
    }
    if (data.items.toList.isEmpty) {}
    else list(page+1)
  }

  @tailrec final def downloadAll(outDir: String = ".", page: Int = 1, pageSize: Int = 1000): Unit = {
    val response = sttp.get(uri"$url/articles?page=$page&page_size=$pageSize").send()
    if (response.body.isLeft) throw new RuntimeException(response.body.left.get)
    val data = Zeison.parse(response.body.right.get)
    if (data.error.isDefined) throw new RuntimeException(data.error.toStr)
    for (result <- data.items.toList.par) {
      download(result.article_id.toInt.toString, outDir)
    }
    if (data.items.toList.isEmpty) {}
    else downloadAll(outDir, page+1)
  }
}
