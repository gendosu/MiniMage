package controllers

import play.api._
import play.api.mvc._
import shared.Common
import java.util.Calendar
import java.io.File
import play.api.libs.json._
import magick.MagickImage
import magick.MagickException
import magick.ImageInfo

object ApiController extends Controller with Common {
  /*
   * ファイルをアップロードするAPIインターフェース
   * formのPOSTでアップロードする
   * ここでアップロードされた画像ファイルは[images.tmpDirectory]で指定されたディレクトリに一度格納される
   * [images.tmpDirectory]ディレクトリは一時保存場所
   * [images.tmpDirectory]ディレクトリに保存された画像ファイルは、cronなどで一定期間を過ぎたものを削除するようにする
   * 
   * formのパラメータ
   * image_type :: イメージ種類。画像格納ディレクトリ内の第一階層のディレクトリ名
   * image_id :: ファイル名etc
   * file :: 画像ファイルの実態
   */
  def upload = Action(parse.multipartFormData) { request =>
    val startNanoSecond = System.nanoTime
    request.body.file("file").map { picture =>
      
      // イメージ種類をフォームデータから取得
      val imageType = request.body.dataParts.get("image_type").get.head
      
      // イメージ名(id)を取得
      val imageId = request.body.dataParts.get("image_id").get.head
      
      // 拡張子を取得
      val ext = imageId.split("\\.").last.toLowerCase
      
      val fileList = inExtList.collect({
        case a if a == ext => ext
      })
      
      if(!fileList.isEmpty) {
        // ファイル名を取得
        val fileName = imageId.replaceAll("." + ext + "$", "")
        
        // 年月日を取得
        val cal = Calendar.getInstance
        val year = cal.get(Calendar.YEAR)
        val month = cal.get(Calendar.MONTH) + 1
        val day = cal.get(Calendar.DAY_OF_MONTH)
        
        val contentType = picture.contentType
  
        // ハッシュ値を取得
        val hash = getFileHash(imageType, imageId, ext, cal)
  
        // ファイルのフルパスを作成
        val filePath = getTmpFilePath(imageType, year, month, day, ext, hash)
  
        // アップロードファイルを保存
        picture.ref.moveTo(new File(filePath))
        
        // 画像ファイルの情報を取得
        val info = new ImageInfo(filePath)
        val image = new MagickImage(info)
        
        val width = image.getDimension.getWidth
        val height = image.getDimension.getHeight
        
        // 画像データを破棄
        image.destroyImages
        
        Logger("upload").debug("実行時間:" + ((System.nanoTime - startNanoSecond) / 1000000).toString + "ms")
  
        Ok(Json.toJson(
            Map("success" -> "true", "message" -> "", "path" -> getUrlOnlyPath(imageType, year, month, day, ext.toLowerCase, hash), "width" -> width.toString, "height" -> height.toString)
          )
        )
      } else {
        BadRequest(Json.toJson(
          Map( "success" -> "false", "message" -> ("Missing Image File Type [" + ext + "]"), "path" -> "", "width" -> "", "height" -> "")
        ))
      }
    }.getOrElse(BadRequest(Json.toJson(
      Map( "success" -> "false", "message" -> "Missing parameter [file]", "path" -> "", "width" -> "", "height" -> "")
    )))
  }
  
  
  /*
   * [images.tmpDirectory]ディレクトリにアップロードされた画像ファイルを確定させるためのAPIインターフェース
   * [images.tmpDirectory]ディレクトリに保存されている画像ファイルを[images.Directory]ディレクトリへ移動する
   * 
   * 実際のリクエストは
   *  /api/commit/:imageType/:year/:month/:day/:imageId.:ext
   * という形でURLにアクセスされたときにコミットとする
   * 
   * パラメータ
   * imageType :: 確定させたい画像ファイルのイメージ種類
   * year :: 確定させたい画像ファイルの年
   * month :: 確定させたい画像ファイルの月
   * day :: 確定させたい画像ファイルの日
   * ext:: 確定させたい画像ファイルの拡張子
   */
  def commit(imageType:String, year:Int, month:Int, day:Int, imageId:String, ext:String) = Action {
    val startNanoSecond = System.nanoTime
    
    try {
      // tmpファイルのフルパスを作成
      val tmpFilePath = getTmpFilePath(imageType, year, month, day, ext, imageId)
      
      // ファイルのフルパスを作成
      val filePath = getFilePath(imageType, year, month, day, ext, imageId)
      
      // 実領域にファイルが存在したら、確定済みとして判断
      if(new File(filePath).exists) {
        
        // tmp領域にファイルが存在したら削除する
        if(new File(tmpFilePath).exists) new File(tmpFilePath).delete
        
        Logger("commit").debug("実行時間:" + ((System.nanoTime - startNanoSecond) / 1000000).toString + "ms")
        
        Ok(Json.toJson(Map("success" -> "true", "message" -> "すでにコミット済み")))
      } else {
        // ディレクトリ作成
        new File(new File(filePath).getParent).mkdirs
        
        // 実領域にファイルを移動
        val tmpFile = new File(tmpFilePath)
      
        tmpFile.renameTo(new File(filePath))
      
        Logger("commit").debug("実行時間:" + ((System.nanoTime - startNanoSecond) / 1000000).toString + "ms")
        Ok(Json.toJson(Map("success" -> "true", "message" -> "")))
      }
    } catch {
      case e => InternalServerError(Json.toJson(Map("success" -> "false", "message" -> "予期せぬエラーが発生しました")))
    }
  }
  
  /*
   * [images.Directory]ディレクトリにアップロードされている画像ファイルを削除するためのAPIインターフェース
   * 
   * 実際のリクエストは
   *  /api/delete/:imageType/:year/:month/:day/:imageId.:ext
   * という形でURLにアクセスされたときにコミットとする
   * 
   * パラメータ
   * imageType :: 確定させたい画像ファイルのイメージ種類
   * year :: 確定させたい画像ファイルの年
   * month :: 確定させたい画像ファイルの月
   * day :: 確定させたい画像ファイルの日
   * ext:: 確定させたい画像ファイルの拡張子
   */
  def delete(imageType:String, year:Int, month:Int, day:Int, imageId:String, ext:String) = Action {
    val startNanoSecond = System.nanoTime
    
    try {
      // ファイルのフルパスを作成
      val filePath = getFilePath(imageType, year, month, day, ext, imageId)
      val file = new File(filePath)
      
      // 実領域にファイルが存在したら、削除
      if(file.exists) {
        val result = file.delete

        if(result) {
          Ok(Json.toJson(Map("success" -> "true", "message" -> "ファイル削除完了")))
        } else {
          InternalServerError(Json.toJson(Map("success" -> "false", "message" -> "ファイル削除エラー")))
        }
      } else {
        Ok(Json.toJson(Map("success" -> "true", "message" -> "ファイル削除済み、または未登録")))
      }
    } catch {
      case e => InternalServerError(Json.toJson(Map("success" -> "false", "message" -> "予期せぬエラーが発生しました")))
    }
  }
}
