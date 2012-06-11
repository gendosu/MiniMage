package controllers

import play.api.mvc._
import play.api.libs.iteratee.Enumerator
import play.api.Logger
import shared.Common
import shared.ImagesCommon

object ImagesController extends Controller with Common with ImagesCommon {
  /*
   * イメージを返す
   * imageType :: imagesディレクトリの第一階層のディレクトリ名を指定する
   * imageId   :: イメージのID
   * size      :: イメージのサイズ(default:org)
   * hash      :: imageType、imageId、sizeから算出したハッシュ値
   * ext       :: 拡張子
   */
  def index(imageType:String, year:Int, month:Int, day:Int, imageId:String, size:String = "org", hash:String, ext:String) = Action {
    val startNanoSecond = System.nanoTime
    
    // ファイルシステムに保存してあるファイル名
    getFilePathWithAnyExt(imageType, year, month, day, ext, imageId).map(filePath => {
      
      val result = fileConvert(imageType, year, month, day, imageId, size, ext, filePath, hash)
      
      Logger("index").debug("実行時間:" + ((System.nanoTime - startNanoSecond) / 1000000).toString + "ms")
      
      result
    }).getOrElse(NotFound)
  }
}
