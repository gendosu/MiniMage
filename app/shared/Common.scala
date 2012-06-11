package shared

import java.security.MessageDigest
import play.api.Play
import java.util.Calendar
import java.io.File

trait Common {
  
  // 入力許可拡張子一覧
  val inExtList = Array("jpg", "jpeg", "png")
  
  // 出力許可拡張子一覧
  val extList = Array("jpg", "jpeg", "png")
  
  // 実画像保存ディレクトリ
  val imageBasePath = Play.current.configuration.getString("images.directory").getOrElse("/var/images")
  
  // 一時画像保存ディレクトリ
  val tmpImageBasePath = Play.current.configuration.getString("images.tmpDirectory").getOrElse("/var/images.tmp")
  
  val secretKey = Play.current.configuration.getString("secretkey").getOrElse("secret")

  // 年月日のArray
  def getDateArray(year:Int, month:Int, day:Int):Array[String] = Array(("%04d" format(year)), ("%02d" format(month)), ("%02d" format(day)))
  
  /*
   * ハッシュ値取得
   * imageType :: イメージの種類。このimageTypeで保存ディレクトリの最初の名前が決定される
   *  year :: 年。保存ディレクトリの2番目の名前
   *  month :: 月。保存ディレクトリの3番目の名前
   *  day :: 日。保存ディレクトリの4番目の名前
   *  imageId :: ファイルの名前
   *  ext :: ファイルの拡張子
   *  size :: サイズ指定文字列(デフォルトは:org)
   */
  def getHash(imageType:String, year:Int, month:Int, day:Int, imageId:String, ext:String, size:String = "org"):String = {
    val hashSeed = (Array(imageType).toList ::: getDateArray(year, month, day).toList ::: Array(imageId, size, ext.toLowerCase, secretKey).toList ::: Nil).mkString("-")
    val digestedBytes = MessageDigest.getInstance("MD5").digest(hashSeed.getBytes)
    
    digestedBytes.map("%02x".format(_)).mkString
  }

  def getFileHash(imageType:String, imageId:String, ext:String, cal:Calendar = Calendar.getInstance):String = {
    val dateSeed = "%tY-%<tm-%<td-%<th-%<tM-%<ts" format cal
    val hashSeed = Array(imageType, dateSeed, imageId, "org", ext).mkString("-")
    val digestedBytes = MessageDigest.getInstance("MD5").digest(hashSeed.getBytes)
    
    digestedBytes.map("%02x".format(_)).mkString
  }

  /*
   * ファイルパス取得
   * 基本になるディレクトリはapplication.confに設定してあるimages.directory。無ければ/var/images
   * imageType :: イメージの種類。このimageTypeで保存ディレクトリの最初の名前が決定される
   *  year :: 年。保存ディレクトリの2番目の名前
   *  month :: 月。保存ディレクトリの3番目の名前
   *  day :: 日。保存ディレクトリの4番目の名前
   *  imageId :: ファイルの名前
   *  ext :: ファイルの拡張子
   *  hash :: オリジナルファイルのハッシュ値
   */
  def getFilePath(imageType:String, year:Int, month:Int, day:Int, ext:String, imageId:String):String = {
    imageBasePath + "/" + (Array(imageType).toList ::: getDateArray(year, month, day).toList ::: Array(imageId + "." + ext.toLowerCase).toList ::: Nil).mkString("/")
  }

  def getTmpFilePath(imageType:String, year:Int, month:Int, day:Int, ext:String, imageId:String):String = {
    tmpImageBasePath + "/" + (Array(imageType).toList ::: getDateArray(year, month, day).toList ::: Array(imageId + "." + ext.toLowerCase).toList ::: Nil).mkString("/")
  }

  def getFilePathWithAnyExt(imageType:String, year:Int, month:Int, day:Int, ext:String, imageId:String):Option[String] = {
    val fileList = extList.collect({
      case a if(new File(imageBasePath + "/" + (Array(imageType).toList ::: getDateArray(year, month, day).toList ::: Array(imageId + "." + a).toList ::: Nil).mkString("/"))).exists => {imageBasePath + "/" + (Array(imageType).toList ::: getDateArray(year, month, day).toList ::: Array(imageId + "." + a).toList ::: Nil).mkString("/")}
    })
    
    if(!fileList.isEmpty) {
      Some(fileList.head)
    } else {
      None
    }
  }

  def getTmpFilePathWithAnyExt(imageType:String, year:Int, month:Int, day:Int, ext:String, imageId:String):Option[String] = {
    val fileList = extList.collect({
      case a if(new File(tmpImageBasePath + "/" + (Array(imageType).toList ::: getDateArray(year, month, day).toList ::: Array(imageId + "." + a).toList ::: Nil).mkString("/"))).exists => {tmpImageBasePath + "/" + (Array(imageType).toList ::: getDateArray(year, month, day).toList ::: Array(imageId + "." + a).toList ::: Nil).mkString("/")}
    })
    
    if(!fileList.isEmpty) {
      Some(fileList.head)
    } else {
      None
    }
  }

  /*
   * URLのパスの部分を取得
   * imageType :: イメージの種類。このimageTypeで保存ディレクトリの最初の名前が決定される
   *  year :: 年。保存ディレクトリの2番目の名前
   *  month :: 月。保存ディレクトリの3番目の名前
   *  day :: 日。保存ディレクトリの4番目の名前
   *  imageId :: ファイルの名前
   *  ext :: ファイルの拡張子
   *  hash :: オリジナルファイルのハッシュ値(getHashでsize = "org"の物)
   */
  def getUrlOnlyPath(imageType:String, year:Int, month:Int, day:Int, ext:String, hash:String):String = {
    "/" + (Array(imageType, "%04d" format(year), "%02d" format(month), "%02d" format(day)).toList ::: List(hash + "." + ext) ::: Nil).mkString("/")
  }
}
