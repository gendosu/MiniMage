package shared

import play.api.mvc._
import play.api.libs.iteratee.Enumerator
import magick.MagickImage
import magick.ImageInfo
import magick.CompositeOperator

trait ImagesCommon extends Controller with Common {
  // 画像サイズ指定部分の文字列解析用正規表現
  val regWithBGColor = """([0-9]+)x([0-9]+)w_(..)_(..)_(..)""".r
  val regWithCut = """([0-9]+)x([0-9]+)c""".r
  val regXY = """([0-9]+)x([0-9]+)""".r
  val regY = """^x([0-9]+)""".r
  val regX = """^([0-9]+)x""".r
  val reg = """^([0-9]+)""".r

  protected def fileConvert(imageType:String, year:Int, month:Int, day:Int, imageId:String, size:String, ext:String, filePath:String, inHash:String):Result = {
    // リクエスト認証用ハッシュ文字列生成
    val hash = getHash(imageType, year, month, day, imageId, ext, size)
    // ハッシュ値がURLの物と一致しない場合、エラー
    Some(hash).collectFirst{case a if a == inHash => a}.map[Result](hash => {
      
      val info = new ImageInfo(filePath)
      val afterInfo = new ImageInfo(imageId + "." + ext.toLowerCase)
      val image = new MagickImage(info)
      var width:Float = 0
      var height:Float = 0
  
      // 画像のクオリティを指定する
      info.setQuality(100)
      info.setCompression(0)
      afterInfo.setQuality(100)
      afterInfo.setCompression(0)
      
      // イメージの情報を削除
      image.profileImage("*", null);  // IPTCとかEXIFとか
  
      val validSize:Option[MagickImage] = size match {
        // オリジナルサイズの場合、そのままサイズを返す
        case "org" => {
          Some(image.scaleImage(image.getDimension().getWidth.toInt, image.getDimension().getHeight.toInt))
        }
  
        case regWithBGColor(x, y, r, g, b) => {
          // x,yのサイズに収まるように縦横比を維持したまま縮小
          // 空白領域はr,g,bで指定された色で塗りつぶす
          // TODO 縦横比を固定で、x,yの範囲に収まるように拡大縮小
          val scaleX = x.toFloat / image.getDimension.getWidth
          val scaleY = y.toFloat / image.getDimension.getHeight
          
          val baseInfo = new ImageInfo("xc:#%s%s%s" format(r, g, b))
          baseInfo.setSize("%sx%s" format(x, y))
          val baseImage = new MagickImage(baseInfo)
          
          if(scaleX > scaleY) {
            val scaledImage = image.scaleImage((image.getDimension.getWidth * scaleY).toInt, (image.getDimension.getHeight * scaleY).toInt)
            baseImage.compositeImage(CompositeOperator.SrcOverCompositeOp, scaledImage, ((x.toInt / 2) - ((image.getDimension.getWidth * scaleY) / 2)).toInt, 0)
            scaledImage.destroyImages
          } else {
            val scaledImage = image.scaleImage((image.getDimension.getWidth * scaleX).toInt, (image.getDimension.getHeight * scaleX).toInt)
            baseImage.compositeImage(CompositeOperator.SrcOverCompositeOp, scaledImage, 0, ((y.toInt / 2) - ((image.getDimension.getHeight * scaleX) / 2)).toInt)
            scaledImage.destroyImages
          }
          
          Some(baseImage)
        }
        
        case regWithCut(x, y) => {
          // TODO x,yのサイズで画像をカット
          // とりあえず、まだ実装しない
          Some(image.scaleImage(image.getDimension.getWidth.toInt, image.getDimension.getHeight.toInt))
        }
        case regXY(x, y) => {
          // TODO 縦横比を固定で、x,yの範囲に収まるように拡大縮小
          val scaleX = x.toFloat / image.getDimension.getWidth
          val scaleY = y.toFloat / image.getDimension.getHeight
          if(scaleX > scaleY) {
            Some(image.scaleImage((image.getDimension.getWidth * scaleY).toInt, (image.getDimension.getHeight * scaleY).toInt))
          } else {
            Some(image.scaleImage((image.getDimension.getWidth * scaleX).toInt, (image.getDimension.getHeight * scaleX).toInt))
          }
        }
        case regY(y) => {
          // 縦横比は固定で縦のサイズをyに設定
          Some(image.scaleImage((image.getDimension.getWidth).toInt, (image.getDimension.getHeight).toInt))
        }
        case regX(x) => {
          // 縦横比は固定で横のサイズをxに設定
          Some(image.scaleImage((image.getDimension.getWidth).toInt, (image.getDimension.getHeight).toInt))
        }
        case reg(x) => {
          // 縦横比は固定で、縦横の長い方の辺をxに設定
          Some(image.scaleImage((image.getDimension.getWidth).toInt, (image.getDimension.getHeight).toInt))
        }
        case _ => {
          // サイズ表現にマッチしない
          None
        }
      }
  
      // サイズ表現にマッチしている場合、縮小処理
      validSize.map(changedSize => {
        val image2 = changedSize
        image2.setImageFormat(ext.toLowerCase)
        
        val file = new java.io.File(filePath)
        val fileContent: Enumerator[Array[Byte]] = Enumerator.fromFile(file)
        
        val result = SimpleResult(
            header = ResponseHeader(200),
            body = Enumerator.apply(image2.imageToBlob(afterInfo))
        )
        
        image.destroyImages
        image2.destroyImages
        
        result
      }).getOrElse(NotFound)
    }).getOrElse(NotFound)
  }
}
