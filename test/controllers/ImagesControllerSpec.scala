package controllers

import org.specs2.mutable.Specification
import play.api.test._
import play.api.test.Helpers._
import shared.Common
import play.api.Play

class ImagesControllerSpec extends Specification {
  "The 'getDateArray' array" should {
    "get Array(2015, 05,10)" in {
      running(FakeApplication(additionalConfiguration = Map("images.directory" -> "./test.images"))) {
        
        println(Play.current.configuration.getString("images.directory").getOrElse("/var/images"))
        val result = controllers.ImagesController.index("sample", 2012, 5, 10, "test-image-id", "100x100", "test-hash", "jpg")(FakeRequest())
        
        status(result) must equalTo(OK)
      }
    }
  }
}
