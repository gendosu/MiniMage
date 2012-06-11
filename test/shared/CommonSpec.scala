package shared

import org.specs2.mutable._
import play.api.test._
import play.api.test.Helpers._
import play.api.Play
import play.Configuration

class CommonSpec extends Specification {
  "The 'getDateArray' array" should {
    "get Array(2015, 05,10)" in {
      running(FakeApplication()) {
        val common = new Common(){}
        val year = 2012
        val month = 5
        val day = 10
        common.getDateArray(year, month, day) must beEqualTo(Array("2012", "05", "10"))
      }
    }
  }

  "The 'getUrlOnlyPath' array" should {
    "get image uri" in {
      running(FakeApplication()) {
        val common = new Common(){}
        val year = 2012
        val month = 5
        val day = 10
        common.getUrlOnlyPath("imageType", 2015, 5, 15, "jpg", "hash") must beEqualTo("/imageType/2015/05/15/hash.jpg")
      }
    }
  }
}
