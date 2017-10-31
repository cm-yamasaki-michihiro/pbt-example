import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{ Matchers, WordSpec }

class SeqUtilSpec extends WordSpec with Matchers with GeneratorDrivenPropertyChecks {

  import SeqUtil._

  "sum()" should {
    "空のリストの合計は0" in {
      sum(List.empty[Int]) shouldBe 0 //ScalaTestの通常の書き方
    }

    "空ではないリストの合計はtailの合計にheadを足したもの" in
      forAll { (head: Int, tail: List[Int]) =>
        sum(head :: tail) shouldBe (head + sum(tail))
      }
  }
}
