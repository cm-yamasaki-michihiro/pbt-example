import org.mockito.ArgumentMatchers._
import org.mockito.Mockito._
import org.scalacheck.Arbitrary.{ arbitrary, _ }
import org.scalacheck.{ Arbitrary, Gen, Shrink }
import org.scalatest.mockito.MockitoSugar
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{ Matchers, WordSpec }

import scala.util.{ Failure, Success }

class PostServiceSpec extends WordSpec
  with Matchers
  with GeneratorDrivenPropertyChecks
  with MockitoSugar {

  trait Context {
    val postDaoMock: PostDAO = mock[PostDAO]
    val forbiddenWordsLoaderMock: ForbiddenWordsLoader =
      mock[ForbiddenWordsLoader]

    val postService = new PostService(postDaoMock, forbiddenWordsLoaderMock)
  }

  implicit val arbPost: Arbitrary[Post] = Arbitrary {
    for {
      userId <- Gen.posNum[Long]
      comment <- arbitrary[String]
    } yield Post(userId, comment)
  }

  implicit val arbForbiddenWord: Arbitrary[ForbiddenWord] = Arbitrary {
    arbitrary[String].map(ForbiddenWord)
  }

  /*下のようにも書けます
  implicit val arbPost: Arbitrary[Post] = Arbitrary(Gen.resultOf(Post))
  implicit val arbForbiddenWord: Arbitrary[ForbiddenWord] = Arbitrary(Gen.resultOf(ForbiddenWord))
  */

  // (Post, PostにマッチするForbiddenWord)を生成する
  def postAndForbiddenWordMatchesGen: Gen[(Post, ForbiddenWord)] =
    (for {
      post <- arbitrary[Post]
      start <- Gen.choose(0, post.comment.length)
      end <- Gen.choose(start, post.comment.length)
    } yield (post, ForbiddenWord(post.comment.substring(start, end))))
      .suchThat { case (post, word) => post.contains(word) }

  // （Post, PostにマッチするForbiddenWordを含んだリスト)を生成する
  def forbiddenWordsMatchesGen: Gen[(Post, List[ForbiddenWord])] =
    (for {
      (post, matches) <- postAndForbiddenWordMatchesGen

      prefix <- Gen.listOf(arbitrary[ForbiddenWord])
      postfix <- Gen.listOf(arbitrary[ForbiddenWord])
    } yield (post, prefix ++ List(matches) ++ postfix))
        .suchThat{ case (post, words) => words.exists(post.contains)}

  // (Post, PostにマッチしないForbiddenWord)を生成する
  def forbiddenWordNotMatchesGen: Gen[(Post, ForbiddenWord)] =
    arbitrary[(Post, ForbiddenWord)]
      .suchThat{case (post, forbiddenWord) => !post.contains(forbiddenWord)}


  // (Post, PostにマッチしないForbiddenWordのリスト)を生成する
  def forbiddenWordsNotMatchesGen: Gen[(Post, List[ForbiddenWord])] =
    arbitrary[(Post, List[ForbiddenWord])]
        .suchThat{case (post, forbiddenWords) => forbiddenWords.forall(word => !post.contains(word))}

  "post()" when {
    "Loaderでの処理が失敗した時" should {
      "例外をLeftに包んで返す" in new Context {
        forAll { (post: Post, exception: Exception) =>
          when(forbiddenWordsLoaderMock.load()).thenReturn(Failure(exception))
          postService.doPost(post) shouldBe Failure(exception)
        }
      }
    }

    "DAOでの処理が失敗した時" should {
      "例外をLeftに包んで返す" in new Context {
        forAll { (post: Post, exception: Exception) =>
          when(forbiddenWordsLoaderMock.load()).thenReturn(Success(Seq.empty))
          when(postDaoMock.create(any[Post]())).thenReturn(Failure(exception))
          postService.doPost(post) shouldBe Failure(exception)
        }
      }
    }

    "依存コンポーネントが正常に処理を終了する時" should {

      "禁止ワードが含まれないポストは正常に投稿される" in new Context {

        forAll(forbiddenWordsNotMatchesGen) {
          case (post: Post, forbiddenWords: Seq[ForbiddenWord]) =>
            when(forbiddenWordsLoaderMock.load()).thenReturn(Success(forbiddenWords))
            when(postDaoMock.create(any[Post])).thenReturn(Success(()))

            val result = postService.doPost(post)

            verify(postDaoMock, times(1)).create(post)
            result shouldBe Success(())
        }
      }

      "禁止ワードが含まれたポストは投稿されない" in new Context {

        forAll(forbiddenWordsMatchesGen) {
          case (post: Post, forbiddenWords: Seq[ForbiddenWord]) =>
            when(forbiddenWordsLoaderMock.load()).thenReturn(Success(forbiddenWords))

            val result = postService.doPost(post)

            verify(postDaoMock, never()).create(post)
            result shouldBe Failure(ForbiddenWordIsContainedException())
        }
      }
    }
  }
}
