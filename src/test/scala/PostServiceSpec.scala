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


  // PostにマッチするForbiddenWordを生成する
  def postAndForbiddenWordMatchesGen(post: Post): Gen[ForbiddenWord] =
    (for {
      post <- arbitrary[Post]
      start <- Gen.choose(0, post.comment.length)
      end <- Gen.choose(start, post.comment.length)
    } yield ForbiddenWord(post.comment.substring(start, end)))
      .suchThat { word => post.contains(word) }

  // PostにマッチするForbiddenWordを含んだリストを生成する
  def forbiddenWordListMatchesGen(post: Post): Gen[List[ForbiddenWord]] =
    (for {
      matches <- postAndForbiddenWordMatchesGen(post)

      prefix <- Gen.listOf(arbitrary[ForbiddenWord])
      postfix <- Gen.listOf(arbitrary[ForbiddenWord])
    } yield prefix ++ List(matches) ++ postfix)
      .suchThat { words => words.exists(post.contains) }

  // PostにマッチしないForbiddenWordを生成する
  def forbiddenWordNotMatchesGen(post: Post): Gen[ForbiddenWord] =
    arbitrary[ForbiddenWord]
      .suchThat { forbiddenWord => !post.contains(forbiddenWord) }


  // PostにマッチしないForbiddenWordのリストを生成する
  def forbiddenWordListNotMatchesGen(post: Post): Gen[List[ForbiddenWord]] =
    Gen.listOf(forbiddenWordNotMatchesGen(post))

  "post()" when {
    "Loaderでの処理が失敗した時" should {
      "例外をLeftに包んで返す" in new Context {
        forAll { (post: Post, exception: Exception) =>
          when(forbiddenWordsLoaderMock.load()).thenReturn(Failure(exception))

          val result = postService.doPost(post)

          result shouldBe Failure(exception)
        }
      }
    }

    "DAOでの処理が失敗した時" should {
      "例外をLeftに包んで返す" in new Context {
        forAll { (post: Post, exception: Exception) =>
          when(forbiddenWordsLoaderMock.load()).thenReturn(Success(Seq.empty))
          when(postDaoMock.create(post)).thenReturn(Failure(exception))

          val result = postService.doPost(post)

          result shouldBe Failure(exception)
        }
      }
    }


    "禁止ワードが含まれないポストは正常に投稿される" in new Context {
      forAll {(post: Post) =>
        forAll(forbiddenWordListNotMatchesGen(post), minSuccessful(1)) {
          forbiddenWords: Seq[ForbiddenWord] =>

            when(forbiddenWordsLoaderMock.load()).thenReturn(Success(forbiddenWords))
            when(postDaoMock.create(post)).thenReturn(Success(()))

            val result = postService.doPost(post)

            verify(postDaoMock, times(1)).create(post)
            result shouldBe Success(())
        }
      }
    }

    "禁止ワードが含まれたポストは投稿されない" in new Context {
      forAll { (post: Post) =>
        forAll(forbiddenWordListMatchesGen(post), minSuccessful(1)) {
          forbiddenWords: Seq[ForbiddenWord] =>

            when(forbiddenWordsLoaderMock.load()).thenReturn(Success(forbiddenWords))

            val result = postService.doPost(post)

            verify(postDaoMock, never()).create(post)
            result shouldBe Failure(ForbiddenWordIsContainedException())
        }
      }
    }
  }
}
