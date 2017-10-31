import org.mockito.ArgumentMatchers._
import org.mockito.Mockito._
import org.scalacheck.Arbitrary.{ arbitrary, _ }
import org.scalacheck.{ Arbitrary, Gen }
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

  // PostにマッチするForbiddenWordを生成する
  def forbiddenWordMatchesGen(post: Post): Gen[ForbiddenWord] =
    for {
      start <- Gen.choose(0, post.comment.length)
      end <- Gen.choose(start, post.comment.length)
    } yield ForbiddenWord(post.comment.substring(start, end))

  // PostにマッチするForbiddenWordを含んだリストを作成する
  def forbiddenWordsGenMatches(post: Post): Gen[List[ForbiddenWord]] =
    for {
      start <- Gen.choose(0, post.comment.length)
      end <- Gen.choose(start, post.comment.length)
      matches = ForbiddenWord(post.comment.substring(start, end))

      prefix <- Gen.listOf(arbitrary[ForbiddenWord])
      postfix <- Gen.listOf(arbitrary[ForbiddenWord])
    } yield {
      prefix ++ List(matches) ++ postfix
    }

  // PostにマッチしないForbiddenWordを生成する
  def forbiddenWordNotMatchesGen(post: Post): Gen[ForbiddenWord] =
    arbitrary[ForbiddenWord].filter{forbiddenWord =>
      !post.contains(forbiddenWord)
    }

  // PostにマッチしないForbiddenWordのリストを生成する
  def forbiddenWordsNotMatchesGen(post: Post): Gen[List[ForbiddenWord]] =
    Gen.listOf(forbiddenWordNotMatchesGen(post))

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
        //(Post, PostにマッチするForbiddenWordを含まないList[ForbiddenWord])を生成

        val paramGen = for {
          post <- arbitrary[Post]
          forbiddenWords <- forbiddenWordsNotMatchesGen(post)
        } yield (post, forbiddenWords)

        forAll(paramGen) {
          case (post: Post, forbiddenWords: Seq[ForbiddenWord]) =>
            when(forbiddenWordsLoaderMock.load()).thenReturn(Success(forbiddenWords))
            postService.doPost(post)
            verify(postDaoMock).create(post)
        }
      }

      "禁止ワードが含まれたポストは投稿されない" in new Context {

        //(Post, PostにマッチするForbiddenWordを含むList[ForbiddenWord])を生成
        val paramGen = for {
          post <- arbitrary[Post]
          forbiddenWords <- forbiddenWordsGenMatches(post)
        } yield (post, forbiddenWords)

        forAll(paramGen) {
          case (post: Post, forbiddenWords: Seq[ForbiddenWord]) =>
            when(forbiddenWordsLoaderMock.load()).thenReturn(Success(forbiddenWords))
            postService.doPost(post)
            verify(postDaoMock, never()).create(post)
        }
      }
    }
  }
}
