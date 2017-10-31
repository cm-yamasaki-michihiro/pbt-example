import scala.util.{ Failure, Try }

case class Post(userId: Long, comment: String) {
  def contains(forbiddenWord: ForbiddenWord): Boolean = comment.contains(forbiddenWord.word)
}

case class ForbiddenWord(word: String)

case class ForbiddenWordIsContainedException() extends RuntimeException

class PostService(postDAO: PostDAO, forbiddenWordsLoader: ForbiddenWordsLoader) {
  def doPost(post: Post): Try[Unit] = for {
    forbiddenWords <- forbiddenWordsLoader.load()

    _ <- if (forbiddenWords.exists(post.contains)) {
      Failure(ForbiddenWordIsContainedException())
    } else {
      postDAO.create(post)
    }
  } yield ()
}

trait PostDAO {
  def create(post: Post): Try[Unit]
}

trait ForbiddenWordsLoader {
  def load(): Try[Seq[ForbiddenWord]]
}
