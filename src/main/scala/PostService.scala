import scala.util.{ Failure, Try }

case class Post(userId: Long, comment: String) {
  def contains(forbiddenWord: ForbiddenWord): Boolean = comment.contains(forbiddenWord.word)
}

case class ForbiddenWord(word: String)

class ForbiddenWordIsContainedException(comment: String, forbiddenWord: ForbiddenWord) extends RuntimeException

class PostService(postDAO: PostDAO, forbiddenWordsLoader: ForbiddenWordsLoader) {
  def doPost(post: Post): Try[Unit] = for {
    forbiddenWords <- forbiddenWordsLoader.load()

    _ <- forbiddenWords.find(post.contains) match {
      case None => postDAO.create(post)
      case Some(forbiddenWord) => Failure(new ForbiddenWordIsContainedException(post.comment, forbiddenWord))
    }
  } yield ()
}

trait PostDAO {
  def create(post: Post): Try[Unit]
}

trait ForbiddenWordsLoader {
  def load(): Try[Seq[ForbiddenWord]]
}
