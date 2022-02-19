package wikigraph

import wikigraph.Articles.ArticleId
import wikigraph.errors.WikiError.ArticleNotFound

import scala.concurrent.ExecutionContext.Implicits.global


/**
  * Analyze the graph of Wikipedia Articles
  *
  * @param client the wikipedia client providing access to the data.
  */
final class Wikigraph(client: Wikipedia):

  /**
    * Retrieves the names of the articles linked in a page.
    *
    * @param of the id of the page from which the links are retrieved
    *
    * Hint: Use the methods that you implemented in WikiResult.
    */
  def namedLinks(of: ArticleId): WikiResult[Set[String]] =
    for
      links <- client.linksFrom(of)
      seqOf <- WikiResult.traverse(links.toSeq)(client.nameOfArticle)
    yield
      seqOf.toSet
  end namedLinks

  /**
    * Computes the distance between two articles using breadth first search.
    *
    * @param start compute the distance from this node to `target`
    * @param target compute the distance from `start` to this node
    * @param maxDepth stop if the depth exceeds this value
    *
    * @return an asynchronous computation that might fail. If the maximal distance
    *         is exceeded during the search, the result is None
    *
    * Note: if a domain error occurs when jumping from node to node,
    *       fallback by ignoring the problematic node. On the other hand,
    *       any system failure just ends the algorithm by returning that
    *       system failure.
    *
    * Hint: More information is provided in the description of the assignment
    *       Use the `enqueue` and `dequeue` methods of `Queue`.
    */
  def breadthFirstSearch(start: ArticleId, target: ArticleId, maxDepth: Int): WikiResult[Option[Int]] =
    import scala.collection.immutable.Queue
    /**
      * This recursive method iterates on the graph.
      *
      * The algorithm is detailed in the assignment description.
      * - When the queue is empty or the maxDepth is exceeded (in the next element of the queue),
      *   the search fails with None
      * - Otherwise a node is retrieved from the queue and its neighbors fetched from the dataset.
      *   The search succeeds if `target` is in this set of neighbors.
      *   Otherwise we recursively search after modifying `visited` and adding the unknown
      *   neighbors to the queue with the correct distance.
      *
      * @param visited keep the nodes the are already visited, used no to iterate infinitely on
      *        graph cycles
      * @param q the next nodes to visit and their distance from `start`
      *
      * HINT: Have a look at the implementation of [[wikigraph.WikiResult#zip]] to see how to use
              [[wikigraph.WikiResult#flatMap]] to work with the content of [[wikigraph.WikiResult]].
              This is useful to chain successive calls to `iterate`.
      *
      * HINT: Do not forget, if a domain error occurs during exploration of the graph,
      *       to fallback by continuing iteration without modifying visited or q.
      *       Refer to the documentation of[[wikigraph.WikiResult#fallbackTo]].
      */
    // THE SECOND HINT IS WRONG!  You must modify q or you get into an infinite recursion.
    def iterate(visited: Set[ArticleId], q: Queue[(Int, ArticleId)]): WikiResult[Option[Int]] =
      q.dequeueOption match
        case None =>
          // this is the empty queue case
          WikiResult.successful(None)
        case Some(((distance, _), _)) if distance > maxDepth =>
          WikiResult.successful(None)
        case Some(((distance, articleId), _)) if articleId == target =>
          WikiResult.successful(Some(distance))
        case Some(((distance, articleId), qWithoutThisPair)) =>
          client.linksFrom(articleId)
            .flatMap { articleIds =>
              if articleIds.contains(target) then
                WikiResult.successful(Some(distance))
              else
                val newVisited = visited ++ articleIds
                val newQ = qWithoutThisPair.enqueueAll(articleIds.map(id => (distance+1) -> id))
                iterate(newVisited, newQ)
              end if
            }
            .fallbackTo {
              // THE SECOND HINT IS WRONG!  You must modify q or you get into an infinite recursion.
              iterate(visited, qWithoutThisPair)
            }
      end match
    end iterate

    if start == target then WikiResult.successful(Some(0))
    else iterate(Set(start), Queue(1->start))
  end breadthFirstSearch

  /**
    * Computes the distances between some pages provided the list of their titles.
    * Do not compute the distance from page and itself.
    *
    * @param titles names of the articles
    * @param maxDepth stop the search this value of distance is exceeded
    *
    * @return An asynchronous computation of the following form:
    *         Seq((distanceFromTitle, distanceToTitle, distance), ...)
    *
    * Hint: You should use the methods that you implemented on WikiResult as well as
    *       breadthFirstSearch
    */
  def distanceMatrix(titles: List[String], maxDepth: Int = 50): WikiResult[Seq[(String, String, Option[Int])]] =
    val titlePairs =
      for
        startTitle <- titles
        targetTitle <- titles if startTitle != targetTitle
      yield
        (startTitle, targetTitle)

    WikiResult.traverse(titlePairs){ (fromTitle, toTitle) =>
      // start the title->id lookups in parallel
      // @see https://danielwestheide.com/blog/the-neophytes-guide-to-scala-part-8-welcome-to-the-future/
      val fromIdFuture = client.searchId(fromTitle)
      val toIdFuture = client.searchId(toTitle)
      for
        fromId <- fromIdFuture
        toId <- toIdFuture
        bfsResult <- breadthFirstSearch(fromId, toId, maxDepth) // must run serially after IDs since depends on IDs
      yield
        (fromTitle, toTitle, bfsResult)
    }
  end distanceMatrix

end Wikigraph
