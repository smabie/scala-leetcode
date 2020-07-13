/*
 * 1086 High Five
 *
 * Given a list of scores of different students, return the average score of each student's
 * top five scores in the order of each student's id.
 */

/*
 * Simple
 */
def highFive(items: Array[Array[Int]]): Array[Array[Int]] = {
   items.groupBy(_(0)).mapValues { xs =>
     xs.sortBy(_(1))(Ordering[Int].reverse).take(5).foldLeft(0) { (x, y) =>
       x + y.last
     } / 5
   }.toArray.sortBy { case (id, _) => id }.map { case (id, x) => Array(id, x) }
}

highFive(Array(Array(1, 91), Array(1, 92), Array(2, 93),
  Array(2, 97), Array(1, 60), Array(2, 77), Array(1, 65), Array(1, 87),
  Array(1, 100), Array(2, 100), Array(2, 76)))