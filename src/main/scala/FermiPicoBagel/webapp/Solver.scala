package FermiPicoBagel.webapp

import scala.language.postfixOps
import scala.util.Random

class Solver(nDigits: Int, allowDuplicates: Boolean, allowZero: Boolean, allowLeadingZero: Boolean) {

  var candidates: List[String] = _

  def initialize(): this.type = {
    val range: Seq[Int] = if (allowZero) 0 to 9 else 1 to 9
    val digits: List[Int] = if (allowDuplicates) {
      List.fill(nDigits)(range).flatten
    } else {
      List.fill(1)(range).flatten
    }

    val permutations: Iterator[List[Int]] = digits.combinations(nDigits).flatMap(_.permutations)
    candidates = {
      if (allowLeadingZero)
        permutations
      else
        permutations filter (_.head != 0)
    } map {
      comb => comb.mkString("")
    } toList

    this
  }

  def getHint(implicit random: Random): String = {
    val hint = if (candidates.length <= 1000) {
      generateHint
    } else {
      candidates(random.nextInt(candidates.length))
    }

    hint
  }

  def addStep(guess: String, response: String): this.type = {
    val formattedResponse = formatResponse(response)
    candidates = candidates.filter {
      candidate =>
        responseMatch(calcResponse(candidate, guess), formattedResponse)
    }

    this
  }

  private def formatResponse(response: String): String = {
    if (response.length == nDigits) response
    else response + Seq.fill(nDigits - response.length)('B').mkString
  }

  private def responseMatch(r1: String, r2: String): Boolean = {
    r1.toLowerCase.toSeq.sorted == r2.toLowerCase.toSeq.sorted
  }

  private def calcResponse(candidate:String, guess: String): String = {
    val g = guess

    val f = candidate.zipWithIndex.intersect(g.zipWithIndex).length
    val p = candidate.toSeq.intersect(g).length - f
    val b = nDigits - f - p

    (1 to f).map(_ => 'F').mkString("") + (1 to p).map(_ => 'P').mkString("") + (1 to b).map(_ => 'B').mkString("")
  }

  private def generateMatrix: List[(String, String)] = {
    candidates flatMap {
      c1 =>
        candidates map {
          c2 =>
            (c1, c2)
        }
    }
  }

  private def generateHint: String = {
    val all = generateMatrix

    // (guess, candidate)
    val bestOfWorst = (all.groupBy(_._1) map {
      case (g, comb) =>
        val worstCase: (String, List[((String, String), String)]) = comb.map {
          case (guess, candidate) =>
            val response = calcResponse(candidate, guess)
            ((guess, candidate), response)
        }.groupBy(_._2).maxBy(_._2.size)

        (g, worstCase._2.size)
    }).minBy(_._2)

    bestOfWorst._1
  }
}

object Solver {
  implicit val random: Random = new Random()
}
