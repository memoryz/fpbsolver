package FermiPicoBagel.webapp

import scala.language.postfixOps
import scala.util.Random

class Solver(nDigits: Int, allowDuplicates: Boolean, allowZero: Boolean, allowLeadingZero: Boolean) {

  import Solver._

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
      generateHint(candidates)
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
}

object Solver {
  implicit val random: Random = new Random()

  private def responseMatch(r1: String, r2: String): Boolean = {
    r1.toLowerCase.toSeq.sorted == r2.toLowerCase.toSeq.sorted
  }

  private def calcResponse(candidate: String, guess: String): String = {
    val n = candidate.length
    val f = candidate.zipWithIndex.intersect(guess.zipWithIndex).length
    val p = candidate.toSeq.intersect(guess).length - f
    val b = n - f - p

    (1 to f).map(_ => 'F').mkString("") + (1 to p).map(_ => 'P').mkString("") + (1 to b).map(_ => 'B').mkString("")
  }

  private def generateLowerDiagonalMatrix(candidates: List[String]): List[(String, String)] = {
    candidates flatMap {
      c1 =>
        candidates filter (_ < c1) map {
          c2 =>
            (c1, c2)
        }
    }
  }

  private val log2: Double=>Double = math.log(_) / math.log(2)

  private def generateHint(candidates: List[String]): String = {
    val lowerDiagonal = generateLowerDiagonalMatrix(candidates)

    val responseLD = lowerDiagonal map {
      case (guess, candidate) =>
        val response = calcResponse(candidate, guess)
        (guess, candidate, response)
    }

    val responseUD = responseLD map {
      case (guess, candidate, response) =>
        (candidate, guess, response)
    }

    val diagonal = candidates map {
      c => (c, c, "FFFF")
    }

    val allResponse = responseLD ++ responseUD ++ diagonal

    // (guess, candidate, response)
    // group by guess
    val bestOfWorst = allResponse.groupBy(_._1).map {
      case (guess, responseGroup) =>
        // group by response, and get each subgroup's size
        val cost = responseGroup
          .groupBy(_._3)
          .map(_._2.size)
          .map(s => s * log2(s))
          .sum
        (guess, cost)
    }.minBy(_._2)

    bestOfWorst._1
  }
}
