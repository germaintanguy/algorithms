package part2.week1

import scala.io.Source

/**
 * Created by germaDoubleanguy on 14/04/15.
 *
 * Source: COURSERA - Algorithms: Design and Analysis, Part 2 - Programming Assignment #1
 *
 * QUESTION 1 :
 * In this programming problem and the next you'll code up the greedy algorithms from
 * lecture for minimizing the weighted sum of completion times..
 * Download the text file here. This file describes a set of jobs with positive and
 * Doubleegral weights and lengths.
 *
 * It has the format :
 *
 * [number_of_jobs] 
 * [job_1_weight] [job_1_length] 
 * [job_2_weight] [job_2_length] 
 * ... 
 *
 * For example, the third line of the file is "74 59", indicating thatthe second job has
 * weight 74 and length 59. You should NOT assume that edge weights or lengths are distinct.
 *
 * Your task in this problem is to run the greedy algorithm that schedules jobs in decreasing
 * order of the difference (weight - length). Recall from lecture that this algorithm
 * is not always optimal. IMPORTANT: if two jobs have equal difference (weight - length),
 * you should schedule the job with higher weight first. Beware: if you break ties in a different way,
 * you are likely to get the wrong answer.
 * You should report the sum of weighted completion times of the resulting schedule
 * --- a positive Doubleeger --- in the box below.
 *
 * QUESTION 2 :
 * For this problem, use the same data set as in the previous problem. Your task now is to run the
 * greedy algorithm that schedules jobs (optimally) in decreasing order of the ratio (weight/length).
 * In this algorithm, it does not matter how you break ties. You should report the sum of weighted
 * completion times of the resulting schedule --- a positive Doubleeger --- in the box below.
 *
 */
object Scheduler extends App {

  def readData(path: String): List[(Double, Double)] = {
    val dataSet = Source.fromFile(path).getLines.toList
    val nbJobs = dataSet.head.toDouble
    val jobs: List[(Double, Double)] = dataSet.drop(1).map {
      j =>
        val arr = j.split(" ")
        val jobWeights = arr(0).toDouble
        val jobLenght = arr(1).toDouble
        (jobWeights, jobLenght)
    }
    jobs
  }

  def scheduler(jobs: List[(Double, Double)], score: (Double, Double) => Double = scoreByDivision): Double = {
    val jobsOrderded = jobs.map(j => (score(j._1, j._2), j._1, j._2))
      .sortBy(j => (j._1, j._2))(Ordering.Tuple2(Ordering[Double].reverse, Ordering[Double].reverse))
      .map(j => (j._2, j._3))
    val w = jobsOrderded.head._1
    val l = jobsOrderded.head._2
    val c = w * l
    val completionTime = computeCompletionTime(jobsOrderded.tail, w, l, c)
    completionTime
  }

  private def computeCompletionTime(jobsOrderded: List[(Double, Double)], w: Double, l: Double, c: Double): Double = {
    if (jobsOrderded.isEmpty) {
      c
    } else {
      val newW = jobsOrderded.head._1
      val newLenght = jobsOrderded.head._2 + l
      val newC = (newLenght * newW) + c
      computeCompletionTime(jobsOrderded.tail, newW, newLenght, newC)
    }
  }

  private def scoreByDifference(w: Double, l: Double) = {
    w - l
  }

  private def scoreByDivision(w: Double, l: Double) = {
    w / l
  }

  val jobsToSchedule = readData("data/part2/week1/jobs.txt")
  println(scheduler(jobsToSchedule, scoreByDifference))
  println(scheduler(jobsToSchedule, scoreByDivision))

}
