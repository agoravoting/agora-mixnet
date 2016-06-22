/**
 * This file is part of agora-mixnet.
 * Copyright (C) 2015-2016  Agora Voting SL <agora@agoravoting.com>

 * agora-mixnet is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License.

 * agora-mixnet is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.

 * You should have received a copy of the GNU Affero General Public License
 * along with agora-mixnet.  If not, see <http://www.gnu.org/licenses/>.
**/

package election

import models._
import shapeless._
import nat._
import syntax.sized._
import ops.nat._
import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, Materializer}
import scala.util.{Try, Success, Failure}
import scala.concurrent.{Future, Promise}
import play.api.libs.json._

/** defines a possible answer for a question asked in an election */
case class TallyAnswer(id: Int, category: String, details: String, sort_order: Int, urls: Seq[Url], text: String, total_count: Int, winner_position: Option[Int]) 

case class Totals(blank_votes: Int, null_votes: Int, valid_votes: Int)

/** defines a question asked in an election */
case class TallyQuestion(description: String, layout: String, max: Int, min: Int, num_winners: Int, title: String,
  randomize_answer_order: Boolean, tally_type: String, answer_total_votes_percentage: String, answers: Seq[TallyAnswer],
  totals: Totals, winners: Seq[String])
  
case class Tally(questions: Seq[TallyQuestion])
  
object Tally
{
  implicit val system = ActorSystem()
  implicit val executor = system.dispatchers.lookup("my-other-dispatcher")
  implicit val materializer = ActorMaterializer()
  
  implicit val UrlF = Json.format[Url]
  implicit val TallyAnswerF = Json.format[TallyAnswer]
  implicit val TotalsF = Json.format[Totals]
  implicit val TallyQuestionF = Json.format[TallyQuestion]
  implicit val TallyF = Json.format[Tally]
  
  def tally[W <: Nat : ToInt](election: Election[W, Decrypted], dto: ElectionDTO) : Future[String] = {
    val promise = Promise[String]()
    Future {
      var questions : Seq[TallyQuestion] = Seq()
      
      // iterate over all questions (in this case there should be only one)
      dto.configuration.questions foreach { q =>
        //valid votes (index 0 will be blank votes)
        val tallyMap = scala.collection.mutable.Map[Int, Int]()
        // null votes
        var nullCount : Int = 0
        // initialize the tally to zero
        tallyMap += (0 -> 0)
        q.answers foreach { a =>
          tallyMap += (a.id + 1 -> 0)
        }
        // this is the main tally count
        election.state.decrypted foreach { voteStr =>
          val vote = voteStr.toInt
          tallyMap.get(vote) match {
            case Some(count) =>
              tallyMap += (vote -> (count + 1))
            case None =>
              nullCount = nullCount + 1
          }
        }
        val blankCount = tallyMap.get(0).get
        val totals = Totals ( blankCount, nullCount, election.state.decrypted.size - nullCount - blankCount)
        var winner = (0, 0)
        q.answers foreach { a =>
          val count = tallyMap.get(a.id  + 1).get
          if (count > winner._2) {
            winner = ((a.id + 1) -> count)
          }
        }
        var answers: Seq[TallyAnswer] = Seq()
        q.answers foreach { a =>
          answers = answers :+ 
            TallyAnswer(
                a.id,
                a.category,
                a.details,
                a.sort_order,
                a.urls,
                a.text,
                tallyMap.get(a.id + 1).get,
                (if(winner._1 == a.id + 1) { Some(0) } else { None })
            )
        }
        
        questions =  questions :+ TallyQuestion( q.description,
                       q.layout,
                       q.max,
                       q.min,
                       q.num_winners,
                       q.title,
                       q.randomize_answer_order,
                       q.tally_type,
                       q.answer_total_votes_percentage,
                       answers,
                       totals,
                       (if (winner._1 != 0) { Seq(winner._1.toString) } else { Seq() })
                     )
      }
      val results = Json.prettyPrint(Json.toJson(Tally(questions)))
      promise.success(results)
    } recover { case err =>
      promise.tryFailure(err)
    }
    promise.future
  }
  
  def getRawResults(decrypted: Seq[String]) : Future[String] = {
    val promise = Promise[String]()
    Future {
      var results : String = ""
      decrypted foreach { case vote =>
        results += "\"" + vote +"\"\n"
      }
      promise.success(results)
    } recover { case err =>
      promise.tryFailure(err)
    }
    promise.future
  }
}