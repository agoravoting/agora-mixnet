package models


import play.api.Play
import play.api.libs.json._

import java.sql.Timestamp
import java.util.Date

/** defines election presentation extra options for an election */
case class ElectionExtra(foo: Option[Int])

/** an url to be shown when presenting election data */
case class Url(title: String, url: String) 
/** defines a possible answer for a question asked in an election */
case class Answer(id: Int, category: String, details: String, sort_order: Int, urls: Seq[Url], text: String) 

/** defines a question asked in an election */
case class Question(description: String, layout: String, max: Int, min: Int, num_winners: Int, title: String,
  randomize_answer_order: Boolean, tally_type: String, answer_total_votes_percentage: String, answers: Seq[Answer])//, extra_options: Option[QuestionExtra]) 
  
  /** defines presentation options for an election */
case class ElectionPresentation(share_text: String, theme: String, urls: Seq[Url], theme_css: String, extra_options: Option[ElectionExtra])

/** an election configuration defines an election */
case class ElectionConfig(id: Long, layout: String, director: String, authorities: Seq[String], title: String, description: String,
  questions: Seq[Question], start_date: Timestamp, end_date: Timestamp, presentation: ElectionPresentation, real: Boolean, extra_data: Option[String])

/** used to return an election with config in structured form */
case class ElectionDTO(id: Long, configuration: ElectionConfig, state: String, startDate: Timestamp,
  endDate: Timestamp, pks: Option[String], results: Option[String], resultsUpdated: Option[Timestamp], real: Boolean)