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

package accumulator

import shapeless._
import ops.nat._
import app._
import models._
import play.api.libs.json._
import java.sql.Timestamp
import election.Election
import election.Combined

object ElectionDTOData {
  val REGISTERED = "registered"
  val CREATED = "created"
  val CREATE_ERROR = "create_error"
  val STARTED = "started"
  val STOPPED = "stopped"
  val TALLY_OK = "tally_ok"
  val TALLY_ERROR = "tally_error"
  val RESULTS_OK = "results_ok"
  val DOING_TALLY = "doing_tally"
  val RESULTS_PUB = "results_pub"
}

class ElectionDTOData(val id: Long, val numAuth: Int) {
  
  private var state = initDTO()
  def apply() = state
  
  private def genAuthArray(): Array[String] = {
    var authArray : Array[String] = Array()
    if(numAuth > 1) {
      for(index <- 2 to numAuth) {
        authArray = authArray :+ ("auth" + index)
      }
    }
    authArray
  }
  
  private def initDTO(): ElectionDTO = {
    val startDate = new Timestamp(2015, 1, 27, 16, 0, 0, 1)
    ElectionDTO(
        id,
        ElectionConfig(
            id,
            "simple",
            "auth1",
            genAuthArray(),
            "Election title",
            "Election description",
            Seq(
                Question(
                    "Question 0",
                    "accordion",
                    1,
                    1,
                    1,
                    "Question title",
                    true,
                    "plurality-at-large",
                    "over-total-valid-votes",
                    Seq(
                      Answer(
                          0,
                          "",
                          "",
                          0,
                          Seq(),
                          "voting option A"
                      ),
                      Answer(
                          1,
                          "",
                          "",
                          1,
                          Seq(),
                          "voting option B"
                      )
                    )
                )
            ),
            startDate,
            startDate,
            ElectionPresentation(
                "",
                "default",
                Seq(),
                "",
                None
            ),
            false,
            None
        ),
        ElectionDTOData.REGISTERED,
        startDate,
        startDate,
        None,
        None,
        None,
        false
    )
  }
  
  def setDTO(dto: ElectionDTO) = {
    state = dto
  }
  
  def setState(newState: String) {
    state = 
      ElectionDTO(
          state.id,
          state.configuration,
          newState,
          state.startDate,
          state.endDate,
          state.pks,
          state.results,
          state.resultsUpdated,
          state.real
      )
  }
  
  def setResults(results: String) {
    state = 
      ElectionDTO(
          state.id,
          state.configuration,
          state.state,
          state.startDate,
          state.endDate,
          state.pks,
          Some(results),
          state.resultsUpdated,
          state.real
      )
  }
  
  def setPublicKeys[W <: Nat : ToInt](combined: Election[W, Combined]) {
    
    val jsPk : JsValue = 
      Json.arr(Json.obj(
          "q" -> combined.state.cSettings.group.getOrder().toString(),
          "p" -> combined.state.cSettings.group.getModulus().toString(),
          "y" -> combined.state.publicKey,
          "g" -> combined.state.cSettings.generator.getValue().toString()
      ))
      
    state = 
      ElectionDTO(
          state.id,
          state.configuration,
          state.state,
          state.startDate,
          state.endDate,
          Some(jsPk.toString()),
          state.results,
          state.resultsUpdated,
          state.real
      )
  }
}