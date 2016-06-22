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

package utils

import javax.inject._
import com.typesafe.config.ConfigFactory


object BoardConfig
{
  case class Server(interface: String, startPort: Int, portRange: Int)
  case class AgoraBoard(url: String)
  case class Auth(secret: String, expiry: Int)
  case class Booth(auth: Auth)
  
  val configuration = ConfigFactory.load()
  val agoraboard = AgoraBoard(configuration.getString("agoraboard.url"))
  val server = Server( 
                configuration.getString("server.interface"),
                configuration.getInt("server.startPort"),
                configuration.getInt("server.portRange"))
  val booth = Booth(
                Auth(
                    configuration.getString("booth.auth.secret"),
                    configuration.getInt("booth.auth.expiry")))
}