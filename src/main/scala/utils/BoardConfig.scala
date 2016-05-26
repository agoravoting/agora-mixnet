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