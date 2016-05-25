package utils

import javax.inject._
import com.typesafe.config.ConfigFactory

case class Server(interface: String, startPort: Int, portRange: Int)
case class AgoraBoard(url: String)

object BoardConfig
{
  val configuration = ConfigFactory.load()
  val agoraboard = AgoraBoard(configuration.getString("agoraboard.url"))
  val server = Server( 
                configuration.getString("server.interface"),
                configuration.getInt("server.startPort"),
                configuration.getInt("server.portRange"))
}