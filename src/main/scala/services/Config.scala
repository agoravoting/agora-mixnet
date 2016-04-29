package services

import javax.inject._
import com.typesafe.config.ConfigFactory

case class Server(url: String)
case class AgoraBoard(url: String)

object BoardConfig
{
  val configuration = ConfigFactory.load()
  val agoraboard = AgoraBoard(configuration.getString("agoraboard.url"))
  //val server = Server(configuration.getString("play.server.http.url").getOrElse("http://localhost:9500"))
}