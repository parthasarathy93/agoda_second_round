package com.example

//#user-registry-a  ctor
import akka.actor.{ Actor, ActorLogging, Props }
import scala.collection.mutable.ArrayBuffer
import java.io.File
import com.typesafe.config.{ Config, ConfigFactory }
import spray.json.DefaultJsonProtocol

//#user-case-classes
final case class User(name: String, age: Int, countryOfResidence: String)
final case class Users(users: Seq[User])
//#user-case-classes

object UserRegistryActor {
  final case class ActionPerformed(description: String)
  final case object GetUsers
  final case class CreateUser(user: User)
  final case class GetUser(name: String)
  final case class DeleteUser(name: String)
  final case class CreateAPIToken(name: String)
  final case class GetHotel(name: String, city_name: String, order: Option[String])

  def props: Props = Props[UserRegistryActor]
}

class UserRegistryActor extends Actor with ActorLogging {
  import UserRegistryActor._
  var rate_limit: Long = _
  var global_limit: Long = 10000
  var users = Set.empty[User]
  var hotelcsv = new HotelCSVReader(".\\hoteldb.csv")
  val hotels = hotelcsv.readHotels()
  var api_key_vs_accessed_time = scala.collection.mutable.Map[String, Long]()
  var suspended_queue = ArrayBuffer[String]()
  var default_order = "asc"
  var apikeys = ArrayBuffer[String]()
  val config = ConfigFactory.parseFile(new File(".\\http.conf"))
  var api_key_vs_user = scala.collection.mutable.Map[String, (String, Long)]()
  var api_user = scala.collection.mutable.Map[String, String]()
  try {
    rate_limit = config.getLong("ratelimit") * 1000
  } catch {
    case e =>
      rate_limit = global_limit
  }

  //val rate_limit: Long = if (config.getLong("ratelimit") != null) config.getLong("ratelimit") else 10000

  def receive: Receive = {
    case GetUsers =>
      sender() ! Users(users.toSeq)
    case CreateUser(user) =>
      users += user
      sender() ! ActionPerformed(s"User ${user.name} created.")
    case GetUser(name) =>
      sender() ! users.find(_.name == name)
    //println(config.getInt("ratelimit"))
    case DeleteUser(name) =>
      users.find(_.name == name) foreach { user => users -= user }
      var tups = api_key_vs_user(name)
      var token = tups._1
      api_key_vs_user.remove(name)
      api_user.remove(token)
      sender() ! ActionPerformed(s"User ${name} deleted.")
    case CreateAPIToken(name) =>
      if (users.find(_.name == name) != None) {
        var token = ""
        if (api_key_vs_user.contains(name)) {
          var tups = api_key_vs_user(name)
          var ctime = System.currentTimeMillis();
          if (ctime - tups._2 >= 3600000) {
            token = randomString(24);
            apikeys += token
            apikeys -= tups._1
            api_key_vs_user(name) = (token, System.currentTimeMillis())
          } else {
            token = tups._1
          }
        } else {
          token = randomString(24);
          apikeys += token
          api_key_vs_user(name) = (token, System.currentTimeMillis())
        }
        api_user(token) = name
        sender() ! token;
      } else {
        sender() ! "Invalid User"
      }
    case GetHotel(name, city_name, order) =>
      if (isValidApi(name)) {
        var ctime = System.currentTimeMillis()
        println("Current" + ctime);
        var sendHotelDetails = false
        println("Suspended Queue" + suspended_queue)
        if (api_key_vs_accessed_time.contains(name)) {
          var last_accessed = api_key_vs_accessed_time(name)
          println(last_accessed);
          var timeDiff = Math.abs(ctime - last_accessed)
          println("The time between last request and curr request" + timeDiff)
          if (timeDiff >= rate_limit) {
            if (!suspended_queue.contains(name)) {
              api_key_vs_accessed_time(name) = ctime
              sendHotelDetails = true
            } else {
              if (ctime >= last_accessed) {
                api_key_vs_accessed_time(name) = ctime
                suspended_queue -= name
                sendHotelDetails = true
              }
            }
          } else {
            //Suspending for next 5 mins
            if (!suspended_queue.contains(name)) {
              api_key_vs_accessed_time(name) = ctime + 300000
              suspended_queue += name
            }
          }
        } else {
          api_key_vs_accessed_time(name) = ctime
          sendHotelDetails = true
        }
        if (order != None) {
          default_order = order.get
        }
        // println((hotels.filter(_.city == city_name).sortWith(_.price < _.price).mkString(" ")))
        var temp = hotels.filter(_.city == city_name)
        if (sendHotelDetails) {
          if (temp.isEmpty) {
            sender() ! "No information is available regarding this city"
          }
          if (default_order == "asc") {
            sender() ! (temp.sortWith(_.price < _.price).mkString("  "))
          } else {
            sender() ! (temp.sortWith(_.price > _.price).mkString("  "))
          }
        } else {
          sender() ! ("You crossed the ratelimit try again after 5 minutes")
        }
      } else {
        sender() ! ("Invalid Apikey")
      }
  }

  def isValidApi(name: String): Boolean = {
    if (api_user.contains(name) && api_key_vs_user.contains(api_user(name))) {
      var tups: (String, Long) = api_key_vs_user(api_user(name))
      var ctime = System.currentTimeMillis();
      if (ctime - tups._2 >= 3600000) {
        api_user.remove(name)
        false

      } else {
        true
      }
    } else {
      false
    }
  }

  def randomString(len: Int): String = {
    val rand = new scala.util.Random(System.nanoTime)
    val sb = new StringBuilder(len)
    val ab = "0123456789abcdefghijklmnopqrstuvwxyz"
    for (i <- 0 until len) {
      sb.append(ab(rand.nextInt(ab.length)))
    }
    sb.toString
  }
}
//#user-registry-actor