package AvailablePerson
import scala.collection.mutable.ArrayBuffer
import java.time.LocalTime
import scala.io.StdIn.readLine

object AvailablePerson extends App {
    val timetable: Map[String, Array[Int]] = Map(
      "Ram" -> Array.fill(48)(0),
      "Shyam" -> Array.fill(48)(0),
      "Hari" -> Array.fill(48)(0),
      "Sanju" -> Array.fill(48)(0)
    )

    def convertToMinutes(time: String): Int = {
      val Array(hours, minutes) = time.split(":").map(_.toInt)
      hours * 60 + minutes
    }

    def vectorToTime(vector: Array[Int]): List[(String, String)] = {
      var startTime: Option[String] = None
      var endTime: Option[String] = None
      var result: List[(String, String)] = Nil

      for (i <- vector.indices) {
        if (vector(i) == 1 && startTime.isEmpty) {
          startTime = Some(convertToTime(i * 30))
        } else if (vector(i) == 0 && startTime.isDefined && endTime.isEmpty) {
          endTime = Some(convertToTime(i * 30))
          result = (startTime.get, endTime.get) :: result
          startTime = None
          endTime = None
        }
      }

      result.reverse
    }

    def timeToVector(startTime: String, endTime: String): Array[Int] = {
      val startMinutes = convertToMinutes(startTime) / 30
      val endMinutes = convertToMinutes(endTime) / 30
      val vector = Array.fill(48)(0)

      for (i <- startMinutes to endMinutes) {
        vector(i) = 1
      }

      vector
    }

    def convertToTime(minutes: Int): String = {
      val hours = minutes / 60
      val mins = minutes % 60
      f"$hours%02d:$mins%02d"
    }

    def markAvailability(name: String, startTime: String, endTime: String): Unit = {
      val personAvailability = timetable.getOrElse(name, timeToVector(startTime, endTime))
      val startMinutes = convertToMinutes(startTime) / 30
      val endMinutes = convertToMinutes(endTime) / 30

      for (i <- startMinutes to endMinutes) {
        personAvailability(i) = 1
      }
    }

    def checkAvailability(startTime: String, endTime: String): List[String] = {
      val startMinutes = convertToMinutes(startTime) / 30
      val endMinutes = convertToMinutes(endTime) / 30

      val availablePersons = timetable.collect {
        case (name, availability) if availability.slice(startMinutes, endMinutes + 1).forall(_ == 1) =>
          name
      }

      availablePersons.toList
    }

    // Load timetable with given time ranges
    markAvailability("Ram", "02:00", "07:00")
    markAvailability("Ram", "15:00", "20:45")
    markAvailability("Shyam", "12:00", "18:30")
    markAvailability("Hari", "08:00", "23:59")
    markAvailability("Sanju", "02:00", "07:00")

    // User Input
    val startTime = scala.io.StdIn.readLine("Enter the start time (in HH:MM format): ")
    val endTime = scala.io.StdIn.readLine("Enter the end time (in HH:MM format): ")

    val availablePersons = checkAvailability(startTime, endTime)

    if (availablePersons.isEmpty) {
      println("No person is available at the specified time.")
    } else {
      val names = availablePersons.mkString(" and ")
      println(s"$names are available at the specified time.")
    }

    val vector = timeToVector(startTime, endTime)
    val timeRanges = vectorToTime(vector)

    println("Time ranges:")
    timeRanges.foreach { case (start, end) =>
      println(s"Start: $start, End: $end")
    }
}
