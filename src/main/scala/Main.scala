//Eduard Ailincai
//Bence Bansaghi
//Delia Fliscu

import play.api.libs.json.{JsObject, JsValue, Json}
import scalaj.http.{Http, HttpOptions}

import java.awt.Desktop
import java.net.URI
import scala.util.Random
object Main {
  // Should be put into .env or something else
  private val apiKey = "84e59ba686b84614b261753d7bddca70"

  def getMenuOption(): Int = {
    println("Welcome to the REPS. What would you like to do?:")
    println("1. Interact with an energy plant")
    println("2. Check on a sensor's or video's data")
    println("3. Modify something in the system")
    println("4. Get notifications")
    println("0. Exit")
    print("Enter your choice: ")
    try {
      scala.io.StdIn.readInt()
    } catch {
      case e: NumberFormatException =>
        println("Please enter a valid integer.")
        getMenuOption()
    }
  }

  def energyPlantOperations(plantID:Int,plantName:String): Unit = {
    println("What would you like to do?")
    println("1. Get basic info of the plant")
    println("2. Get calculations for the specified time period")
    println("3. Put data into a file for the specified time period")
    println("0. Back")
    print("Enter your choice: ")
    try{
      val choice = scala.io.StdIn.readInt()
      choice match {
        case 1 =>
          printBasicInfo(plantID,plantName)
        case 2 =>
          getPlantDataFromTimePeriod(plantID,plantName) match {
            case Left(error) => println(error)
            case Right(list) =>
              //put the value into a list from the json
              val data = list.map(x => (x \ "value").as[Double])
              printCalculations(data)
          }
        case 3 =>
          getPlantDataFromTimePeriod(plantID,plantName) match {
            case Left(error) => println(error)
            case Right(list) =>
              println("Enter the file name:")
              val fileName = scala.io.StdIn.readLine().strip()
              //put all the values into a csv file from the json. there is start time, end time, and values for each json object
              val data = list.map(x => ((x \ "startTime").as[String],(x \ "endTime").as[String],(x \ "value").as[Double]))
              val file = new java.io.PrintWriter(fileName)
              file.write("Start_time,End_time,Value\n")
              data.foreach(x => file.write(s"${x._1},${x._2},${x._3}\n"))
              file.close()
              println(s"Data has been written to $fileName")
          }
        case 0 => energyPlantInfoMenu()
        case _ => println("Invalid choice, choose again")
      }
    } catch {
      case e: NumberFormatException =>
        println("Please enter a valid integer.")
        energyPlantOperations(plantID,plantName)

    }
  }

  def sensorDataMenu(): Unit = {
    println("Which data would you like to check?")
    println("1. Health of the production system (sensor data)")
    println("2. Surveillance videos")
    println("0. Back")
    print("Enter your choice: ")
    try {
      val choice = scala.io.StdIn.readInt()
      choice match {
        case 1 =>
          println("Fetching sensor data related to the health of the production system...")
          fetchSensorData()

        case 2 =>
          println("Fetching surveillance videos...")
          val surveillanceVideoLink = "https://www.youtube.com/watch?v=RaRc0oHBk5M"
          Desktop.getDesktop.browse(new URI(surveillanceVideoLink))

        case 0 => runMenuOption(getMenuOption())
        case _ => println("Invalid choice, choose again")
      }
    } catch {
      case e: NumberFormatException =>
        println("Please enter a valid integer.")
        sensorDataMenu()
    }
  }

  def energyPlantInfoMenu(): Unit = {
    println("Which energy plant would you like to interact with?")
    println("1. Solar")
    println("2. Wind")
    println("3. Hydro")
    println("4. Nuclear")
    println("0. Back")
    print("Enter your choice: ")
    try {
      val plantChoice = scala.io.StdIn.readInt()
      plantChoice match {
        case 1 =>
          energyPlantOperations(186,"solar")
        case 2 =>
          energyPlantOperations(181,"wind")
        case 3 =>
          energyPlantOperations(191,"hydro")
        case 4 =>
          energyPlantOperations(182,"nuclear")
        case 0 => runMenuOption(getMenuOption())
        case _ => println("Invalid choice, choose again")
      }
    } catch {
      case e: NumberFormatException =>
        println("Please enter a valid integer.")
        energyPlantInfoMenu()
    }
  }

  def printBasicInfo(dataID: Int, plantName: String): Unit = {
    makeAPIRequest(s"$dataID") match {
      case Left(error) => println(error)
      case Right(json) =>
        val status = (json \ "status").as[String]
        val organization = (json \ "organization").as[String]
        val nameEn = (json \ "nameEn").as[String]
        val descriptionEn = (json \ "descriptionEn").as[String]
        val unitEn = (json \ "unitEn").as[String]
        println(s"$plantName energy plant info:")
        println(s"Status: $status")
        println(s"Organization: $organization")
        println(s"Name: $nameEn")
        println(s"Description: $descriptionEn")
        println(s"Unit: $unitEn")
    }
  }

  def printCalculations(data: List[Double]){
    makeCalculations(data) match {
      case Left(error) => println(error)
      case Right(result) =>
        println("Calculations for the specified time period:")
        println("Total measurements: " + data.length)
        println(s"Total power generated: ${"%.2f".format((result \ "total").as[Double])}")
        println(s"Average: ${"%.2f".format((result \ "average").as[Double])}")
        println(s"Median: ${"%.2f".format((result \ "median").as[Double])}")
        println(s"Mode: ${"%.2f".format((result \ "mode").as[Double])}")
        println(s"Range: ${"%.2f".format((result \ "range").as[Double])}")
        println(s"Midrange: ${"%.2f".format((result \ "midrange").as[Double])}")
    }
  }


  // Case class to represent each modification option, struct for scala
  case class SystemModifier(
                             option: Int,
                             description: String,
                             action: () => Unit
                           )

  // Function to handle system modifications
  def modifySystem(): Unit = {
    val options = List(
      SystemModifier(1, "Optimize solar panel orientation", optimizeSolarPanelOrientation),
      SystemModifier(2, "Optimize wind turbine orientation", optimizeWindTurbineOrientation),
      SystemModifier(3, "Suggest changes in production setup", suggestProductionSetupChanges)
    )

    println("What would you like to modify?")
    options.foreach(option => println(s"${option.option}. ${option.description}"))
    println("0. Back")

    print("Enter your choice: ")
    try {
      val choice = scala.io.StdIn.readInt()
      options.find(_.option == choice) match {
        case Some(selectedOption) => selectedOption.action()
        case None if choice == 0 => runMenuOption(getMenuOption())
        case _ => println("Invalid choice, choose again")
      }
    } catch {
      case _: NumberFormatException =>
        println("Please enter a valid integer.")
        modifySystem()
      case _: Exception =>
        println("An error occurred.")
    }
  }

  def optimizeWindTurbineOrientation(): Unit = {
    println("Optimizing wind turbine orientation...")
    val optimalAngleInitial = 45 + Random.nextDouble() * (90 - 45)
    val optimalAngle = BigDecimal(optimalAngleInitial).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
    println(s"The optimal angle for Wind Turbines Today is $optimalAngle degrees, according to the power of the wind.")
    println("Adjusting the angle of the solar panels...")
  }

  def suggestProductionSetupChanges(): Unit = {
    println("Suggesting changes in production setup...")
    val historicalEnergyData = List(500, 480, 460, 440, 420, 400, 380, 360, 340, 300)
    val agingEquipmentDetected = detectAgingEquipment(historicalEnergyData)
    if (agingEquipmentDetected) {
      println("Based on the analysis of historical data, aging equipment is contributing to decreased energy production.")
      val recommendations = generateRecommendations()
      println("Recommendations for production setup changes:")
      recommendations.foreach(println)
    } else {
      println("No significant correlation between aging equipment and energy production decline detected.")
    }
  }

  def detectAgingEquipment(data: List[Int]): Boolean = {
    val averageBefore = data.take(data.size / 2).sum / (data.size / 2)
    val averageAfter = data.drop(data.size / 2).sum / (data.size / 2)
    val declinePercentage = (averageBefore - averageAfter).toDouble / averageBefore * 100
    declinePercentage > 10
  }

  def generateRecommendations(): List[String] = {
    val recommendations = List("Replace aging turbines with newer models.", "Upgrade solar panel components.", "Optimize maintenance schedule.")
    Random.shuffle(recommendations).take(2)
  }
  def optimizeSolarPanelOrientation(): Unit = {
    println("Optimizing solar panel orientation...")
    val optimalAngleInitial = 45 + Random.nextDouble() * (50 - 45)
    val optimalAngle = BigDecimal(optimalAngleInitial).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
    println(s"The optimal angle for solar panels Today is $optimalAngle degrees, according to the movement of the Sun.")
    println("Adjusting the angle of the solar panels...")
  }


  def runMenuOption(choice:Int){
    choice match{
      case 1 => energyPlantInfoMenu()
      case 2 => sensorDataMenu()
      case 3 => modifySystem()
      case 4 => getNotifications()
      case 0 => println("Exiting...")
      sys.exit(0)
      case _ =>
        println("Invalid choice, choose again")
        runMenuOption(getMenuOption())
    }
    println()
    runMenuOption(getMenuOption())
  }

  // We will use this makeAPIRequest to work for calculations, and a separate call for the health status
  // Function to make an API request urlEnd: {datasetID}/data?start_time=2021-01-01T00:00:00Z&end_time=2021-01-02T00:00:00Z
  def makeAPIRequest(urlEnd: String): Either[String, JsValue] = {
    val urlBase: String = "https://data.fingrid.fi/api/datasets/"
    try {
      val url = urlBase + urlEnd
      val response = Http(url)
        .header("x-api-key", apiKey)
        .option(HttpOptions.readTimeout(10000))
        .asString

      if (response.is2xx) {
        val json: JsValue = Json.parse(response.body)
        Right(json)
      } else {
        Left(s"Failed to fetch API: ${response.code}")
      }
    }
    catch {
      case e: Exception => Left(s"Exception during API request: ${e.getMessage}")
    }
  }

  def makeAPIRequestHealth(urlEnd: String): Either[String, JsValue] = {
    val urlBase: String = "https://data.fingrid.fi/api/"
    try {
      val url = urlBase + urlEnd
      val response = Http(url)
        .header("x-api-key", apiKey)
        .option(HttpOptions.readTimeout(10000))
        .asString

      if (response.is2xx) {
        val json: JsValue = Json.parse(response.body)
        Right(json)
      } else {
        Left(s"Failed to fetch API: ${response.code}")
      }
    }
    catch {
      case e: Exception => Left(s"Exception during API request: ${e.getMessage}")
    }
  }
  def fetchSensorData(): Unit = {
    val urlEnd = "health"
    makeAPIRequestHealth(urlEnd) match {
      case Left(error) => println(error)
      case Right(sensorData) =>
        println("Sensor data:")
        println(sensorData)
    }
  }

  def makeAPIRequestNotifications(urlEnd: String): Either[String, List[JsValue]] = {
    val urlBase: String = "https://data.fingrid.fi/api/notifications/"
    try {
      val url = urlBase + urlEnd
      val response = Http(url)
        .header("x-api-key", apiKey)
        .option(HttpOptions.readTimeout(10000))
        .asString

      if (response.is2xx) {
        val json = Json.parse(response.body)
        Right(json.as[List[JsValue]])
      } else {
        Left(s"Failed to fetch API: ${response.code}")
      }
    } catch {
      case e: Exception => Left(s"Exception during API request: ${e.getMessage}")
    }
  }

  def getNotifications(): Unit = {
    val urlEnd = "active"
    makeAPIRequestNotifications(urlEnd) match {
      case Right(notifications) =>
        println("Notifications:")
        notifications.foreach(println)
      case Left(error) =>
        println(s"Error: $error")
    }
  }

  def getOperatingTime(dataID: Int, apiRequest: String => Either[String, JsValue]): Either[String, (String, String)] = {
    apiRequest(s"$dataID/data") match {
      case Left(error) => Left(error)
      case Right(json) =>
        val lastPageNumber = (json \ "pagination" \ "lastPage").as[Int]
        apiRequest(s"$dataID/data?page=$lastPageNumber") match {
          case Left(error) => Left(error)
          case Right(firstResponse) =>
            val startTime = ((firstResponse \ "data").last \ "startTime").as[String]
            apiRequest(s"$dataID/data?page=1") match {
              case Left(error) => Left(error)
              case Right(lastResponse) =>
                val endTime = ((lastResponse \ "data")(0) \ "endTime").as[String]
                Right(startTime, endTime)
            }
        }
    }
  }


  def makeCalculations(data: List[Double]): Either[String, JsValue] = {
    if (data.isEmpty) {
      Left("No data to calculate")
    } else {
      val total = data.sum
      val average = total / data.length
      val sortedData = data.sorted
      val median = if (data.length % 2 == 0) {
        val mid = data.length / 2
        (sortedData(mid - 1) + sortedData(mid)) / 2
      } else {
        sortedData(data.length / 2)
      }
      val mode = sortedData.groupBy(identity).maxBy(_._2.size)._1
      val range = sortedData.max - sortedData.min
      val midrange = (sortedData.max + sortedData.min) / 2
      val result = Json.obj(
        "total" -> total,
        "average" -> average,
        "median" -> median,
        "mode" -> mode,
        "range" -> range,
        "midrange" -> midrange
      )
      Right(result)
    }
  }

  def goThroughPages(baseUrlEnd: String, apiRequest: String => Either[String, JsValue],list:List[JsValue],pageNum: Int=1): List[JsValue]={
    apiRequest(s"$baseUrlEnd&page=$pageNum") match {
      case Left(error) => println(error)
        list
      case Right(json) =>
        val measurements = (json \ "data").as[List[JsObject]]
        if (measurements.isEmpty) {
          list
        } else {
          goThroughPages(baseUrlEnd, apiRequest, list ++ measurements, pageNum + 1)
        }
    }
  }
  def getPlantDataFromTimePeriod(dataID: Int, plantName: String): Either[String,List[JsValue]] = {
    println(s"$plantName energy plant info:")
    getOperatingTime(dataID, makeAPIRequest) match {
      case Left(error) => Left(error)
      case Right((start, finish)) =>
        println(s"This $plantName plant has measurements from $start to $finish")
        println("Enter the start date in the same format as the measurements:")
        val startDate: String = scala.io.StdIn.readLine().strip()
        println("Enter the end date in the same format as the measurements:")
        val endDate: String = scala.io.StdIn.readLine().strip()
        val urlEnd = s"$dataID/data?startTime=$startDate&endTime=$endDate&pageSize=20000"
        Right(goThroughPages(urlEnd, makeAPIRequest, List()))
    }
  }


  def main(args: Array[String]): Unit = {
    runMenuOption(getMenuOption())
  }
}