import scalaj.http.{Http, HttpOptions}
import play.api.libs.json.{JsError, JsObject, JsSuccess, JsValue, Json}



object Main {
  // Should be put into .env or something
  private val apiKey = "d60e8862f9c94ca5b9687c3a7cd9c5af"

  def getMenuOption(): Int = {
    println("Welcome to the REPS. What would you like to do?:")
    println("1. Get info of an energy plant")
    println("2. Check on a sensor's or video's data")
    println("3. Modify something in the system")
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

  def energyPlantInfo(): Unit = {
    println("Which energy plant would you like to know about?")
    println("1. Solar")
    println("2. Wind")
    println("3. Hydro")
    println("4. Nuclear")
    println("0. Back")
    print("Enter your choice: ")
    try {
      val choice = scala.io.StdIn.readInt()
      choice match {
        case 1 =>
          getSolarPlantInfo(248) match {
            case Left(error) => println(error)
            case Right(list) =>
              printCalculations(list)
          }
        case 2 =>
          getWindPlantInfo(181) match {
            case Left(error) => println(error)
            case Right(list) =>
              printCalculations(list)
          }
        case 3 =>
          getHydroPlantInfo(191) match {
            case Left(error) => println(error)
            case Right(list) =>
              printCalculations(list)
          }
        case 4 =>
          getNuclearPlantInfo(188) match {
            case Left(error) => println(error)
            case Right(list) =>
              printCalculations(list)
          }
        case 0 => runMenuOption(getMenuOption())
        case _ => println("Invalid choice, choose again")
      }
    } catch {
      case e: NumberFormatException =>
        println("Please enter a valid integer.")
        energyPlantInfo()
    }
  }

  def printCalculations(data: List[Double]){
    makeCalculations(data) match {
      case Left(error) => println(error)
      case Right(result) =>
        println("Calculations for the specified time period:")
        println("Total measurements: " + data.length)
        println(s"Total: ${"%.2f".format((result \ "total").as[Double])}")
        println(s"Average: ${"%.2f".format((result \ "average").as[Double])}")
        println(s"Median: ${"%.2f".format((result \ "median").as[Double])}")
        println(s"Mode: ${"%.2f".format((result \ "mode").as[Double])}")
        println(s"Range: ${"%.2f".format((result \ "range").as[Double])}")
        println(s"Midrange: ${"%.2f".format((result \ "midrange").as[Double])}")
    }
  }
  def sensorVideoData(): Unit = {
    println("Which data would you like to check?")
    println("1. Sensor data")
    println("2. Video data")
    println("0. Back")
    print("Enter your choice: ")
    try {
      val choice = scala.io.StdIn.readInt()
      choice match {
        case 1 => println("Sensor data")
        case 2 => println("Video data")
        case 0 => runMenuOption(getMenuOption())
        case _ => println("Invalid choice, choose again")
      }
    } catch {
      case e: NumberFormatException =>
        println("Please enter a valid integer.")
        sensorVideoData()
    }
  }

  def modifySystem(): Unit = {
    println("What would you like to modify?")
    println("1. Add a new task")
    println("2. List all tasks")
    println("3. Modify a task")
    println("0. Back")
    print("Enter your choice: ")
    try {
      val choice = scala.io.StdIn.readInt()
      choice match {
        case 1 => println("Add a new task")
        case 2 => println("List all tasks")
        case 3 => println("Modify a task")
        case 0 => runMenuOption(getMenuOption())
        case _ => println("Invalid choice, choose again")
      }
    } catch {
      case _: NumberFormatException =>
        println("Please enter a valid integer.")
        modifySystem()
    }
  }

  def runMenuOption(choice:Int){
    choice match{
      case 1 => energyPlantInfo()
      case 2 => sensorVideoData()
      case 3 => modifySystem()
      case 0 => println("Exiting...")
      sys.exit(0)
      case _ =>
        println("Invalid choice, choose again")
        runMenuOption(getMenuOption())
    }
    println()
    runMenuOption(getMenuOption())
  }


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


  def getNuclearPlantInfo(dataID:Int): Either[String, List[Double]] = {
    println("Nuclear energy plant info:")
    getOperatingTime(dataID, makeAPIRequest) match {
      case Left(error) => Left(error)
      case Right((start, finish)) =>
        println(s"This nuclear plant has measurements from $start to $finish")
        println("Enter the start date in the same format as the measurements:")
        val startDate: String = scala.io.StdIn.readLine().strip()
        println("Enter the end date in the same format as the measurements:")
        val endDate: String = scala.io.StdIn.readLine().strip()
        println(startDate, endDate)
        val urlEnd = s"$dataID/data?startTime=$startDate&endTime=$endDate&pageSize=20000"
        Right(goThroughPages(urlEnd, makeAPIRequest, List()))
    }
  }

  def goThroughPages(baseUrlEnd: String, apiRequest: String => Either[String, JsValue],list:List[Double],pageNum: Int=1): List[Double]={
    apiRequest(s"$baseUrlEnd&page=$pageNum") match {
      case Left(error) => println(error)
        list
      case Right(json) =>
        val measurements = (json \ "data").as[List[JsObject]]
        if (measurements.isEmpty) {
          list
        } else {
          val values = measurements.map(m => (m \ "value").as[Double])
          goThroughPages(baseUrlEnd, apiRequest, list ++ values, pageNum + 1)
        }
    }
  }

  def getHydroPlantInfo(dataID:Int): Either[String, List[Double]] = {
    println("Hydro energy plant info:")
    getOperatingTime(dataID, makeAPIRequest) match {
      case Left(error) => Left(error)
      case Right((start, finish)) =>
        println(s"This hydro plant has measurements from $start to $finish")
        println("Enter the start date in the same format as the measurements:")
        val startDate: String = scala.io.StdIn.readLine().strip()
        println("Enter the end date in the same format as the measurements:")
        val endDate: String = scala.io.StdIn.readLine().strip()
        println(startDate, endDate)
        val urlEnd = s"$dataID/data?startTime=$startDate&endTime=$endDate&pageSize=20000"
        Right(goThroughPages(urlEnd, makeAPIRequest, List()))
    }
  }

  def getWindPlantInfo(dataID:Int): Either[String, List[Double]] = {
    println("Wind energy plant info:")
    getOperatingTime(dataID, makeAPIRequest) match {
      case Left(error) => Left(error)
      case Right((start, finish)) =>
        println(s"This wind plant has measurements from $start to $finish")
        println("Enter the start date in the same format as the measurements:")
        val startDate: String = scala.io.StdIn.readLine().strip()
        println("Enter the end date in the same format as the measurements:")
        val endDate: String = scala.io.StdIn.readLine().strip()
        println(startDate, endDate)
        val urlEnd = s"$dataID/data?startTime=$startDate&endTime=$endDate&pageSize=20000"
        Right(goThroughPages(urlEnd, makeAPIRequest, List()))
    }
  }

  def getSolarPlantInfo(dataID:Int): Either[String, List[Double]] = {
    println("Solar energy plant info:")
    getOperatingTime(dataID, makeAPIRequest) match {
      case Left(error) => Left(error)
      case Right((start, finish)) =>
        println(s"This solar plant has measurements from $start to $finish")
        println("Enter the start date in the same format as the measurements:")
        val startDate: String = scala.io.StdIn.readLine().strip()
        println("Enter the end date in the same format as the measurements:")
        val endDate: String = scala.io.StdIn.readLine().strip()
        println(startDate, endDate)
        val urlEnd = s"$dataID/data?startTime=$startDate&endTime=$endDate&pageSize=20000"
        Right(goThroughPages(urlEnd, makeAPIRequest, List()))
    }
  }


  def main(args: Array[String]): Unit = {

    runMenuOption(getMenuOption())
  }
}