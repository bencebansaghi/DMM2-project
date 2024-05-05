import scalaj.http.{Http, HttpOptions}
import play.api.libs.json.{JsError, JsObject, JsSuccess, JsValue, Json}


object Main {
  // Should be put into .env or something
  private val apiKey = "d60e8862f9c94ca5b9687c3a7cd9c5af"

  def getMenuOption(): Int = {
    println("Weclome to the REPS. What would you like to do?:")
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
        case 1 => println("Solar energy plant info")
        case 2 => println("Wind energy plant info")
        case 3 => println("Hydro energy plant info")
        case 4 => nuclearPlantInfo()
        case 0 => runMenuOption(getMenuOption())
        case _ => println("Invalid choice, choose again")
      }
    } catch {
      case e: NumberFormatException =>
        println("Please enter a valid integer.")
        energyPlantInfo()
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

  def nuclearPlantInfo(): Unit = {
    println("Nuclear energy plant info:")
    getOperatingTime(188, makeAPIRequest) match {
      case Left(error) => println(error)
      case Right((start, finish)) =>
        println(s"This has measurements from $start to $finish")
    }
  }

  def main(args: Array[String]): Unit = {

    runMenuOption(getMenuOption())
  }
}