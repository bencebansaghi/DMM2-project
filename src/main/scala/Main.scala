import scalaj.http.{Http, HttpOptions}
import play.api.libs.json.Json
object Main {
  def getMenuOption(): Int = {
    println("Weclome to the REPS. What would you like to do?:")
    println("1. Get general info of an energy plant")
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
    println("0. Back")
    print("Enter your choice: ")
    try {
      val choice = scala.io.StdIn.readInt()
      choice match {
        case 1 => println("Solar energy plant info")
        case 2 => println("Wind energy plant info")
        case 3 => println("Hydro energy plant info")
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
      case e: NumberFormatException =>
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

  def main(args: Array[String]): Unit = {
    val apiKey = "d60e8862f9c94ca5b9687c3a7cd9c5af"
    val url = "https://data.fingrid.fi/api/health"
    val response = Http(url)
      .header("Authorization", s"Bearer $apiKey")
      .option(HttpOptions.readTimeout(10000)) // optional read timeout in milliseconds
      .asString

    println("Status Code: " + response.code) // This will tell you the HTTP status code
    println("Response Body: " + response.body)

    if (response.is2xx) {
      val json = Json.parse(response.body)

      def printStatus(component: String): Unit = {
        (json \ component \ "status").asOpt[String].foreach { status =>
          println(s"$component status: $status")
          if (status == "ERROR") {
            (json \ component \ "message").asOpt[String].foreach { message =>
              println(s"$component error message: $message")
            }
          }
        }
      }
      // Apply the function to each component
      printStatus("app")
      printStatus("database")
      printStatus("network")
        } else {
      println(s"Failed to fetch API: ${response.code}")
        }
      }
    runMenuOption(getMenuOption())
}