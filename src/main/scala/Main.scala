object Main {

  def getMenuOption(): Int = {
    println("What would you like to do?:")
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
    runMenuOption(getMenuOption())
  }
}