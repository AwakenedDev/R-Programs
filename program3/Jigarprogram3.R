Commands <- setRefClass("Commands", methods = list(
  myList <<- c(),
  myString <<- "",
  number1 <<- 0,
  number2 <<- 0,
  number <<- 0,
  
  show <- function() {
    print(myList)
  },
  
  add <- function(string) {
    myString <<- string
    myList <<- append(myList, myString)
  },
  
  move <- function(num1, num2) {
    number1 <<- num1
    number2 <<- num2
    if (((0 < number1) & (number1 <= length(myList))) & ((0 < number2) & (number2 <= length(myList)))) {
      stringTemp <- myList[(number1)]
      myList <<- myList[-(number1)]
      myList <<- append(myList, stringTemp, (number2-1))
    }else {
      cat("Error: Move Unsuccessful")
    }
  },
  
  complete <- function(num) {
    number <<- num
    if ((0 < number) && (number <= length(myList))) {
      myList <<- myList[-(number)]
    }
    else {
      cat(sprintf("Error: %i is not available. Complete Unsuccessful", number))
    }
  }
  
))

main <- function() {
  cat("\n* * * To Do List * * *\n")
  response <- readline(prompt = "Enter a command(Show, Add, Move, Complete) or End: ")
  response <- strsplit(response, " ")
  x <- toString(response[[1]][1])
  
  while (a != "End") {
    tryCatch ({
      if (a == "Show") {
        listCopy = show()
        for (item in 1:length(listCopy)) {
          cat(sprintf("%i = %0.5s\n", item, myList2[[item]]))
        }
      }
      else if (a == "Add") {
        temporaryString = response.mkString(" ")
        responseTemp = temporaryString.split(" ", 2)
        string = responseTemp(1)
        myToDoList.add(string)
      }
      else if (a == "Move") {
        num1 = response(1).toInt
        num2 = response(2).toInt
        myToDoList.move(num1, num2)
      }
      else if (a == "Complete") {
        num = response(1).toInt
        myToDoList.complete(num)
      }
      else {
        println(a + " is an unrecognized command")
      }
    })
  }
  
  
}

main()

