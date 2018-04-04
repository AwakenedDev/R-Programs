Commands <- setRefClass("Commands",
fields = list(
  myList = "list",
  myString = "character",
  number1 = "numeric",
  number2 = "numeric",
  number = "numeric"
),
    
 methods = list(
  
  show = function() {
    return(myList)
  },
  
  add = function(string) {
    myString <<- string
    myList <<- append(myList, myString)
  },
  
  move = function(num1, num2) {
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
  
  complete = function(num) {
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
  toDoList = Commands$new()
  cat("\n* * * To Do List * * *\n")
  response <- readline(prompt = "Enter a command(Show, Add, Move, Complete) or End: ")
  response <- strsplit(response, " ")
  a <- toString(response[[1]][1])
  
  while (a != "End") {
    tryCatch ({
      if (a == "Show") {
        listCopy <- toDoList$show()
        tryCatch({
          for (item in 1:length(listCopy)) {
            cat(sprintf("%i = %s\n", item, listCopy[[item]]))
          }
        },error = function(e) {
          cat("Nothing in the list")
        }
        )
      }
      else if (a == "Add") {
        listString <- response[[1]][-1]
        temporaryString <- paste(listString, collapse = ' ')
        if(temporaryString != ""){
          toDoList$add(temporaryString)
        }
      }
      else if (a == "Move") {
        num1 <- as.integer(response[[1]][2])
        num2 <- as.integer(response[[1]][3])
        toDoList$move(num1, num2)
      }
      else if (a == "Complete") {
        num <- as.integer(response[[1]][2])
        toDoList$complete(num)
      }
      else {
        cat(sprintf("%s is an unrecognized command", a))
      }
    }, error = function(e) {
      cat("Error")
    })
    
    response <- readline(prompt = "Enter a command(Show, Add, Move, Complete) or End: ")
    response <- strsplit(response, " ")
    a <- toString(response[[1]][1])
  
  }
}

main()

