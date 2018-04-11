# @author Jigar D. Prajapati
# This class contains the to do list (myList) and provides methods that modify the list as required
Commands <- setRefClass("Commands",
fields = list(
  # declaration of variables
  myList = "list",
  myString = "character",
  number1 = "numeric",
  number2 = "numeric",
  number = "numeric"
),
    
 methods = list(
  
  #this method displays the values in the list
  # @return the list
  show = function() {
    return(myList)
  },
  
  #in this method we add an item(i.e string, number e.t.c) to the end of the list)
  #@param string to be added to the list
  add = function(string) {
    myString <<- string
    myList <<- append(myList, myString)
  },
  
  #moves item num1 in the list to position num2
  # @param number1
  # @param number2
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
  
  #in this method we mark a specified item in the list
  # @param number
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

#In main, we prompt the user for a command and then read the response and call the Commands' class
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
            cat(sprintf("%i. %s\n", item, listCopy[[item]]))
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
  
  cat("\n* * * To Do List Ended * * *\n")
}

main() #call main when program runs

