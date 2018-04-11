# @author Jigar D. Prajapati
# In this main method we prompt user for commands that does a specific task
main <- function() {
  myList = list()
  cat("\n* * *Floating-point program started * * *\n")
  response <- readline(prompt = "Enter a command (End to quit): ")
  response <- strsplit(response, " ")
  x <- toString(response[[1]][1])
  
  #while the user doesnt type "End", this program will keep prompting the user for a command
  while (x != "End") {
    #condition check for "Insert"
    if (x == "Insert") {
      tryCatch ({
        y = as.double(response[[1]][2])
        myList <- append(myList, y)
        myList2 <- myList[order(sapply(myList, '[[', 1))]
        cat("\nThe array currently contains:\n")
        for (item in 1:length(myList2)) {
          cat(sprintf("Value[%i] = %0.5f\n", (item-1), myList2[[item]]))
        }
      }, #detects warning
      warning = function(w) {
        cat("Error: Wrong input format")
      })
      #condition check for "Delete"
    }else if (x == "Delete") {
      tryCatch ({
        y = as.double(response[[1]][2])
        myList <- myList[myList != y]
        myList2 <- myList[order(sapply(myList, '[[', 1))] #sort
        cat("\nThe array currently contains:\n")
        for (item in 1:length(myList2)) {
          cat(sprintf("Value[%i] = %0.5f\n", (item-1), myList2[[item]]))
        }
      }, #detects warning
      warning = function(w) {
        cat("Error: Wrong input format")
      }, error = function(e) {
        cat("Error: Empty Array")
      }
      )
      #condition check for "Sum"
    }else if (x == "Sum") {
      tryCatch ({
        a = 0.0
        for (item in 1:length(myList)) {
          a <- a + myList[[item]]
        }
        cat(sprintf("\nThe total is %0.1f\n", a))
      }, error = function(e) {
        cat("Error: Empty Array")
      })
    } else {
      cat(
        "\nError: Invalid command entered (Try: Insert <number>, Delete <number>, Sum or End to quit"
      )
    }
    
    response <- readline(prompt = "Enter a command (End to quit): ")
    response <- strsplit(response, " ")
    x <- toString(response[[1]][1])
  }
}

main() #call main when program runs
