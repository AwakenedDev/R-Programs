.Commands <- setRefClass("Commands", methods = list(
  
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

add("Hey")
add("f")
add("j")
show()
move(3,1)
show()
complete(2)
show()