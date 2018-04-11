board <- c()

initializeBoard <- function() {
  #readBadMoves()
  board <<- c(" ", " ", " ", " ", " ", " ", " ", " ", " ")
}

printBoard <- function() {
  cat(sprintf("\n%s | %s | %s\n", toString(board[[1]]), toString(board[[2]]), toString(board[[3]])))
  cat("---------")
  cat(sprintf("\n%s | %s | %s\n", toString(board[[4]]), toString(board[[5]]), toString(board[[6]])))
  cat("---------")
  cat(sprintf("\n%s | %s | %s\n", toString(board[[7]]), toString(board[[8]]), toString(board[[9]])))
}

isBoardFull <- function() {
  isFull <- TRUE
  for (i in 1:9) {
    if (toString(board[[i]]) == ' ') {
      isFull = FALSE
    }
  }
  return(isFull)
}

checkWinner <- function() {
  #check rows
  if ((toString(board[[1]]) != ' ') & (toString(board[[1]]) == toString(board[[2]])) & (toString(board[[2]]) == toString(board[[3]]))) {
    return(TRUE)
  }
  if ((toString(board[[4]]) != ' ') & (toString(board[[4]]) == toString(board[[5]])) & (toString(board[[5]]) == toString(board[[6]]))) {
    return(TRUE)
  }
  if ((toString(board[[7]]) != ' ') & (toString(board[[7]]) == toString(board[[8]])) & (toString(board[[8]]) == toString(board[[9]]))) {
    return(TRUE)
  }
  
  #check columns
  if ((toString(board[[1]]) != ' ') & (toString(board[[1]]) == toString(board[[4]])) & (toString(board[[4]]) == toString(board[[7]]))) {
    return(TRUE)
  }
  if ((toString(board[[2]]) != ' ') & (toString(board[[2]]) == toString(board[[5]])) & (toString(board[[5]]) == toString(board[[8]]))) {
    return(TRUE)
  }
  if ((toString(board[[3]]) != ' ') & (toString(board[[3]]) == toString(board[[6]])) & (toString(board[[6]]) == toString(board[[9]]))) {
    return(TRUE)
  }
  
  #check diagonal(\)
  if ((toString(board[[1]]) != ' ') & (toString(board[[1]]) == toString(board[[5]])) & (toString(board[[5]]) == toString(board[[9]]))) {
    return(TRUE)
  }
  #check diagonal(/)
  if ((toString(board[[3]]) != ' ') & (toString(board[[3]]) == toString(board[[5]])) & (toString(board[[5]]) == toString(board[[7]]))) {
    return(TRUE)
  }
  return(FALSE)
  
}

playerMove <- function() {
  while (TRUE){
    tryCatch({
      response <- readline(prompt = "Enter a move (1-9): ")
      input <- as.integer(response)
      
      if ((input >= 0) & (input < 10)) {
        if (board[[input]] == ' ') {
          board[[input]] <<- "O"
          return()
        } else {
          cat(sprintf("\nThis position is not empty!!!\n"))
        }
      }else {
        cat(sprintf("\nEnter a legal move (1-9)!!!\n"))
      }
    }, error = function(e) {
      cat(sprintf("Enter a legal move (1-9)!!!\n"))
    })
    
  }
}

computerMove <- function() {
  while (TRUE) {
    #computerPosition = checkBadMoves()
    #if (computerPosition < 0) {
    computerPosition <- sample(1:9, 1)
    if ((computerPosition >= 0) & (computerPosition < 10)) {
      if (board[[computerPosition]] == ' ') {
        board[[computerPosition]] <<- "X"
        return()
      } 
    }
    #} else {
    #board <<- append(board, "X", (computerPosition-1))
    #return
    #}
  }
}

checkBadMoves <- function() {
  
}

play <- function() {
  initializeBoard()
  while(TRUE) {
    computerMove()
    printBoard()
    if (checkWinner() == TRUE) {
      cat(sprintf("\nComputer is a winner!\n"))
      return()
    } else if (isBoardFull() == TRUE) {
      cat(sprintf("\nIt is a draw!\n"))
      return()
    }
    
    playerMove()
    printBoard()
    if (checkWinner() == TRUE) {
      saveBadMoves()
      cat(sprintf("\nPlayer is a winner!"))
      return()
    } else if (isBoardFull() == TRUE) {
      cat(sprintf("\nThis game ends in a tie!"))
      return()
    }
  }
}

saveBadMoves <- function() {
  boardCopy <- board
  stringBad <- paste(boardCopy, collapse = ",")
  setwd("~/R-Programs/program5/") #set project folder path
  write(stringBad, file = "badMoves.txt", append=TRUE)
}

readBadMoves <- function() {
  fileName <- "badMoves.txt"
  tryCatch({
    
    #fill in
    
  },error = function(e) {
    cat("")
  })
}

main <- function() {
  keepPlaying <- TRUE
  while(keepPlaying == TRUE){
    play()
    cat(sprintf("\nWould you like to play again? (y/n): "))
    response <- readline()
    if((toString(response) == "n")|(toString(response) == "N")){
      keepPlaying <- FALSE
    }
  }
  
}

main()
