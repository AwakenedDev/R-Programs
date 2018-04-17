# @author Jigar D. Prajapati
#TicTacToe game (Computer vs. User) where the computer learns from its mistake. If in windows change this to wherever your R-Programs folders is stored (e.g C:\...Desktop\)

setwd("~/") #setting project directory

board <- c()
badMoveDF <- data.frame()
totalBadMoves <- 0

#initialize board
initializeBoard <- function() {
  readBadMoves()
  board <<- c(" ", " ", " ", " ", " ", " ", " ", " ", " ")
}

#print board
printBoard <- function() {
  cat(sprintf("\n%s | %s | %s\n", toString(board[[1]]), toString(board[[2]]), toString(board[[3]])))
  cat("---------")
  cat(sprintf("\n%s | %s | %s\n", toString(board[[4]]), toString(board[[5]]), toString(board[[6]])))
  cat("---------")
  cat(sprintf("\n%s | %s | %s\n", toString(board[[7]]), toString(board[[8]]), toString(board[[9]])))
}

#check if game is draw
# @return isFull
isBoardFull <- function() {
  isFull <- TRUE
  for (i in 1:9) {
    if (toString(board[[i]]) == ' ') {
      isFull = FALSE
    }
  }
  return(isFull)
}

#checks winner
# @return boolean
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

#player selects move
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
    }, warning = function(w) {
      cat(sprintf("Enter a legal move (1-9)!!!\n"))
    }, error = function(e) {
      cat(sprintf("Enter a legal move (1-9)!!!\n"))
    })
    
  }
}

#computer selects move
computerMove <- function() {
  while (TRUE) {
    computerPosition = checkBadMoves()
    if (computerPosition < 0) {
      computerPosition <- sample(1:9, 1)
      if ((computerPosition >= 0) & (computerPosition < 10)) {
        if (board[[computerPosition]] == ' ') {
          board[[computerPosition]] <<- "X"
          return()
        } 
      }
    } else {
      board[[computerPosition]] <<- "X"
      return()
    }
  }
}

#check bad moves
checkBadMoves <- function() {
  if (totalBadMoves == 0) {
    return(-1)
  }
  for(i in 1:totalBadMoves) {
    counterTheSameMoves <- 0
    for (j in 1:9) {
      if((toString(badMoveDF[i, j]) == toString(board[[j]])) & (toString(badMoveDF[i, j]) != ' ')) {
        counterTheSameMoves <- counterTheSameMoves + 1
      }
    }
    if (counterTheSameMoves == 4) {
      for (j in 1:9) {
        if ((toString(badMoveDF[i, j]) == 'O') & (toString(board[[j]]) == ' ')) {
          return(j)
        }
      }
    }
  }
  return(-1)
}

#play game
play <- function() {
  initializeBoard()
  while(TRUE) {
    computerMove()
    printBoard()
    if (checkWinner() == TRUE) {
      cat(sprintf("\nComputer is the winner!\n"))
      return()
    } else if (isBoardFull() == TRUE) {
      cat(sprintf("\nIt is a draw!\n"))
      return()
    }
    
    playerMove()
    printBoard()
    if (checkWinner() == TRUE) {
      saveBadMoves()
      cat(sprintf("\nPlayer is the winner!"))
      return()
    } else if (isBoardFull() == TRUE) {
      cat(sprintf("\nThis game ends in a tie!"))
      return()
    }
  }
}

#This method saves bad moves to file
saveBadMoves <- function() {
  boardCopy <- board
  stringBad <- paste(boardCopy, collapse = ",")
  write(stringBad, file = "R-Programs/program5/badMoves.txt", append=TRUE)
}

#This function reads a file of bad moves (if it is available) at the start of the program
readBadMoves <- function() {
  tryCatch({
    badMoveDF <<- read.table("R-Programs/program5/badMoves.txt", header = FALSE, sep = ",")
    names(badMoveDF) <<- c("1","2","3","4","5","6","7","8","9")
    totalBadMoves <<- nrow(badMoveDF)
    #cat(toString(badMoveDF[i, j]))
  },error = function(e) {
    cat("")
  })
}

#main method
main <- function() {
  keepPlaying <- TRUE
  while(keepPlaying == TRUE){
    tryCatch({
      play()
      cat(sprintf("\nWould you like to play again? (\"n\" to quit): "))
      response <- readline()
      if((toString(response) == "n")|(toString(response) == "N")){
        keepPlaying <- FALSE
      }
    }, warning = function(w) {
      cat("")
    }, error = function(e) {
      cat("")
    }
    )
  }
  
}

main() #call main when program runs
