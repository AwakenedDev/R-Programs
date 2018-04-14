# This method reads a postive integer and then prints the nth Fibonacci number.
# param n:
# return value of the nth fibonacci number:
fib <- function(n) {
  n <- as.integer(n)
  if (n>1) {
    return(fib(n-1)+fib(n-2))
  }
  return(as.integer(n))
}

# This method reads a postive integer and then prints the nth Fibonacci number.
main <-function(){
  cat("\n* * * Fibonacci Printer * * *\n") #print can also be used
  number <- readline(prompt="Which Fibonacci number would you like to see: ") #here we ask for the user input
  
  #tryCatch checks whether input is present and valid
  tryCatch(
    {
      number <- as.integer(number) #converts to int
      
      #condition check for positive numbers
      if ((number>0)&(number<46)) {
        start <- as.numeric(Sys.time()) #calculate the start time
        fibNum <- fib(number)
        finalTime <- as.numeric(Sys.time()) - start #calculate the end time
        cat(sprintf("\nFibonacci number %i is: %i\n", number, fibNum)) #print the fibonacci number the user wanted
        cat(sprintf("\nThis calculation required %f seconds\n", finalTime)) #print the total time
      }else {
        cat("Error: Input is out of range")
      }
    }, 
    #detects warning
    warning = function(w) {
      cat("Error: Wrong input format")
    }, 
    #detects error
    error = function(e) {
      cat("Error: No input")
    }
  ) 
}

main() #call main when program runs
