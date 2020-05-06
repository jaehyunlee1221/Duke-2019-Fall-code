#Exercise for 1st week
#making fibonacci function
#Question : how to remove call of stop function

seq_fib <- function(n) {
  if(n<1) {
    stop("n >= 1 is not TRUE")     #positive integer requirement
  } 
  if(length(n) != 1){
    stop("length(n) ==1 is not true")   #single component requirement
  }
  result <- 0
  if(n==1) {
    return(result)
  } else {
  result[2] <- 1
  if(n==2){
    return(result)
  } 
  for(i in 3:n){
      result[i] <- result[i-1] + result[i-2] 
  }
  return(result)
  }
}
seq_fib(n=1)
seq_fib(n=2)
seq_fib(n=7)
seq_fib(n=10)
seq_fib(n=-2)
seq_fib(n=c(2,3))
