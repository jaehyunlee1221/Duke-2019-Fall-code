#Exercise
get_ols_estimate <- function(X, Y) {
  coeff <- solve(t(X) %*% X) %*% t(X) %*% Y
  return(coeff)
}


#Exercise
source("fib_seq.R")

test_results <- test_dir("tests/")
test_results
