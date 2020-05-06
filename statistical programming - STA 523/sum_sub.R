get_sum_sub <- function(x, k, max = TRUE) {
  x <- sort(x)
  x_len <- length(x)
  if(max) {
    result <- sum(x[((x_len - k) + 1):x_len])
  } else {
    result <- sum(x[1:k])
  }
  return(result)
}