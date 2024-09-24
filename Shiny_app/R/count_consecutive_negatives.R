count_consecutive_negatives <- function(vec, hours) {
  # Define the function to count consecutive negative values with only one allowed positive number in a sequence
  n <- length(vec)
  output <- integer(n)
  
  i <- 1
  while (i <= n) {
    if (vec[i] < 0) {
      count <- 0
      j <- i
      positive_count <- 0
      
      while (j <= n && (vec[j] < 0 || (vec[j] > 0 && positive_count < 1 && j < n && vec[j - 1] < 0 && vec[j + 1] < 0))) {
        if (vec[j] < 0 || (vec[j] > 0 && vec[j - 1] < 0 && vec[j + 1] < 0)) {
          count <- count + 1
          if (vec[j] > 0) {
            positive_count <- positive_count + 1
          }
        }
        j <- j + 1
      }
      
      # Fill the output for the sequence
      for (k in i:(j - 1)) {
        output[k] <- count
      }
      
      i <- j
    } else {
      i <- i + 1
    }
  }
  
  return(data.frame(hours, output))
}