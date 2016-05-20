# sampling.R
# R 3.2.4

my_sample = function(N, k) {
  # select k random ints between 1 and N
  result <- c(1:k) # [1, 2, . . k]
  for (i in (k+1):N) {  # reservoir sampling algorithm
    j <- floor(runif(1, min=1.0, max=i+1)) 
    if (j <= k) {
      result[j] = i 
    }   
  }
  return(result)
}

my_shuffle = function(v) {
  # Fisher-Yates shuffle
  n = length(v)
  for (i in 1:n) {
    ri <- floor(runif(1, min=i, max=n+1))
    t <- v[i]; v[i] <- v[ri]; v[ri] <- t
  }
  return(v)
}

cat("\nBegin vector sampling demo \n\n")
set.seed(20) # arbitrary

N <- 9
k <- 4
cat("N = ", N, "\n")
cat("k = ", k, "\n\n")

cat("Sampling 4 items using program-defined my_sample() \n")
samp <- my_sample(N, k)
cat("Sample = ", samp, "\n\n")

cat("Sampling 4 items using built-in sample() \n")
samp <- sample(N, k)
cat("Sample = ", samp, "\n\n")

cat("Sampling 4 items using built-in sample(replace=TRUE) \n")
samp <- sample(N, k, replace=T)
cat("Sample = ", samp, "\n\n")

cat("Shuffling (1,2,3,4,5,6) using built-in sample() \n")
v <- c(1:6)
vv <- sample(v)
cat("Shuffled v = ", vv, "\n\n")

cat("Shuffling (1,2,3,4,5,6) using program-defined my_shuffle() \n")
v <- c(1:6)
vv <- my_shuffle(v)
cat("Shuffled v = ", vv, "\n\n")

cat("End demo\n")
