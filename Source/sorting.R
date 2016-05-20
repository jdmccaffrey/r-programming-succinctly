# sorting.R
# R 3.2.4

bubb_sort = function(v) {
  # -----
  exchange = function(ii, jj) {
    tmp <<- v[ii]
    v[ii] <<- v[jj]
    v[jj] <<- tmp
  }
  # -----
  n <- length(v)
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      if (v[i] > v[j]) {
        exchange(i, j)
        # tmp = v[i]; v[i] = v[j]; v[j] = tmp
      }
    }
  }
  return(v)
}

my_print = function(v, h, t) {
  n <- length(v)
  cat("[1]    ")
  cat(head(v, h), "\n")
  cat(" . . . ", "\n")
  idx <- n-t+1
  cat("[", idx, "]  ", sep="")
  cat(tail(v, t), "\n\n")
}

cat("\nBegin sorting demo \n\n")

cat("Generating 5,000 random values \n")
set.seed(0)
vec <- rnorm(5000)
cat("Vector to sort is : \n")
my_print(vec, 4, 4)
cat("\n")

cat("Sorting vector using built-in shell sort() \n")
start_t <- proc.time()
sorted <- sort(vec, method="shell")
end_t <- proc.time()
times <- end_t - start_t
cat("Sorted vector is \n")
my_print(sorted, 4, 4)
e_time <- formatC(times[3], digits=2, format="f")
cat("Elapsed time =", e_time, "sec. \n") 
cat("\n")

cat("Sorting with user-defined bubb_sort() \n")
start_t <- proc.time()
sorted <- bubb_sort(vec)
end_t <- proc.time()
times <- end_t - start_t

cat("Sorted vector is \n")
my_print(sorted, 4, 4)
cat("Elapsed time =", times[3], "sec.\n") 
cat("\n")

cat("Verifying result is sorted using is.sorted() \n")
unsorted <- is.unsorted(sorted)
if (unsorted == T) {
  cat("Error. Result not sorted \n\n")
} else {
  cat("Result verified sorted \n\n")
}

cat("End demo \n")
