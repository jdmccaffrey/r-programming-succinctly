# binsearching.R
# R 3.2.4

bin_search = function(v, t, eps) {
  # search sorted vector v for target value t
  # eps is epsilon tolerance for equality
  lo <- 1
  hi <- length(v)
  while (lo <= hi) {
    mid <- as.integer(round((lo + hi) / 2)) # always even!
    cat("lo, mid, hi = ", lo, mid, hi, "\n")
    
    if (abs(v[mid] - t) <= eps) {
      return(mid)
    }
    else if (v[mid] < t) { # C format OK in a function
      lo <- mid + 1
    }
    else {
      hi <- mid - 1
    }
  }
  return(0)
}

cat("\nBegin binary search demo \n\n")

vec <- c(1.5, 3.5, 5.5, 7.5, 9.5, 11.5, 13.5, 15.5, 17.5, 19.5)
target <- 17.5
epsilon <- 1.0e-5

cat("Vector is: \n")
print(vec)
cat("Target is ", target, "\n")
cat("Epsilon is ", epsilon, "\n\n")

cat("Begin search \n\n")
idx <- bin_search(vec, target, epsilon)
if (idx == 0) {
  cat("\nTarget not found \n\n")
} else {
  cat("\nTarget found at cell index ", idx, "\n\n")
}

cat("End demo \n")
