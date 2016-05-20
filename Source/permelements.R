# permelements.R
# R 3.2.4

perm_init = function(n) {
  data <- c(1:n)
  return(data)
}

perm_display = function(perm) {
  n <- length(perm)
  cat("# " )
  for (i in 1:n) {
    cat(perm[i], " ", sep="")
  }
  cat("# \n")
}

perm_succ = function(perm) {
  n <- length(perm)
  result <- perm
  left <- n - 1
  while (result[left] > result[left+1] && left >= 2) {
    left <- left - 1
  }

  if (left == 1 && result[left] > result[left+1]) {
    return(NULL)
  }

  right <- n
  while (result[left] > result[right]) {
    right <- right - 1
  }

  tmp <- result[left]
  result[left] <- result[right]
  result[right] <- tmp

  i <- left + 1
  j <- n
  while (i < j) {
    tmp <- result[i]
    result[i] <- result[j]
    result[j] <- tmp
    i <- i + 1; j <- j - 1
  }
  return(result)
} # perm_succ

perm_elem = function(n, m) {
  # mth element of perm order n
  m <- m - 1 # make m 0-based
  result <- c(1:n)
  factoradic <- c(1:n)

  for (j in 1:n) {
    factoradic[n-j+1] <- m %%j 
    m <- m %/% j
  }

  for (i in 1:n) {
    factoradic[i] <- factoradic[i] + 1
  }

  result[n] <- 1 # last value to 1
  
  i <- n-1
  while(i >= 1) {
    result[i] <- factoradic[i]
    for (j in (i+1):n) {
      if (result[j] >= result[i]) {
        result[j] <- result[j] + 1
      }
    }
    i <- i - 1
  }

  return(result)
} # perm_element


# =====

cat("\nBegin permutation element demo \n\n")

n <- as.integer(12)
cat("Setting n = ", n, "\n\n")
cat("Generating element [9999999] using perm_element() \n")
start_t <- proc.time()
pe <- perm_elem(n, 9999999)
end_t <- proc.time()
times <- end_t - start_t
perm_display(pe)
cat("Elapsed time =", times[3], "sec.\n") 
cat("\n\n")

cat("Generating element [9999999] using perm_succ() \n")
start_t <- proc.time()
p <- perm_init(n)
m <- 9999999
for (j in 1:(m-1)) {
  p = perm_succ(p)
}
end_t <- proc.time()
times <- end_t - start_t
perm_display(p)
cat("Elapsed time =", times[3], "sec.\n") 
cat("\n")

cat("End permutations demo \n")
