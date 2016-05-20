# combinations.R
# R 3.2.4

comb_init = function(n, k) {
  data <- c(1:(k+1))
  data[k+1] <- n  # store n in dummy last cell
  return(data)
}

comb_display = function(comb) {
  len <- length(comb)
  k <- len - 1
  n <- comb[len]
  
  cat("^ " )
  for (i in 1:k) {
    cat(comb[i], " ", sep="")
  }
  cat("^ |", n, "\n")
}

comb_succ = function(comb) {
  len <- length(comb)
  k <- len - 1
  n <- comb[len]

  if (comb[1] == n - k + 1) {
    return(NULL)
  }
  
  result <- comb

  i <- k
  while (i > 1 && (result[i] == (n-k+i))) {
    i <- i - 1
  }
  
  result[i] = result[i] + 1
  j <- i + 1
  while (j <= k) {
    result[j] <- result[j-1] + 1
    j <- j + 1
  }

  return(result)
} # comb_succ

# -----

cat("\nBegin combinations demo \n\n")

n <- as.integer(6)
k <- as.integer(4)
cat("Setting n, k = ", n, k, "\n\n")
nc <- choose(n, k)
cat("There are", nc, "total combinations \n\n")

cat("Displaying all combinations: \n")
cmb <- comb_init(n, k)
i <- as.integer(1)
while (is.null(cmb) == FALSE) {
  cat(formatC(i, digits=2), " ")
  comb_display(cmb)
  cmb <- comb_succ(cmb)
  i <- i + 1
}
cat("\n")

cat("End combinations demo \n")
