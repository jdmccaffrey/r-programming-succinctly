# permutations.R
# R 3.2.4

perm_init = function(n) {
  data <- c(1:n)  # vector simple way
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
  left = n - 1
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

# =====

cat("\nBegin permutations demo \n\n")

n <- as.integer(4)
cat("Setting n =", n, "\n\n")

cat("Displaying all permutations using perm_succ(): \n")
p <- perm_init(n)
i <- as.integer(1)
while (is.null(p) == FALSE) {
  cat(formatC(i, digits=2), " ")
  perm_display(p)
  p <- perm_succ(p)
  i <- i + 1
}
cat("\n")

cat("End permutations demo \n")
