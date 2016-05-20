# combelements.R
# R 3.2.4

comb_init = function(n, k) {
  data <- c(1:(k+1))
  data[k+1] <- n
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
}

comb_elem = function(n, k, m) {
  # mth element, combinadic

  m <- m - 1  # zero-based m
  maxM <- choose(n, k) - 1  # largest z-index
  ans <- c(1:(k+1))  # extra cell for [0]

  a <- n  # look for a v less than this
  b <- k
  x <- maxM - m  # x is the dual of m

  for (i in 1:k) {
    v <- a - 1
    while (choose(v, b) > x) {
      v <- v - 1
    }

    ans[i] <- v
    x <- x - choose(v, b)
    a <- v
    b <- b - 1
  }

  for (i in 1:k) {
    ans[i] <- n - ans[i]  # (+1) to [1]-based
  }
  ans[k+1] <- n  # recall n goes in last cell

  return(ans)
}

# -----

cat("\nBegin combination element demo \n\n")

n <- as.integer(40)
k <- as.integer(8)
cat("Setting n, k = ", n, k, "\n\n")

cat("Calculating comb[9999999] using comb_elem() \n")
m <- as.integer(9999999)

cmb <- comb_elem(n, k, m)
comb_display(cmb)
cat("\n\n")


cat("Calculating comb[9999999] using comb_succ() \n")
cmb <- comb_init(n, k)
for (i in 1:(m-1)) {
  cmb <- comb_succ(cmb)
}
comb_display(cmb)

cat("\nEnd combination element demo \n")
