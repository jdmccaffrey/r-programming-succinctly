# seqsearching.R
# R 3.2.4

seq_search = function(v, t, eps) {
  # search vector v for target value t
  # eps is epsilon tolerance for equality

  n <- length(v)
  for (i in 1:n) {
    if (abs(v[i] - t) <= eps) {
      return(i)
    }
  }
  return(0)
}

my_print = function(v, dec) {
  n <- length(v)
  for (i in 1:n) {
    x <- v[i]
    xx <- formatC(x, digits=dec, format="f")
    cat(xx, " ")
  }
  cat("\n")
}

cat("\nBegin sequential search demo \n\n")

vec <- c(1.0, 5.0, 2.0, 3.0, 4.0)
target <- 2.0
epsilon <- 1.0e-5

cat("Vector is: ")
my_print(vec, dec=2)
cat("\n")

cat("Target is ")
cat(formatC(target, digits=1, format="f"), "\n")
cat("Epsilon is ", epsilon, "\n")
cat("Search using program-defined seq_search() \n")
idx <- seq_search(vec, target, epsilon)
cat("idx = ", idx, "\n\n")

cat("Search using base %in% operator \n")
there <- target %in% vec
cat("there = ", there, "\n\n")

cat("Search using base which.max() \n")
idx <- which.max(vec)
cat("idx of largest = ", idx, "\n\n")

target <- c(5.0, 3.0)
cat("Target is ", target, "\n")
cat("Search using base match() \n")
matches <- match(vec, target)
cat("matches = ", matches, "\n\n")

cat("Search using base is.element() \n")
matches <- is.element(vec, target)
cat("matches = ", matches, "\n\n")

cat("End demo\n")
