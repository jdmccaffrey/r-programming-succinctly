# randoms.R
# R 3.2.4

LehmerRng = setRefClass(
  "LehmerRng",

  fields = list(
    seed = "integer"
  ),

  methods = list(
    initialize = function(seed) {  # special
      .self$seed <- seed
      for (i in 1:100) {  # burn a few
        dummy <- .self$Next()
      }
    },

    Next = function() {  # not next!
      a <- as.integer(16807)
      m <- as.integer(2147483647)
      q <- as.integer(127773)
      r <- as.integer(2836)
      hi <- seed %/% q
      lo <- seed %% q
      seed <<- (a * lo) - (r * hi)
      if (seed <= 0) {
        seed <<- seed + m
      }
      return(seed / m)
    }
  )
)

# -----

cat("\nBegin custom RNG demo \n\n")

cat("Instantiating custom RNG object \n\n")
my_rng <- LehmerRng$new(as.integer(1))

cat("Generating 8 (pseudo) random numbers \n")
for (i in 1:8) {
  x <- my_rng$Next()
  xx <- formatC(x, digits=4, format="f")
  cat(xx, " ")
}
cat("\n\n")

cat("Generating 20 random integers in [1,6] \n")
lo <- as.integer(1)
hi <- as.integer(7)
for (i in 1:20) {
  n <- floor((hi - lo) * my_rng$Next() + lo)
  cat(n, " ")
}
cat("\n\n")

cat("End custom RNG demo \n")
