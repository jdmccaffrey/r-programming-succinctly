# syntaxdemo.R                                # comments start with '#'
# R 3.2.4                                     # filename and version

tri_max = function(x, y, z) {                 # program-defined function          
  if (x > y && y > z) {                       # logical AND
    return(x)                                 # return() requires parens
  }
  else if (y > z) {                           # C-style braces OK here
    return(y)
  }
  else {
    return(z)
  }                                           
}

my_display = function(v, dec=2) {             # default argument value
  # display vector v to console
  n <- length(v)                              # built-in length() 
  for (i in 1:n) {                            # for-loop
    x <- v[i]                                 # 1-based indexing
    xf <- formatC(x,                          # built-in formatC() 
      digits=dec, format="f")                 # you can break long lines         
    cat(xf, " ")                              # basic display function
  }
  cat("\n\n")                                 # print two newlines                                 
}


my_binsearch = function(v, t) {               # program-defined function
  # search sorted integer vector v for t
  lo <- 1
  hi <- length(v)
  while (lo <= hi) {                          # while loop
    mid <- as.integer(round((lo + hi) / 2))   # built-in round()
    if (v[mid] == t) {                        # equality
      return(mid)
    } else if (v[mid] < t) {                  # R-style braces optional
      lo <- mid + 1
    } else {
      hi <- mid - 1
    }
  }
  return(0) # not found                       # could just use 0 here
}

# -----                                       # functions must be defined

cat("\nBegin R program syntax demo \n\n")

xx <- 4.4; yy <- 6.6; zz <- 2.2               # multiple values, one line
mx <- tri_max(xx, yy, zz)                     # function call
cat("Largest value is", mx, "\n\n")           # use ',' or paste()

v <- c(1:4)                                   # make vector of integers
decimals <- 3                                 # '<-' or '=' assignment
my_display(v, decimals)                       # override default 2 value

v <- vector(mode="integer", length=4)         # make vector of integers
v[1] <- 9; v[2] <- 6; v[3] <- 7; v[4] <- 8    # multiple statements
t <- 7                                        
idx <- my_binsearch(v, t)
if (idx >= 1) {                               # R-style braces required here
  cat("Target ", t, "in cell", idx, "\n\n")
} else {
  cat("Target", t, "not found \n\n")
}

cat("End syntax demo \n\n")
