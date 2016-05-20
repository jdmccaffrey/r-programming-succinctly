# books.R
# R 3.2.4

# LE (list encapsulation)
book = list(
  title <- NULL,
  year <- NULL,
  price <- NULL,

  init = function(lst, t, y, p) {
    lst$title <- t
    lst$year <- y
    lst$price <- p
    return(lst)  
  },

  display = function(lst) {
    cat("Title :", lst$title, "\n")
    cat("Year  :", lst$year, "\n")
    cat("Price : $", lst$price, "\n")
  },

  setPrice = function(lst, p) {
    lst$price <- p
    return(lst)
  },

  getPrice = function(lst) {
    return(lst$price)
  }
)

# -----

cat("\nBegin OOP with list encapsulation demo \n\n")

# initialization
cat("Initializing a 'book' object \n") 
book <- book$init(book, "NOTITLE", "NOYEAR", "NOPRICE")
cat("Object 'book' is: \n")
book$display(book)
cat("\n\n")

# set fields
cat("Setting 'book' directly and with a setter() \n") 
book$title <- "A Princess of Mars"
book$year <- as.integer(1912) 
book <- book$setPrice(book, 12.95)
book$display(book)
cat("\n")

# get fields
cat("Getting title and price fields \n")
ti <- book$title
pr <- book$getPrice(book) 
cat("Price of '")
cat(ti)
cat("' is ", pr, "\n")
cat("\n\n")

# assignment
cat("Calling 'tome <- book' \n")
tome <- book
cat("Object 'tome' is: \n")
tome$display(tome)
cat("\n")

cat("Modifying title of object 'tome' \n")
tome$title <- "xxxxxxxx"
cat("Object 'tome' is: \n")
tome$display(tome)

cat("\n")
cat("Original object 'book' is unchanged: \n")
book$display(book)

cat("\nEnd demo \n")
