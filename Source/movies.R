# movies.R
# R 3.2.4

# RC (Reference Class) OOP
Movie <- setRefClass(
  "Movie",

  fields = list(
    title = "character",
    released = "character", # release date
    runTime = "integer"
  ),

  methods = list(
    display = function() {
      cat("Title        :", title, "\n")
      cat("Release date :", released, "\n")
      cat("Run time     :", runTime, "\n")
    },

    getAge = function() {
      rd = as.POSIXlt(released)
      today = as.POSIXlt(Sys.Date())

      age = today$year - rd$year
      if (today$mon < rd$mon ||
       (today$mon == rd$mon && today$mday < rd$mday)) {
        return(age - 1)
      } else {
        return(age)
      }
    },

    setReleased = function(rd) {
      released <<- rd
      # .self$released <- rd
    }
  ) # methods
)

# -----

cat("\nBegin OOP using RC demo \n\n")

# initialization
cat("Initializing a blank Movie object using $new() \n")
m1 <- Movie$new()
cat("Movie 'm1' is: \n")
m1$display()
cat("\n")

cat("Initializing a Movie object m2 \n") 
m2 <- Movie$new(title="Blade Runner",
 released="1982-06-25", runTime=as.integer(117))
cat("Movie 'm2' is: \n")
m2$display()
cat("\n")

# set fields
cat("Setting 'm1' directly and with a setter() \n") 
m1$title <- "Alien"
m1$setReleased("1979-05-25")
m1$runTime <- as.integer(117)
cat("Movie 'm1' is now: \n")
m1$display()
cat("\n")

# get fields
cat("Getting title and age of 'p1' \n")
ti <- m1$title
age <- m1$getAge() 
cat("The age of '")
cat(ti)
cat("' is ", age, "years\n")
cat("\n")

# assignment
cat("Calling m3 <- m1$copy() to make a copy \n")
m3 <- m1$copy()
cat("Object 'm3' is: \n")
m3$display()
cat("\n")

cat("Modifying all fields of object 'm3' \n")
m3$title <- "Cube"
m3$setReleased("1997-09-09")
m3$runTime <- as.integer(90)
cat("Object 'm3' is now: \n")
m3$display()
cat("\n")

cat("Original object 'm1' is unchanged: \n")
cat("Object m1 is: \n")
m1$display()
cat("\n")

cat("Calling 'm4 <- m1' (probably wrong!) \n")
m4 <- m1
cat("Object 'm4' is: \n")
m4$display()
cat("\n")

cat("Modifying all fields of object 'm4' \n")
m4$title <- "Dark City"
m4$setReleased("1998-02-27")
m4$runTime <- as.integer(100)
cat("Object 'm4' is now: \n")
m4$display()
cat("\n")

cat("BUT original object 'm1' has changed too: \n")
cat("Object m1 is: \n")
m1$display()

cat("\nEnd demo \n")
