# persons.R
# R 3.2.4

# S4 OOP
Person = setClass(
  "Person",

  slots = list(
    lastName = "character",
    age = "integer",
    payRate = "numeric"
  ),

  prototype = list(
    lastName = "NONAME",
    age = as.integer(-1),
    payRate = 0.00
  )

  # could define validity() here
)

setGeneric(name="display",
 def=function(obj) {
  standardGeneric("display")
 }
)

setMethod(f="display",
 signature="Person",
 definition=function(obj) {
   cat("Last Name :", obj@lastName, "\n")
   cat("Age       :", obj@age, "\n")
   cat("Pay Rate  : $", obj@payRate, "\n")
 }
)

setGeneric(name="setPayRate",
 def=function(obj, pRate) {
  standardGeneric("setPayRate")
 }
)

setMethod(f="setPayRate",
 signature="Person",
 definition=function(obj, pRate) {
   obj@payRate <- pRate
   return(obj)
 }
)

setGeneric(name="getPayRate",
 def=function(obj) {
  standardGeneric("getPayRate")
 }
)

setMethod(f="getPayRate",
 signature="Person",
 definition=function(obj) {
   return(obj@payRate)
 }
)

# -----

cat("\nBegin OOP with S4 demo \n\n")

# initialization
cat("Initializing a blank Person object p1 \n") 
p1 <- new("Person")  # calls prototype()
# p1 <- Person()  # non-preferred
cat("Person 'p1' is: \n")
display(p1)
cat("\n")

cat("Initializing a Person object p2 \n") 
p2 <- new("Person", lastName="Baker", age=as.integer(22), payRate=22.22)
cat("Person 'p2' is: \n")
display(p2)
cat("\n")

# set fields
cat("Setting 'p1' directly and with a setter() \n") 
p1@lastName <- "Adams"
p1@age <- as.integer(18)
p1 <- setPayRate(p1, 11.11)
cat("Person 'p1' is now: \n")
display(p1)
cat("\n")

# get fields
cat("Getting lastName and payRate of 'p1' \n")
ln <- p1@lastName
pr <- getPayRate(p1) 
cat("Pay rate of '")
cat(ln)
cat("' is ", pr, "\n")
cat("\n")

# assignment
cat("Calling 'p3 <- p1' \n")
p3 <- p1
cat("Object 'p3' is: \n")
display(p3)
cat("\n")

cat("Modifying lastName, payRate of object 'p3' \n")
p3@lastName <- "Chang"
p3 <- setPayRate(p3, 33.33)
cat("Object 'p3' is now: \n")
display(p3)
cat("\n")

cat("Original object 'p1' is unchanged: \n")
display(p1)

cat("\nEnd demo \n")
