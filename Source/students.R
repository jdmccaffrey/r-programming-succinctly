# students.R
# R 3.2.4

# S3 OOP
Student = function(ln="NONAME", un=0, gpa=0.00) {
  this <- list(
    lastName = ln,
    units = un,
    gradePoint = gpa
  )
  class(this) <- append(class(this), "Student")
  return(this)
}

display = function(obj) {
  UseMethod("display", obj)
}

display.Student = function(obj) {
  cat("Last name : ", obj$lastName, "\n")
  cat("Units     : ", obj$units, "\n")
  cat("GPA       : ", obj$gradePoint, "\n")
}

setUnits = function(obj, un) {
  UseMethod("setUnits", obj)
}

setUnits.Student = function(obj, un) {
  obj$units <- un
  return(obj)
}

getUnits = function(obj) {
  UseMethod("getUnits", obj)
}

getUnits.Student = function(obj) {
  return(obj$units)
}

# -----

cat("\nBegin OOP with S3 demo \n\n")

# initialization
cat("Initializing a default Student object s1 \n") 
s1 <- Student()  # default param values
cat("Student 's1' is: \n")
display(s1)
cat("\n")

cat("Initializing a Student object s2 \n")
s2 <- Student("Barker", 27, 2.87)
cat("Student 's2' is: \n")
display(s2)
cat("\n")

# set fields
cat("Setting s1 directly and with a setter() \n") 
s1$lastName <- "Archer"
s1 <- setUnits(s1, 19)
s1$gradePoint <- 1.99
cat("Student 's1' is now: \n")
display(s1)
cat("\n")

# get fields
cat("Getting lastName and units for 's1' \n")
ln <- s1$lastName
units <- getUnits(s1) 
cat("Units for Student '")
cat(ln)
cat("' are ", units, "\n")
cat("\n")

# assignment
cat("Calling 's3 <- s1' \n")
s3 <- s1
cat("Object 's3' is: \n")
display(s3)
cat("\n")

cat("Modifying all fields of object 's3' \n")
s3$lastName <- "Coogan"
s3 <- setUnits(s3, 38)
s3$gradePoint <- 3.08
cat("Object 's3' is now: \n")
display(s3)
cat("\n")

cat("Original object 's1' is unchanged: \n")
display(s1)

cat("\nEnd demo \n")
