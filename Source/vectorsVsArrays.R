# vectorsVsArrays.R
# R 3.2.4

cat("\nBegin vectors vs. arrays demo \n\n")

cat("Creating three demo vectors \n\n")
v <- c(1:3)  # [1 2 3]
cat(v, "\n\n")

v <- vector(mode="numeric", 4)  # [0.0 0.0 0.0 0.0]
cat(v, "\n\n")

v <- c("a", "b", "x")
cat(v, "\n\n")

cat("Creating two demo lists \n\n")
ls <- list("a", 2.2)
ls[3] <- as.integer(3)
print(ls)
cat("\n")
cat("Cell [2] is: ", ls[[2]], "\n\n")

ls <- list(lname="Smith", age=22)
cat("Cells by cell names are: ", ls$lname, "-", ls$age)
cat("\n\n")

cat("Creating a 2x3 matrix \n\n")
m <- matrix(0.0, nrow=2, ncol=3)
print(m)
cat("\n")

cat("Creating 1 and 2-dim arrays \n\n")
arr <- array(0.0, 3)  # [0.0 0.0 0.0]
print(arr)
cat("\n")

arr <- array(0.0, c(2,3))  # 2x3 matrix
print(arr)
cat("\n")

# arr = array(0.0, c(2,5,4)) # 2x5x4 n-array
# print(arr)  # 40 values displayed
# cat("\n")

cat("Creating a data frame \n\n")
people <- c("Alex", "Barb", "Carl")
ages <- c(19, 29, 39)
df <- data.frame(people, ages)
names(df) <- c("NAME", "AGE")
print(df)

cat("\nEnd vectors vs. arrays demo \n\n")
