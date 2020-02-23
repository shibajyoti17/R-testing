# first functiion to add two numbers
add2 <- function(x,y){
  x + y
}
#using it
add2(5,10)

#function to return a subset of a vector which has values > 10
above10 <- function(x){
  use <- x > 10
  x[use]
}
some_vector = c(1,3,9,13,17,2,3,64)
above10(some_vector)

#function to return a subset of a vector which has values > n
above <- function(x,n){
  use <- x > n
  x[use]
}
above(some_vector,3)

#function to return men of each column in a dataframe
column_mean <- function(x,removeNA = TRUE){
  nc <- ncol(x)
  means <- numeric(nc)
  for(i in 1:nc){
    means[i] <- mean(x[,i],na.rm = removeNA)
  }
  means
}
column_mean(airquality)