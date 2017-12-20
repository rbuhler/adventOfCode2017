

myArraySwap<-function(entry){
  size <- length(entry)
  pointer <- size
  
  return <- matrix(c(1))
  
  for(x in 1 : size){
    return[x]<-entry[pointer]
    pointer = pointer-1
  }
  return
}


myDetermineMatixSize<-function(entry){
  higherValue = 0
  return      = 0
  while (higherValue < entry) {
    return = return + 1
    higherValue = (return * return)
    
  }
  return
}

mySpiralMatrix <- function(n) {
  stopifnot(is.numeric(n))
  stopifnot(n > 0)
  
  steps <- c(1, n, -1, -n)
  reps <- n - seq_len(n * 2 - 1L) %/% 2
  
  indicies <- rep(rep_len(steps, length(reps)), reps)
  indicies <- cumsum(indicies)
  # custom development
  indicies<-myArraySwap(indicies)
  
  values <- integer(length(indicies))
  values[indicies] <- seq_along(indicies)
  
  matrix(values, n, n, byrow = TRUE)
}


myStressMatrix<-function(entry){
  
  x=0
  y=0
  j=0
  k=0
  stepsNumber = 1
  
  
}

myManhattanDistance<-function(entry){

  s = myDetermineMatixSize(entry)
  m = mySpiralMatrix(s)
  
  searchResult <- which(m==entry, arr.in=TRUE)
  centerResult <- which(m==1, arr.in=TRUE)
  
  searchX <- searchResult[1,1]
  searchY <- searchResult[1,2]
  
  centerX <- centerResult[1,1]
  centery <- centerResult[1,2]
  
  return = (abs(centerX-searchX)+abs(centery-searchY))  
  return
}

# # -*-*-*-*-*-*-*-*-*-*-*-*
# # UNIT TESTING
# # -*-*-*-*-*-*-*-*-*-*-*-*
# # ASSERTS

source('app/library/Assert.R')

# -*-*-*-*-*-*-*-*-*-*-*-*
# VARIANTS
# -*-*-*-*-*-*-*-*-*-*-*-*
# 1
entry = 1

actual   = myManhattanDistance(entry)
expected = 0

message  = 'Tip1'
myAssert.integer.equals(message, expected, actual)

# 2
entry = 12

actual   = myManhattanDistance(entry)
expected = 3

message  = 'Tip1'
myAssert.integer.equals(message, expected, actual)

# 3
entry = 23

actual   = myManhattanDistance(entry)
expected = 2

message  = 'Tip1'
myAssert.integer.equals(message, expected, actual)

# 4
entry = 1024

actual   = myManhattanDistance(entry)
expected = 31

message  = 'Tip1'
myAssert.integer.equals(message, expected, actual)

# 5
entry = 368078

actual   = myManhattanDistance(entry)
expected = 371

message  = 'Exercise'
myAssert.integer.equals(message, expected, actual)