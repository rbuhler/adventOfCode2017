

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

#368078
input = 368078

s = myDetermineMatixSize(input)
m = mySpiralMatrix(s)

searchResult <- which(m==input, arr.in=TRUE)
centerResult <- which(m==1, arr.in=TRUE)

searchX <- searchResult[1,1]
searchY <- searchResult[1,2]

centerX <- centerResult[1,1]
centery <- centerResult[1,2]

result = (abs(centerX-searchX)+abs(centery-searchY))  

print(paste0("Final : ", result))