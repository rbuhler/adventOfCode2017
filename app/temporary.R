
myDetermineMatixSize<-function(entry){
  
  higherValue = 0
  valueOfn    = 1
  while (higherValue < entry) {
  
    higherValue = (valueOfn * valueOfn)
    valueOfn = valueOfn + 1
  }
  valueOfn
}

mySpiralMatrix <- function(n) {
  stopifnot(is.numeric(n))
  stopifnot(n > 0)
  steps <- c(1, n, -1, -n)
  reps <- n - seq_len(n * 2 - 1L) %/% 2
  indicies <- rep(rep_len(steps, length(reps)), reps)
  indicies <- cumsum(indicies)
  
  values <- integer(length(indicies))
  values[indicies] <- seq_along(indicies)
  values = sort(values, decreasing = TRUE)

  matrix(values, n, n, byrow = TRUE)
}

#t = myDetermineMatixSize(368078)
s = myDetermineMatixSize(23)
m = mySpiralMatrix(s)
initialPosition = s/2

print(paste0("Matrix size :", s,":",s))
print(paste0("Initial position ", initialPosition,":", initialPosition  ))