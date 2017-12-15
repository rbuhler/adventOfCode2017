
# -*-*-*-*-*-*-*-*-*-*-*-*
# SOLUTION LIBRARY
# -*-*-*-*-*-*-*-*-*-*-*-*
myCreateMatrix<-function(node){
  
  for(x in 1 : node){
    
  }
  
}


# -*-*-*-*-*-*-*-*-*-*-*-*
mySpiralMemory<-function(entry){
  result = 0

  
  #Return value 
  result
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
entry    = 1
actual   = mySpiralMemory(entry)
expected = 0

message  = 'happy path 1'
myAssert.integer.equals(message, expected, actual)

# 2
entry    = 12
actual   = mySpiralMemory(entry)
expected = 3

message  = 'happy path 2'
myAssert.integer.equals(message, expected, actual)

# 3
entry    = 23
actual   = mySpiralMemory(entry)
expected = 2

message  = 'happy path 3'
myAssert.integer.equals(message, expected, actual)

# 4
entry    = 1024
actual   = mySpiralMemory(entry)
expected = 31

message  = 'happy path 3'
myAssert.integer.equals(message, expected, actual)

# 5
entry    = 368078
actual   = mySpiralMemory(entry)
expected = 0

message  = 'Exercise'
myAssert.integer.equals(message, expected, actual)