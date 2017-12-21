# Set up some input
myCenterMatrix<-function(entry){
  middle = ceiling(entry/2)
  middle
    
}

mySpiralMatrix<-function(entry){
  
  matrixDimX  = ceiling(sqrt(entry))
  matrixSize  = matrixDimX ^ 2
  
  rounds=1
  x<-0
  y<-0
  jx<-0
  ky<-1

  for (count in 1 : matrixSize){

    # Steps
    if(jx==1){
      jx=-1
    }else if(jx==-1){
      jx=1
    }
    
    if(jx==1){
      ky=-1
    }else if(jx==-1){
      ky=1
    }
    
    # Coordinates
    x<-0
    y<-0
    
    array<-c(1 : matrixSize)
    myMatrix<-matrix(array, nrow=matrixDimX, ncol=matrixDimX, byrow = TRUE)

  }
  print(myMatrix)
  myMatrix
  
}

mySpiralMatrix(25)

print()


#print(paste0("Center ", myCenterMatrix(5)))
#print(paste0("Center ", myCenterMatrix(608)))