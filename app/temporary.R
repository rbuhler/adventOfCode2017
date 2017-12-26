# Set up some input
myCenterMatrix<-function(entry){
  middle = ceiling(entry/2)
  middle
    
}

myRight<-function(entry){
  
}
myUp<-function(entry){
  
}
myLeft<-function(entry){
  
}
myDown<-function(entry){
  
}
myLayer<-function(entry){
  
  return<-(entry+2)
  return
}

mySpiralMatrix<-function(entry){
  
  matrixDimX  = ceiling(sqrt(entry))
  matrixSize  = matrixDimX ^ 2
  
  x<-matrixDimX
  y<-matrixDimX
  jx<-0
  ky<-0
  layer<-0
  loop<-1
  direction<-''
  
  data<-data.frame(layer, direction, jx, ky)

  for (count in 1 : matrixSize){

    if(loop==layer){
      print(paste0('Layer ', loop))
    }

    print(paste0('Step ', x, ':', y))

    # Adjust the next Layer
    loop<-loop-1
    if(loop==0){
      layer<-myLayer(layer)
      loop<-layer
    }

    #array<-c(1 : matrixSize)
    #myMatrix<-matrix(array, nrow=matrixDimX, ncol=matrixDimX, byrow = TRUE)

  } # for
}

mySpiralMatrix(25)

print('Done !')