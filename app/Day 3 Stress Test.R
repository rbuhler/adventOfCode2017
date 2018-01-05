# Set up some input
myCenterMatrix<-function(entry){
  middle = ceiling(entry/2)
  middle
}

myUnclock<-function(entry){
  if(entry=='RIGHT'){
    return('UP')
  } else if (entry=='UP'){
    return('LEFT')
  }else if(entry=='LEFT'){
    return('DOWN')
  }else if(entry=='DOWN'){
    return('RIGHT')
  }else{
    return('RIGHT')
  }
}

myNextCell<-function(entry){
  
  df<-entry
  
  direct<-df['direction']  
  posX<-df['x']
  posY<-df['y']
  
  if(direct=='RIGHT'){
    posY<-posY+1
    
  } else if(direct=='UP'){
    posX<-posX-1
    
  }else if(direct=='LEFT'){
    posY<-posY-1
    
  }else if(direct=='DOWN'){
    posX<-posX+1
  }
  
  df['direction']<-direct
  df['x']<-posX
  df['y']<-posY
  
  return(df)
}

myLayer<-function(entry){
  return<-(entry+2)
  return
}


mySpiralMatrix<-function(entry){
  
  matrixDimX  = ceiling(sqrt(entry))
  matrixSize  = matrixDimX ^ 2
  
  x<-myCenterMatrix(matrixDimX)
  y<-x
  celValue<-0
  
  layer<-0
  loop<-1
  direction<-''
  changeDirection<-0
  arrayMatrix<-as.matrix(array(as.integer(0), dim=c(matrixDimX, matrixDimX)))
  
  dFrame<-data.frame(direction, x, y)
  
  for (count in 1 : matrixSize){
    
    # neighbourRight<-if((y+1)<=matrixDimX){y+1}else{y}
    # neighbourLeft<-if((y-1)>=1){y-1}else{y}
    # neighbourTop<-if((x-1)>=1){x-1}else{x}
    # neighbourBottom<-if((x+1)<=matrixDimX){x+1}else{x}
    neighbourRight<-y+1
    neighbourLeft<-y-1
    neighbourTop<-x-1
    neighbourBottom<-x+1
    
    vRight<-0
    vLeft<-0
    vTop<-0
    vBottom<-0
    vNWest<-0
    vNEast<-0
    vSWest<-0
    vSEast<-0
    
    # ----------------------------------------------------------------    
    if(neighbourTop>=1){
      vTop<-if(neighbourTop>=1){arrayMatrix[neighbourTop, y]}
      
      if(neighbourLeft>=1){
        vNWest<-arrayMatrix[neighbourTop, neighbourLeft]  
      }
      if(neighbourRight<=matrixDimX){
        vNEast<-arrayMatrix[neighbourTop, neighbourRight]   
      }
    }
    
    if(neighbourBottom<=matrixDimX){
      vBottom<-arrayMatrix[neighbourBottom, y]
      
      if(neighbourLeft>=1){
        vSWest<-arrayMatrix[neighbourBottom, neighbourLeft]
      }      
      if(neighbourRight<=matrixDimX){
        vSEast<-arrayMatrix[neighbourBottom, neighbourRight] 
      }      
    }
    # ----------------------------------------------------------------    
    if(neighbourLeft>=1){
      vLeft<-if(neighbourLeft>=1){arrayMatrix[x, neighbourLeft]}  
    }
    
    if(neighbourRight<=matrixDimX){
      vRight<-arrayMatrix[x, neighbourRight]   
    }
    # ----------------------------------------------------------------
    
    # Continuous values
    #celValue<-count
    celValue<-vRight + vLeft + vTop + vBottom +
      vNWest + vNEast + vSWest + vSEast
    
    if(celValue > entry ){
      print(paste0("Answer ", celValue))
      break()
    }
    # First cell
    if(celValue==0){celValue=1}
    
    arrayMatrix[x , y]<-celValue
    
    # Adjust the next Layer
    loop<-loop-1
    if(loop==0){
      layer<-myLayer(layer)
      loop<-layer
      
      direction<-myUnclock(direction)
      changeDirection<-loop/2
    }
    
    # Change direction
    if(loop==changeDirection){
      direction<-myUnclock(direction)  
    }
    
    dFrame['direction']<-direction
    dFrame<-myNextCell(dFrame)
    x<-as.integer(dFrame['x'])
    y<-as.integer(dFrame['y'])
    
  } # for
  return(arrayMatrix)
}

target<-368078

#print(mySpiralMatrix(target))
mySpiralMatrix(target)