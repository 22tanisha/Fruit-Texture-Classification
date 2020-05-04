lbp <- function(img,X,Y)
{
  img_apple <- readImage(img)
  x = dim(img_apple)[1]
  y = dim(img_apple)[2]
  
  gray <- matrix(nrow = x, ncol = y)
  
  for (i in 1:x)
  {
    for (j in 1:y)
    {
      gray[i, j] <- mean(img_apple[i, j, ])
    }
  }
  
  k = 0
  #lbp
  lbp = matrix(nrow = x, ncol = y)
  for (xindex in 2:x - 1)
  {
    for (yindex in 2:y - 1)
    { 
      centerPixel <- gray[xindex, yindex]
      k = as.numeric( gray[xindex - 1, yindex - 1])
      pixel1 <- ifelse(centerPixel < k , 1, 0)
      
      k= as.numeric( gray[xindex - 1, yindex])
      pixel2 <- ifelse(centerPixel < k , 1, 0)
      
      k= as.numeric( gray[xindex - 1, yindex + 1] )
      pixel3 <- ifelse(centerPixel <k, 1, 0)
      
      k= as.numeric( gray[xindex, yindex + 1] )
      pixel4<- ifelse(centerPixel <k , 1, 0)
      
      k= as.numeric( gray[xindex + 1, yindex + 1])
      pixel5<- ifelse(centerPixel <k , 1, 0)
      
      k= as.numeric( gray[xindex + 1, yindex])
      pixel6<- ifelse(centerPixel<k , 1, 0)
      
      k= as.numeric( gray[xindex + 1, yindex - 1] )
      pixel7<- ifelse(centerPixel <k, 1, 0)
      
      k= as.numeric( gray[xindex, yindex - 1])
      pixel8<- ifelse(centerPixel <k , 1, 0)
      
      vInt<- (pixel1 * (2 ^ 7)) + (pixel2 * (2 ^ 6)) + (pixel3 * (2 ^ 5)) + (pixel4 * (2 ^ 4)) + (pixel5 * (2 ^ 3)) + (pixel6 * (2 ^ 2)) + (pixel7 *  (2 ^ 1)) + (pixel8 * (2 ^ 0))
      lbp[xindex, yindex] <- ifelse( length(vInt) == 0 ,0,vInt)
    }
  }
  #concatenated histogram feasture vector
  
  #define grid size 
  xGrid = X
  yGrid = Y
  featureVector <- vector()
  xlength = ceiling(x/xGrid)
  ylength = ceiling(y/yGrid)
  
  for(i in 0:(xGrid-1))
  {
    for(j in 0:(yGrid-1))
    {
      xInit <- xlength*i
      xUpper <- xlength*(i+1)
      
      yInit <- ylength*j
      yUpper <- ylength*(j+1)
      #for first window of every row
      if(xInit ==0)
      {
        xInit <-1
      }
      if(yInit ==0)
      {
        yInit <-1
      }
      
      #check length constraint for the last cell 
      if(xUpper > x)
      {
        xUpper <-x
      }
      if(yUpper > y)
      {
        yUpper<-y
      }
      histogram = hist(x = lbp[xInit:xUpper,yInit:yUpper],freq = TRUE,breaks = seq(0,256,by = 1))$counts
      featureVector<- c(featureVector,histogram)
    }
  }
  return(featureVector)
  #need to store feature vector to a file/db 
}

#result1 <- lbp('E:/Concordia projects/INSE 6180(Data Mining)/Code/Apple.jpeg',3,3)