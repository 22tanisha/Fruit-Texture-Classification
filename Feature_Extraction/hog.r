hog_feature <- function(fileName){
  
  library(EBImage)
  library(spatstat)
  library(OpenImageR)
  
  #i<- readImage(fileName)
  z<-readImage(fileName)
  # resize(i, 64, 64, filter = "bilinear", output.dim = c(64, 64))
  
  
  #dim(z)
  # print(z)
  # imageShow(z)
  # print(z)
  x <- dim(z)[1]
  y <- dim(z)[2]
  
  gray <- matrix(nrow = x, ncol = y)
  for (i in 1:x)
  {
    for (j in 1:y)
    {
      gray[i, j] <- mean(z [i, j, ])
    }
  }
  #imageShow(gray)
  
  rows <- nrow(gray);
  
  cols <- ncol(gray)
  
  Ix=gray
  Iy=gray
  # Gradients in X and Y direction. Iy is the gradient in X direction and Iy
  #is the gradient in Y direction
  
  for (i in 1:rows-2){
    Iy[i,] <- gray[i,]-gray[i+2,]
  }
  
  for (i in 1:cols-2){
    Ix[,i] <- gray[,i]-gray[,i+2];
  }
  
  
  #library(EBImage)
  gray <- gblur(gray,sigma = 4) #Initialized a gaussian filter with sigma=0.5 * block width.  
  
  #imageShow(gray)
  
  angle=atan(Ix/ Iy)
  angle <- angle*180/3.14
  
  # Matrix containing the angles of each edge gradient
  angle1=  angle +90; #Angles in range (0,180)
  angle1[angle1 < 0] <- 0
  
  magnitude= sqrt(Ix*Ix + Iy*Iy)
  
  feature=c()
  
  # for Blocks
  for ( i in 0: (rows/8 - 2))
  {  
    for (j in  0: (cols/8 -2))
    {
      xlow <- 8*i+1
      
      xhigh <-  8*i+16
      ylow <- 8*j+1
      yhigh<- 8*j+16
      mag_patch = magnitude[xlow : xhigh, ylow : yhigh];
      ang_patch = angle1[xlow : xhigh , ylow : yhigh]
      block_feature <- c()
      
      #%Iterations for cells in a block
      for (x in 0:1)
      {
        for (y in 0:1)
        {
          xlow <- 8*x+1
          xhigh<-8*x+8
          ylow <- 8*y+1
          yhigh <-8*y+8
          angleA =ang_patch[xlow:xhigh, ylow:yhigh]
          magA   =mag_patch[xlow:xhigh, ylow:yhigh]
          
          
          histr <-  matrix(data =  0, nrow =1, ncol = 9)
          
          
          for ( p in 1:8)
          {
            for (q in 1:8)
            {                    
              
              alpha <- as.numeric( angleA[p,q])
              if(is.nan(alpha))
              {
                alpha<-0
                next
              }
              
              if (alpha >=0 && alpha<20)
              {
                histr[1]=histr[1]+ magA[p,q]*(20-alpha)/20
                histr[2]=histr[2]+ magA[p,q]*(alpha-0)/20
              }
              
              if  (alpha>=20 && alpha<40)
              {
                histr[2]=histr[2]+ magA[p,q]*(40-alpha)/20                
                histr[3]=histr[3]+ magA[p,q]*(alpha-20)/20
              }
              
              if  (alpha>=40 && alpha<60)
              {
                histr[3]=histr[3]+ magA[p,q]*(60-alpha)/20
                histr[4]=histr[4]+ magA[p,q]*(alpha-40)/20
              }
              if  (alpha>=60 && alpha<80)
              {
                histr[4]=histr[4]+ magA[p,q]*(80-alpha)/20
                histr[5]=histr[5]+ magA[p,q]*(alpha-60)/20
              }
              if (alpha>=80 && alpha<100)
              {
                histr[5]=histr[5]+ magA[p,q]*(100-alpha)/20
                histr[6]=histr[6]+ magA[p,q]*(alpha-80)/20
              }
              
              if (alpha>=100 && alpha<120)
              {
                histr[6]=histr[6]+ magA[p,q]*(120-alpha)/20
                histr[7]=histr[7]+ magA[p,q]*(alpha-100)/20
              }
              if (alpha>=120 && alpha<140)
              {
                histr[7]=histr[7]+ magA[p,q]*(140-alpha)/20
                histr[8]=histr[8]+ magA[p,q]*(alpha-120)/20
              }
              if (alpha>=140 && alpha<160)
              {
                histr[8]=histr[8]+ magA[p,q]*(160-alpha)/20
                histr[9]=histr[9]+ magA[p,q]*(alpha-140)/20
              }
              
              if (alpha>=160 && alpha<=180) 
              {
                histr[1]=histr[1]+ magA[p,q]*(alpha-160)/20
                histr[9]=histr[9]+ magA[p,q]*(180-alpha)/20
              }
              # else (alpha>170 && alpha<=180)
              # {
              #   histr[9]=histr[9]+ magA[p,q]*(180-alpha)/20
              #   histr[1]=histr[1]+ magA[p,q]*(alpha-170)/20
              # 
            }
          }
          block_feature <- c(block_feature, histr) # Concatenation of Four histograms to form one block feature
          
        }
      }
      
      #Normalize the values in the block using L1-Norm
      norm_vec <- function(block_feature) sqrt(sum(x^2))
      block_feature=block_feature/sqrt(norm_vec(block_feature)^2+.01)
      
      feature=c(feature, block_feature) #Features concatenation
    }
  }
  
  norm_vec <- function(feature) sqrt(sum(x^2))
  feature=feature/sqrt(norm_vec(feature)^2+.001)
  
  for( z in 1:length(feature))
  {
    
    if (feature[z]>0.2)
    {
      feature[z]=0.2
    }
    if(feature[z]< 0.000)
      feature[z]=0
  }
  feature=feature/sqrt(norm_vec(feature)^2+.001)  
}
