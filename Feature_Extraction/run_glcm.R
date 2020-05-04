setwd('E:/Concordia projects/INSE 6180(Data Mining)/Code')
img_1 <-init()

#storing feature vector for TRAINING images using GLCM method
fileList <-list.files(path="E:/Concordia projects/INSE 6180(Data Mining)/Code/Training Images")
for(i in fileList)
{
  fileName <- paste("E:/Concordia projects/INSE 6180(Data Mining)/Code/Training Images/",i,sep ="")
  featureVector <- hog_feature(fileName)
  output <- paste("E:/Concordia projects/INSE 6180(Data Mining)/Code/output_glcm_train/",i,".rds",sep= "")
  
  saveRDS(featureVector,file=output)
}


#storing feature vector for TESTING images using GLCM method
fileList1 <-list.files(path="E:/Concordia projects/INSE 6180(Data Mining)/Code/Testing images")
for(i in fileList1)
{
  fileName1 <- paste("E:/Concordia projects/INSE 6180(Data Mining)/Code/Testing images/",i,sep ="")
  featureVector1 <- hog_feature(fileName1)
  output1 <- paste("E:/Concordia projects/INSE 6180(Data Mining)/Code/output_glcm_test/",i,".rds",sep= "")
  
  saveRDS(featureVector1,file=output1)
}