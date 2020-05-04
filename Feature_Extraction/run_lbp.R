img_1 <-init()

#storing feature vector for TRAINING images using LBP method
fileList <-list.files(path="E:/Concordia projects/INSE 6180(Data Mining)/Code/Training Images")
for(i in fileList)
{
fileName <- paste("E:/Concordia projects/INSE 6180(Data Mining)/Code/Training Images/",i,sep ="")

 featureVector <-lbp(fileName,3,3)
 featureVector <-  ((featureVector - min(featureVector)) / (max(featureVector) - min(featureVector)))
 output<- paste("E:/Concordia projects/INSE 6180(Data Mining)/Code/output_lbp_train/",i,".rds",sep= "")
 #print(output)
 saveRDS(featureVector,file=output)
}


#storing feature vector for TESTING images using LBP method
fileList1 <-list.files(path="E:/Concordia projects/INSE 6180(Data Mining)/Code/Testing images")
for(i in fileList1)
{
  fileName1 <- paste("E:/Concordia projects/INSE 6180(Data Mining)/Code/Testing images/",i,sep ="")

  featureVector1 <-lbp(fileName1,3,3)
  featureVector1 <-  ((featureVector1 - min(featureVector1)) / (max(featureVector1) - min(featureVector1)))
  output1<- paste("E:/Concordia projects/INSE 6180(Data Mining)/Code/output_lbp_test/",i,".rds",sep= "")
  #print(output1)
  saveRDS(featureVector1,file=output1)
}

