naive_bayes <- function(testdata, traindata, t_labels){
  traindata <- as.data.frame(traindata)
  t_labels <- as.data.frame(t_labels)
  train <- cbind(traindata, t_labels)
  
  #Divding the data according to the classes
  class_apple <- train[which(train[,5] == 'apple'),]
  class_avocado <- train[which(train[,5] == 'avocado'),]
  class_banana <- train[which(train[,5] == 'banana'),]
  class_dates <- train[which(train[,5] == 'dates'),]
  class_lemon <- train[which(train[,5] == 'lemon'),]
  class_lychee <- train[which(train[,5] == 'lychee'),]
  class_mango <- train[which(train[,5] == 'mango'),]
  class_orange <- train[which(train[,5] == 'orange'),]
  class_pomengrate <- train[which(train[,5] == 'pomengrate'),]
  class_raspberry <- train[which(train[,5] == 'raspberry'),]
  class_starwberry <- train[which(train[,5] == 'starwberry'),]
  
  
  #Calculating probability for each class(i.e prior)
  p_apple <- nrow(as.matrix(class_apple)) / 4048
  p_avocado <- nrow(as.matrix(class_avocado)) / 4048
  p_banana <- nrow(as.matrix(class_banana)) / 4048
  p_dates <- nrow(as.matrix(class_dates)) / 4048
  p_lemon <- nrow(as.matrix(class_lemon)) / 4048
  p_lychee <- nrow(as.matrix(class_lychee)) / 4048
  p_mango <- nrow(as.matrix(class_mango)) / 4048
  p_orange <- nrow(as.matrix(class_orange)) / 4048
  p_pomengrate <- nrow(as.matrix(class_pomengrate)) / 4048
  p_raspberry <- nrow(as.matrix(class_raspberry)) / 4048
  p_strawberry <- nrow(as.matrix(class_starwberry)) / 4048
  
  
  #Calculating likelihood for each feature of each class
  posterior <- function(x, classtype, prior){
    likelihood = 1
    for (i in c(1:4356)) {
      likelihood <- likelihood * dnorm(x[i], mean(classtype[,i]), sd(classtype[,i]))
    }
    
    vec <- prior * likelihood 
  }
  
  #Matrix that stores predicted class labels for the testing data
  preds = matrix(nrow = 0, ncol = 1)
  
  #Iterating each row of test data
  for(i in c(1:1640)){
    answer <- list(apples = posterior(testdata[i,], class_apple, p_apple),
                   avocado = posterior(testdata[i,], class_avocado, p_avocado),
                   banana = posterior(testdata[i,], class_banana, p_banana),
                   dates = posterior(testdata[i,], class_dates, p_dates),
                   lemon = posterior(testdata[i,], class_lemon, p_lemon),
                   lychee = posterior(testdata[i,], class_lychee, p_lychee),
                   mango = posterior(testdata[i,], class_mango, p_mango),
                   orange = posterior(testdata[i,], class_orange, p_orange),
                   pomengrate = posterior(testdata[i,], class_pomengrate, p_pomengrate),
                   raspberry = posterior(testdata[i,], class_raspberry, p_raspberry),
                   strawberry = posterior(testdata[i,], class_starwberry, p_strawberry))
    
    
    #Stores class label of the maximum posterior probability
    predicts <- toString(names(which.max(answer)))
    preds <- rbind(preds, predicts)
    print('done')
  }
  return(preds)
}


#Train labels
labels1 <- matrix(nrow = 4048,ncol = 1)#vector(length = 280)
for(i in 1:180)
  labels1[i] <- 'Apples'

for(i in 181:552)
  labels1[i] <- 'Avocado'

for(i in 553:943)
  labels1[i] <- 'Banana'

for(i in 944:1345)
  labels1[i] <- 'Dates'

for(i in 1346:1745)
  labels1[i] <- 'Lemon'

for(i in 1746:2136)
  labels1[i] <- 'Lychee'

for(i in 2137:2538)
  labels1[i] <- 'Mango'

for(i in 2539:2838)
  labels1[i] <- 'Orange'

for(i in 2839:3242)
  labels1[i] <- 'Pomengrate'

for(i in 3243:3644)
  labels1[i] <- 'Raspberry'

for(i in 3645:4048)
  labels1[i] <- 'Strawberry'


#Actual test labels
actual <- matrix(nrow = 1640, ncol = 1) #vector(length = 150)
for(i in 1:158)
  actual[i] <- 'Apples'

for(i in 159:257)
  actual[i] <- 'Avocado'

for(i in 258:423)
  actual[i] <- 'Banana'

for(i in 424:589)
  actual[i] <- 'Dates'

for(i in 590:654)
  actual[i] <- 'Lemon'

for(i in 655:820)
  actual[i] <- 'Lychee'

for(i in 821:986)
  actual[i] <- 'Mango'

for(i in 987:1145)
  actual[i] <- 'Oranges'

for(i in 1146:1309)
  actual[i] <- 'Pomengrate'

for(i in 1310:1476)
  actual[i] <- 'Raspberry'

for(i in 1477:1640)
  actual[i] <- 'Strawberry'


#Getting features generated from HOG for training images and creating trainingMatrix
fileList <-list.files(path="E:/Concordia projects/INSE 6180(Data Mining)/Code/output_hog_train")

trainingMatrix <- matrix(data = 0 ,nrow = 4048, ncol = 4356 )
j=0
for(i in fileList)
{
  j=j+1
  fileName <- paste("E:/Concordia projects/INSE 6180(Data Mining)/Code/output_hog_train/",i,sep ="")
  featureVector <- readRDS(file = fileName)
  trainingMatrix[j,] <- featureVector
}


#Getting features generated from HOG for testing images and creating testingMatrix
fileList1 <-list.files(path="E:/Concordia projects/INSE 6180(Data Mining)/Code/output_hog_test")
testingMatrix <- matrix(data = 0, nrow = 1640, ncol = 4356)
j=0
for(i in fileList1)
{
  j=j+1
  fileName1 <- paste("E:/Concordia projects/INSE 6180(Data Mining)/Code/output_hog_test/",i,sep ="")
  featureVector1 <-readRDS(file = fileName1)
  testingMatrix[j,] <- featureVector1
}

pred = naive_bayes(testingMatrix, trainingMatrix, labels1)
print(table(pred, actual))   #confusion matric

#Calculating accuracy
count <- 0
for(i in 1:length(actual)){
  if(pred[i] == actual[i]){
    count <- count + 1
  }
}
accuracy <- count/nrow(actual)
print(accuracy)


