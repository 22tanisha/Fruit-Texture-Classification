#Getting features generated from LBP for training images and creating trainingMatrix
fileList <-list.files(path="E:/Concordia projects/INSE 6180(Data Mining)/Code/output_lbp_train")

trainingMatrix <- matrix(data = 0 ,nrow = 4048, ncol = 2304 )
j=0
for(i in fileList)
{
  j=j+1
  fileName <- paste("E:/Concordia projects/INSE 6180(Data Mining)/Code/output_lbp_train/",i,sep ="")
  featureVector <- readRDS(file = fileName)
  trainingMatrix[j,] <- featureVector
}

#Getting features generated from LBP for testing images and ceating testingMatrix
fileList1 <-list.files(path="E:/Concordia projects/INSE 6180(Data Mining)/Code/output_lbp_test")
testingMatrix <- matrix(data = 0, nrow = 1640, ncol = 2304)
j=0
for(i in fileList1)
{
  j=j+1
  fileName1 <- paste("E:/Concordia projects/INSE 6180(Data Mining)/Code/output_lbp_test/",i,sep ="")
  featureVector1 <-readRDS(file = fileName1)
  testingMatrix[j,] <- featureVector1
}

#Implemented kNN classifier
myknn = function(target, traindata, testdata, k) {
  
  n = nrow(testdata)
  pred = rep(NA_character_, n)
  trainlabels = target[,1]
  
  for(i in 1:n) {
    # compute squared euclidean distances to all instances in training set
    nn = order(apply(traindata, 1, function(x) sqrt(sum((x - testdata[i, ])^2))))[1:k]
    # compute frequencies of classes
    class.frequency = table(trainlabels[nn])
    most.frequent.classes = names(class.frequency)[class.frequency == max(class.frequency)]
    # tie breaking
    pred[i] = sample(most.frequent.classes, 1)
  }
  return(as.matrix(pred))
}


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


pred = myknn(labels1, traindata = trainingMatrix, testdata = testingMatrix, k = 3)
print(table(pred, actual))   #confusion matrix

#Calculating accuracy
count <- 0
for(i in 1:length(actual)){
  if(pred[i] == actual[i]){
    count <- count + 1
  }
}
accuracy <- count/nrow(actual)
print(accuracy)
