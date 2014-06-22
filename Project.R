library(caret)
#Loading the caret library and reading the training dataset
#Columns with missing values are removed
data <- read.csv("pml-training.csv", header = TRUE, na.strings = c("NA", ""))
csums <- colSums(is.na(data))
cs <- (csums == 0)
data <- data[, (colSums(is.na(data)) == 0)]

#Deleting additional unnecessary columns from the training dataset
trainIndex <- createDataPartition(y = data$classe, p=0.2,list=FALSE)
traindata <- data[trainIndex,]
removeIndex <- grep("timestamp|X|user_name|new_window",names(traindata))
traindata <- traindata[,-removeIndex]

#Training the model using a random forest approach on the train data partition
#Using a cross validation method when applying the random forest routine 
modFit <- train(traindata$classe ~.,  method = "rf", data = traindata, trControl = trainControl(method = "cv", number = 4), importance = TRUE)

#Reading the testing dataset and removing columns with NAs
data2 <- read.csv("pml-testing.csv", header = TRUE, na.strings = c("NA", ""))
data2 <- data2[, (colSums(is.na(data)) == 0)]

#Finally, predicting values on the testing dataset after removing the column 54 (that one makes problem)
pred <- predict(modFit, data2[,-54])
pred
