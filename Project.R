library(caret)
#Columns in the training dataset with missing values are removed
data <- read.csv("pml-training.csv", header = TRUE, na.strings = c("NA", ""))
csums <- colSums(is.na(data))
cs <- (csums == 0)
data <- data[, (colSums(is.na(data)) == 0)]

trainIndex <- createDataPartition(y = data$classe, p=0.2,list=FALSE)
traindata <- data[trainIndex,]
removeIndex <- grep("timestamp|X|user_name|new_window",names(traindata))
traindata <- traindata[,-removeIndex]

modFit <- train(traindata$classe ~.,  method = "rf", data = traindata, trControl = trainControl(method = "cv", number = 4), importance = TRUE)

data2 <- read.csv("pml-testing.csv", header = TRUE, na.strings = c("NA", ""))
data2 <- data2[, (colSums(is.na(data)) == 0)]

pred <- predict(modFit, data2[,-54])
pred
