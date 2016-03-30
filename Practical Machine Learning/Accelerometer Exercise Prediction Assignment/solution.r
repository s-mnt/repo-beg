library(caret)
library(randomForest)

# Downloading and reading the data
con <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
download.file(con, "C:/trainingdata.csv")
training <- read.csv("C:/trainingdata.csv")
con2 <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
download.file(con2, "C:/testing.csv")
testing <- read.csv("C:/testing.csv")

# Cleaning the data
missing <- rep(0, times = ncol(training))
for (i in 1:ncol(training)) {
# Replacing blank values in dataset with NA
		training[i][training[i] == ""] <- NA
# Total count of missing values in every column
		missing[i] <- sum(is.na(training[i]))
}
missing <- missing/nrow(training)
table(missing)
# Removing columns having more than 60% of the values as missing
y <- missing < 0.6
training <- training[y]
testing <- testing[y]
# Removing irrelevant columns from training and test set
training <- training[-c(1,3,4,5)]
testing <- testing[-c(1,3,4,5)]

# Splitting training set into training and validation set
ind <- createDataPartition(training$classe, p = 0.7, list = FALSE)
tr <- training[ind,]
cv <- training[-ind,]

# Fitting Random Forest model on training set with 10-fold cross-validation
modFit <- train(classe ~ ., method = "rf", data = tr, trControl = trainControl(method = "cv", number = 10))
# Predicting for validation testt
predcv <- predict(modFit, cv)
# Measuring the accuracy of model on validation set
confusionMatrix(predcv, cv$classe)

# Predicting outcome for test set
pred <- predict(modFit, testing)






