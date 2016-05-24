# Loading training and test sets
training <- read.csv("C:/train.csv")
testing <- read.csv("C:/test.csv")

# Merging both training and testing sets
outcome <- training[, ncol(training)]
training <- training[, -c(1, ncol(training))]
ID <- testing[, 1]
testing <- testing[, -1]
complete <- rbind(training, testing)

names(complete) <- c("monthsincelastdonation", "numberofdonation", "totalvolumedonation", "monthsincefirstdonation")

# Data splitting into training and testing sets
training <- complete[1:nrow(training), ]
training <- cbind(training, outcome)
testing <- complete[(1+nrow(training)):nrow(complete), ]

training$outcome[training$outcome == 1] <- "Yes"
training$outcome[training$outcome == 0] <- "No"
training$outcome <- as.factor(training$outcome)

# Exploratory Data Graphs
library(ggplot2)
qplot(monthsincelastdonation, data = training, geom = "density", facets = . ~ outcome)
qplot(numberofdonatison, data = training, geom = "density", facets = . ~ outcome)
qplot(totalvolumedonation, data = training, geom = "density", facets = . ~ outcome)
qplot(monthsincefirstdonation, data = training, geom = "density", facets = . ~ outcome)

# Splitting training into sub-training and validation set
library(caret)
set.seed(1234)
ind <- createDataPartition(y = training$outcome, p = 0.7, list = FALSE)
tr <- training[ind, ]
val <- training[-ind, ]

table(training$outcome)

library(caret)
set.seed(1234)
up_train <- upSample(x = tr[, -ncol(tr)], y = tr[, ncol(tr)])
names(up_train)[ncol(up_train)] <- "outcome"
table(up_train$outcome)


# Model Averaged Neural Network
set.seed(1234)
modFit_nnet <- train(outcome ~ monthsincelastdonation + numberofdonation + monthsincefirstdonation, method = "avNNet", data = tr, metric = "ROC", trControl = trainControl(method = "cv", number = 10, summaryFunction = twoClassSummary, classProbs = TRUE))

# Predicting on training set
predtr_nnet_raw <- predict(modFit_nnet, tr, type = "raw")
predtr_nnet_prob <- predict(modFit_nnet, tr, type = "prob")
confusionMatrix(predtr_nnet_raw, tr$outcome)    

write.csv(predtr_nnet_prob$Yes, "C:/predtr_nnet_prob.csv", row.names = FALSE)

# Prediction on validation set
predval_nnet_raw <- predict(modFit_nnet, val, type = "raw")
predval_nnet_prob <- predict(modFit_nnet, val, type = "prob")
confusionMatrix(predval_nnet_raw, val$outcome)      

write.csv(predval_nnet_prob$Yes, "C:/predval_nnet_prob.csv", row.names = FALSE)

# Prediction on testing set
pred_nnet_raw <- predict(modFit_nnet, testing, type = "raw")
pred_nnet_prob <- predict(modFit_nnet, testing, type = "prob")

write.csv(pred_nnet_prob$Yes, "C:/pred_nnet_prob.csv", row.names = FALSE)


# C5.0
set.seed(1234)
modFit_c5 <- train(outcome ~ monthsincelastdonation + numberofdonation + monthsincefirstdonation, method = "C5.0", data = tr, metric = "ROC", trControl = trainControl(method = "cv", number = 10, summaryFunction = twoClassSummary, classProbs = TRUE))

# Predicting on training set
predtr_c5_raw <- predict(modFit_c5, tr, type = "raw")
predtr_c5_prob <- predict(modFit_c5, tr, type = "prob")
confusionMatrix(predtr_c5_raw, tr$outcome)      

write.csv(predtr_c5_prob$Yes, "C:/predtr_c5_prob.csv", row.names = FALSE)

# Prediction on validation set
predval_c5_raw <- predict(modFit_c5, val, type = "raw")
predval_c5_prob <- predict(modFit_c5, val, type = "prob")
confusionMatrix(predval_c5_raw, val$outcome)       
		 
write.csv(predval_c5_prob$Yes, "C:/predval_c5_prob.csv", row.names = FALSE)

# Prediction   

pred_c5_raw <- predict(modFit_c5, testing, type = "raw")
pred_c5_prob <- predict(modFit_c5, testing, type = "prob")

write.csv(pred_c5_prob$Yes, "C:/pred_c5_prob.csv", row.names = FALSE)


eval <- cbind(predval_xgb_prob$Yes, predval_gbm_prob$Yes, predval_treebag_prob$Yes, data.frame(val$outcome))

comb <- cbind((predval_c5_prob$Yes + predval_nnet_prob$Yes)/2, data.frame(val$outcome))
comb2 <- cbind(ID, (pred_c5_prob$Yes + pred_nnet_prob$Yes)/2)
write.csv(comb2, "C:/submission.csv", row.names = FALSE)
comb2[,2][comb2[,2] > 0.4] <- 0.7

submission <- cbind(ID, pred_c5_prob$Yes)
write.csv(submission, "C:/submission.csv", row.names = FALSE)
