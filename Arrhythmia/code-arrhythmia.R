# Read the data file
complete <- read.table("C:/complete.txt", sep = ",")

# Correct column classes 
complete$V2[complete$V2 == 0] <- "Male"
complete$V2[complete$V2 == 1] <- "Female"
complete$V11 <- as.numeric(as.character(complete$V11))
complete$V12 <- as.numeric(as.character(complete$V12))
complete$V13 <- as.numeric(as.character(complete$V13))
complete$V14 <- as.numeric(as.character(complete$V14))
complete$V15 <- as.numeric(as.character(complete$V15))

# Missing values
sum(is.na(complete))
# [1] 408

missingcount <- NULL
for (i in 1:ncol(complete)) {
		missingcount[i] <- sum(is.na(complete[,i]))		
}
misscol <- which(missingcount > (0.6 * nrow(complete)))
complete <- complete[, -misscol]

summary(complete)

# Remove near zero variance features
library(caret)
nsv <- nearZeroVar(complete[, -ncol(complete)], saveMetrics = TRUE)
sum(nsv$nzv == TRUE)
# [1] 131
complete <- complete[, !(nsv$zeroVar)]
dim(complete)
# [1] 452 262
nsv <- nearZeroVar(complete[, -ncol(complete)], saveMetrics = TRUE)
sum(nsv$freqRatio > 19)
# [1] 117
complete <- complete[, nsv$freqRatio < 19]
dim(complete)
# [1] 452 145

# Removing unclassified patients
complete <- complete[!(complete$V280 == 16), ]

# Making groups of patients as either "normal" or "patient"
complete$class <- "patient"
complete$class[complete$V280 == 1] <- "normal"
complete <- complete[, -(ncol(complete)-1)]

# Variable names
names(complete) <- paste(letters, names(complete))
names(complete) <- gsub(" ", "", names(complete))
names(complete)[ncol(complete)] <- "class"

# Splitting data into training and test sets
set.seed(1234)
ind <- createDataPartition(y = complete$class, p = 0.7, list = FALSE)
training <- complete[ind, ]
testing <- complete[-ind, ]
# write.csv(training, "C:/training.csv", row.names = FALSE)

# Splitting into sub-training and validation set
set.seed(1234)
ind <- createDataPartition(y = training$class, p = 0.7, list = FALSE)
tr <- training[ind, ]
val <- training[-ind, ]

# Filling in missing values
preObj <- preProcess(tr[,-ncol(tr)], method = "bagImpute")
tr[, -ncol(tr)] <- predict(preObj, tr[, -ncol(tr)])
val[, -ncol(val)] <- predict(preObj, val)
testing[, -ncol(testing)] <- predict(preObj, testing)
# write.csv(tr, "C:/tr.csv", row.names = FALSE)
# write.csv(val, "C:/val.csv", row.names = FALSE)
# write.csv(testing, "C:/testing.csv", row.names = FALSE)

# Assigning weights to classes of target outcome
# tr$weights <- NULL
# tr$weights[tr$class == "normal"] <- nrow(tr)/sum(tr$class == "normal")
# tr$weights[tr$class == "patient"] <- nrow(tr)/sum(tr$class == "patient")

# Gradient Boosting Model
# Training GBM model
# Pass 1
set.seed(1234)
modFit_gbm <- train(class ~ ., method = "gbm", data = tr, preProcess = c("center", "scale"), metric = "ROC", trControl = trainControl(method = "cv", number = 10, summaryFunction = twoClassSummary, classProbs = TRUE), tuneGrid = NULL, tuneLength = 9)

# Tuning parameter 'shrinkage' was held constant at a value of 0.1
# Tuning parameter 'n.minobsinnode' was held constant at a value of 10

 # interaction.depth  n.trees  ROC        Sens       Spec       ROC SD      Sens SD     Spec SD  
  # 2                  450      0.8998106  0.8643939  0.7166667  0.05696845  0.11938063  0.1652719
  

# Pass 2 
grid <- expand.grid(interaction.depth = 1:9, n.trees = c(50, 100, 150, 200, 250, 300, 350, 400, 450), shrinkage = c(.05, .1, 0.15), n.minobsinnode = c(7.5, 9, 10, 11, 12.5))

set.seed(1234)
modFit_gbm <- train(class ~ ., method = "gbm", data = tr, preProcess = c("center", "scale"), metric = "ROC", trControl = trainControl(method = "cv", number = 10, summaryFunction = twoClassSummary, classProbs = TRUE), tuneGrid = grid)

# shrinkage  interaction.depth  n.minobsinnode  n.trees  ROC        Sens       Spec       ROC SD      Sens SD     Spec SD 
  # 0.10       3                   7.5            450      0.9219592  0.8893939  0.7291667  0.04453144  0.09755453  0.16226835

# Predictions on training set
predtr_gbm_raw <- predict(modFit_gbm, tr, type = "raw")
predtr_gbm_prob <- predict(modFit_gbm, tr, type = "prob")
confusionMatrix(predtr_gbm_raw, tr$class)
# write.csv(predtr_gbm_raw, "C:/predtr_gbm_raw.csv", row.names = FALSE)
# write.csv(predtr_gbm_prob, "C:/predtr_gbm_prob.csv", row.names = FALSE)

               # Accuracy : 0.9811          
                 # 95% CI : (0.9524, 0.9948)
    # No Information Rate : 0.5708          
    # P-Value [Acc > NIR] : <2e-16          
                                          
                  # Kappa : 0.9616          
 # Mcnemar's Test P-Value : 0.6171          
                                          
            # Sensitivity : 0.9752          
            # Specificity : 0.9890          
         # Pos Pred Value : 0.9916          
         # Neg Pred Value : 0.9677          
             # Prevalence : 0.5708          
         # Detection Rate : 0.5566          
   # Detection Prevalence : 0.5613          
      # Balanced Accuracy : 0.9821          
                                          
       # 'Positive' Class : normal        
	   
# Predictions on validation set
predval_gbm_raw <- predict(modFit_gbm, val, type = "raw")
predval_gbm_prob <- predict(modFit_gbm, val, type = "prob")
confusionMatrix(predval_gbm_raw, val$class)
# write.csv(predval_gbm_raw, "C:/predval_gbm_raw.csv", row.names = FALSE)
# write.csv(predval_gbm_prob, "C:/predval_gbm_prob.csv", row.names = FALSE)

             # Accuracy : 0.7333          
                 # 95% CI : (0.6297, 0.8211)
    # No Information Rate : 0.5667          
    # P-Value [Acc > NIR] : 0.0008045       
                                          
                  # Kappa : 0.4366          
 # Mcnemar's Test P-Value : 0.0247447       
                                          
            # Sensitivity : 0.8824          
            # Specificity : 0.5385          
         # Pos Pred Value : 0.7143          
         # Neg Pred Value : 0.7778          
             # Prevalence : 0.5667          
         # Detection Rate : 0.5000          
   # Detection Prevalence : 0.7000          
      # Balanced Accuracy : 0.7104  
	  
# Predictions on testing set
pred_gbm_raw <- predict(modFit_gbm, testing, type = "raw")
pred_gbm_prob <- predict(modFit_gbm, testing, type = "prob")
# write.csv(pred_gbm_raw, "C:/pred_gbm_raw.csv", row.names = FALSE)
# write.csv(pred_gbm_prob, "C:/pred_gbm_prob.csv", row.names = FALSE)


# Neural Networks
# Pass 1
set.seed(1234)
modFit_nnet <- train(class ~ ., method = "nnet", data = tr, preProcess = c("center", "scale"), metric = "ROC", trControl = trainControl(method = "cv", number = 10, summaryFunction = twoClassSummary, classProbs = TRUE))
# size  decay  ROC        Sens       Spec       ROC SD      Sens SD     Spec SD
# 5     1e-04  0.8088675  0.8006410  0.6588889  0.10040518  0.10621399  0.1436397

# Pass 2
grid <- expand.grid(size = c(1, 2, 3, 4, 5, 6, 7, 8, 9), decay = c(0e+00, 1e-01, 1e-02, 1e-03, 1e-04, 1e-05, 1e-06))

set.seed(1234)
modFit_nnet <- train(class ~ ., method = "nnet", data = tr, preProcess = c("center", "scale"), metric = "ROC", trControl = trainControl(method = "cv", number = 10, summaryFunction = twoClassSummary, classProbs = TRUE), tuneGrid = grid)
# size  decay  ROC        Sens       Spec       ROC SD      Sens SD     Spec SD
# 2     1e-03  0.8275783  0.8012821  0.6811111  0.06496265  0.05921541  0.1106224

# Predictions on training set
predtr_nnet_raw <- predict(modFit_nnet, tr, type = "raw")
predtr_nnet_prob <- predict(modFit_nnet, tr, type = "prob")
# write.csv(predtr_nnet_raw, "C:/predtr_nnet_raw.csv", row.names = FALSE)
# write.csv(predtr_nnet_prob, "C:/predtr_nnet_prob.csv", row.names = FALSE)

confusionMatrix(predtr_nnet_raw, tr$class)

               # Accuracy : 0.9009          
                 # 95% CI : (0.8526, 0.9376)
    # No Information Rate : 0.5708          
    # P-Value [Acc > NIR] : <2e-16          
                                          
                  # Kappa : 0.7987          
 # Mcnemar's Test P-Value : 0.6625          
                                          
            # Sensitivity : 0.9008          
            # Specificity : 0.9011          
         # Pos Pred Value : 0.9237          
         # Neg Pred Value : 0.8723          
             # Prevalence : 0.5708          
         # Detection Rate : 0.5142          
   # Detection Prevalence : 0.5566          
      # Balanced Accuracy : 0.9010         

# Predictions on validation set
predval_nnet_raw <- predict(modFit_nnet, val, type = "raw")
predval_nnet_prob <- predict(modFit_nnet, val, type = "prob")
# write.csv(predval_nnet_raw, "C:/predval_nnet_raw.csv", row.names = FALSE)
# write.csv(predval_nnet_prob, "C:/predval_nnet_prob.csv", row.names = FALSE)

confusionMatrix(predval_nnet_raw, val$class)

               # Accuracy : 0.7889          
                 # 95% CI : (0.6901, 0.8679)
    # No Information Rate : 0.5667          
    # P-Value [Acc > NIR] : 8.314e-06       
                                          
                  # Kappa : 0.5714          
 # Mcnemar's Test P-Value : 1               
                                          
            # Sensitivity : 0.8039          
            # Specificity : 0.7692          
         # Pos Pred Value : 0.8200          
         # Neg Pred Value : 0.7500          
             # Prevalence : 0.5667          
         # Detection Rate : 0.4556          
   # Detection Prevalence : 0.5556          
      # Balanced Accuracy : 0.7866     
	  
# Prediction on testing set
pred_nnet_raw <- predict(modFit_nnet, testing, type = "raw")
pred_nnet_prob <- predict(modFit_nnet, testing, type = "prob")
# write.csv(pred_nnet_raw, "C:/pred_nnet_raw.csv", row.names = FALSE)
# write.csv(pred_nnet_prob, "C:/pred_nnet_prob.csv", row.names = FALSE)


# Support Vector Machines
# Pass 1
set.seed(1234)
modFit_svm <- train(class ~ ., method = "svmRadial", data = tr, preProcess = c("center", "scale"), metric = "ROC", trControl = trainControl(method = "cv", number = 10, summaryFunction = twoClassSummary, classProbs = TRUE), tuneLength = 20)

# C     ROC        Sens       Spec       ROC SD      Sens SD    Spec SD
# 4.00  0.8505698  0.8019231  0.6811111  0.08044041  0.1247560  0.06329975
# The final values used for the model were sigma = 0.005079309 and C = 4

grid <- expand.grid(C = c(3, 3.25, 3.5, 3.75, 4, 4.5, 5, 5.5, 6), sigma = c(0.005, 0.01, 0.015, 0.02))
set.seed(1234)
modFit_svm2 <- train(class ~ ., method = "svmRadial", data = tr, preProcess = c("center", "scale"), metric = "ROC", trControl = trainControl(method = "cv", number = 10, summaryFunction = twoClassSummary, classProbs = TRUE), tuneGrid = grid)

# C     ROC        Sens       Spec       ROC SD      Sens SD    Spec SD
# 3.50  0.005  0.8543447  0.8102564  0.7022222  0.07628087  0.1355853  0.13050498
# The final values used for the model were sigma = 0.005 and C = 3.5 

# Prediction on training set
predtr_svm_raw <- predict(modFit_svm2, tr, type = "raw")
predtr_svm_prob <- predict(modFit_svm2, val, type = "prob")
# write.csv(predtr_svm_raw, "C:/predtr_svm_raw.csv", row.names = FALSE)
# write.csv(predtr_svm_prob, "C:/predtr_svm_prob.csv", row.names = FALSE)

# Prediction on validation set
predval_svm_raw <- predict(modFit_svm2, val, type = "raw")
predval_svm_prob <- predict(modFit_svm2, val, type = "prob")
# write.csv(predval_svm_raw, "C:/predval_svm_raw.csv", row.names = FALSE)
# write.csv(predval_svm_prob, "C:/predval_svm_prob.csv", row.names = FALSE)
confusionMatrix(predval_svm_raw, val$class)

              # Accuracy : 0.7111         
                 # 95% CI : (0.606, 0.8018)
    # No Information Rate : 0.5667         
    # P-Value [Acc > NIR] : 0.00341        
                                         
                  # Kappa : 0.4046         
 # Mcnemar's Test P-Value : 0.55630        
                                         
            # Sensitivity : 0.7843         
            # Specificity : 0.6154         
         # Pos Pred Value : 0.7273         
         # Neg Pred Value : 0.6857         
             # Prevalence : 0.5667         
         # Detection Rate : 0.4444         
   # Detection Prevalence : 0.6111         
      # Balanced Accuracy : 0.6998  
	  
# Prediction on testing set
pred_svm_raw <- predict(modFit_svm, testing, type = "raw")
pred_svm_prob <- predict(modFit_svm, testing, type = "prob")
# write.csv(pred_svm_raw, "C:/pred_svm_raw.csv", row.names = FALSE)
# write.csv(pred_svm_prob, "C:/pred_svm_prob.csv", row.names = FALSE)


# Ensembling models
tr_ensemb_raw <- cbind(predtr_gbm_raw, predtr_nnet_raw, predtr_svm_raw, tr$class)
tr_ensemb_raw <- data.frame(tr_ensemb_raw)
tr_ensemb_prob <- cbind(predtr_gbm_prob[,1], predtr_nnet_prob[,1], predtr_svm_prob[,1], tr$class)
tr_ensemb_prob <- data.frame(tr_ensemb_prob)
tr_ensemb_prob[[1]] <- as.numeric(as.character(tr_ensemb_prob[[1]]))
tr_ensemb_prob[[2]] <- as.numeric(as.character(tr_ensemb_prob[[2]]))
tr_ensemb_prob[[3]] <- as.numeric(as.character(tr_ensemb_prob[[3]]))
names(tr_ensemb_prob) <- c("gbm", "nnet", "svm", "class")

set.seed(1234)
modFit_ensemb_glm <- train(class ~ ., method = "glm", data = tr_ensemb_prob, metric = "ROC", trControl = trainControl(method = "cv", number = 10, summaryFunction = twoClassSummary, classProbs = TRUE), tuneLength = 9)

# ROC        Sens       Spec  ROC SD      Sens SD     Spec SD
  # 0.9953704  0.9833333  1     0.01464017  0.03513642  0 
  
# Prediction on training set
predtr_ensemb <- predict(modFit_ensemb_glm, tr_ensemb_prob, type = "raw")
confusionMatrix(predtr_ensemb, tr_ensemb_prob$class)
              # Accuracy : 0.9906          
                 # 95% CI : (0.9663, 0.9989)
    # No Information Rate : 0.5708          
    # P-Value [Acc > NIR] : <2e-16          
                                          
                  # Kappa : 0.9807          
 # Mcnemar's Test P-Value : 1               
                                          
            # Sensitivity : 0.9917          
            # Specificity : 0.9890          
         # Pos Pred Value : 0.9917          
         # Neg Pred Value : 0.9890          
             # Prevalence : 0.5708          
         # Detection Rate : 0.5660          
   # Detection Prevalence : 0.5708          
      # Balanced Accuracy : 0.9904  
	  
# Prediction on validation set
val_ensemb_prob <- cbind(predval_gbm_prob[,1], predval_nnet_prob[,1], predval_svm_prob[,1], val$class)
val_ensemb_prob <- data.frame(val_ensemb_prob)
val_ensemb_prob[[1]] <- as.numeric(as.character(val_ensemb_prob[[1]]))
val_ensemb_prob[[2]] <- as.numeric(as.character(val_ensemb_prob[[2]]))
val_ensemb_prob[[3]] <- as.numeric(as.character(val_ensemb_prob[[3]]))
names(val_ensemb_prob) <- c("gbm", "nnet", "svm", "class")

predval_ensemb <- predict(modFit_ensemb_glm, val_ensemb_prob, type = "raw")
confusionMatrix(predval_ensemb, val$class)

              # Accuracy : 0.8111          
                 # 95% CI : (0.7149, 0.8859)
    # No Information Rate : 0.5667          
    # P-Value [Acc > NIR] : 8.698e-07       
                                          
                  # Kappa : 0.6047          
 # Mcnemar's Test P-Value : 0.05235         
                                          
            # Sensitivity : 0.9216          
            # Specificity : 0.6667          
         # Pos Pred Value : 0.7833          
         # Neg Pred Value : 0.8667          
             # Prevalence : 0.5667          
         # Detection Rate : 0.5222          
   # Detection Prevalence : 0.6667          
      # Balanced Accuracy : 0.7941
	  
library(pROC)
actual <- as.character(val$class)
actual[actual == "normal"] <- 0
actual[actual == "patient"] <- 1
actual <- as.numeric(actual)
predicted <- as.character(predval_ensemb)
predicted[predicted == "normal"] <- 0
predicted[predicted == "patient"] <- 1
predicted <- as.numeric(predicted)
r <- roc(actual, predicted)	
auc(r)
# Area under the curve: 0.7941

# Prediction on testing set
testing_ensemb_prob <- cbind(pred_gbm_prob[,1], pred_nnet_prob[,1], pred_svm_prob[,1], testing$class)
testing_ensemb_prob <- data.frame(testing_ensemb_prob)
testing_ensemb_prob[[1]] <- as.numeric(as.character(testing_ensemb_prob[[1]]))
testing_ensemb_prob[[2]] <- as.numeric(as.character(testing_ensemb_prob[[2]]))
testing_ensemb_prob[[3]] <- as.numeric(as.character(testing_ensemb_prob[[3]]))
names(testing_ensemb_prob) <- c("gbm", "nnet", "svm", "class")

pred_ensemb_glm <- predict(modFit_ensemb_glm, testing_ensemb_prob, type = "raw")
confusionMatrix(pred_ensemb_glm, testing$class)

               # Accuracy : 0.7969          
                 # 95% CI : (0.7167, 0.8628)
    # No Information Rate : 0.5703          
    # P-Value [Acc > NIR] : 5.694e-08       
                                          
                  # Kappa : 0.5741          
 # Mcnemar's Test P-Value : 0.03098         
                                          
            # Sensitivity : 0.9041          
            # Specificity : 0.6545          
         # Pos Pred Value : 0.7765          
         # Neg Pred Value : 0.8372          
             # Prevalence : 0.5703          
         # Detection Rate : 0.5156          
   # Detection Prevalence : 0.6641          
      # Balanced Accuracy : 0.7793  


  