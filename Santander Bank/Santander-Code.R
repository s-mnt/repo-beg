# 1st column is Customer ID, so not important
# 1 = Unsatisfied; 			0 = satisfied
# centering and scaling transformation - leave "ID" and "TARGET"

# Load the training and test set
training <- read.csv("C:/train.csv")
dim(training)
# 76020   371

testing <- read.csv("C:/test.csv")
dim(testing)
# 75818   370

table(training$TARGET)
# 0 - 73012			1 - 3008

training_ID <- training[, c("ID", "TARGET")]
testing_ID <- testing[, "ID"]					# vector


training <- training[, !(names(training) == "ID" | names(training) == "TARGET")]
testing <- testing[, !(names(testing) == "ID")]

# Merging both training and test sets
complete <- rbind(training, testing)

# Cleaning variable names
names(complete) <- gsub("_", "", names(complete))
complete <- complete[, order(names(complete))]

# Outliers
deltagroupind <- grep("delta", names(complete))
summary(complete[, deltagroupind])
table(complete[, deltagroupind[1]])

# Removing weird outliers
for (i in 1:length(deltagroupind)) {
# Replacing outliers in dataset with NA
		complete[deltagroupind[i]][complete[deltagroupind[i]] == 9999999999] <- NA
}

summary(complete[, deltagroupind])

impgroupind <- grep("^imp", names(complete))
summary(complete[, impgroupind])
table(complete[, impgroupind[1]])

indgroupind <- grep("^ind", names(complete))
summary(complete[, indgroupind])

numgroupind <- grep("^num", names(complete))
summary(complete[, numgroupind])

salgroupind <- grep("^sal", names(complete))
summary(complete[, salgroupind])

vargroupind <- grep("^var", names(complete))
summary(complete[, vargroupind])
# Removing weird outliers
for (i in 1:length(vargroupind)) {
# Replacing outliers in dataset with NA
		complete[vargroupind[i]][complete[vargroupind[i]] == -999999] <- NA
}
summary(complete[, vargroupind])

# Removing near zero variance variables
library(caret)
nsv <- nearZeroVar(complete, freqCut = 95/5, uniqueCut = 10, saveMetrics = TRUE)
complete <- complete[, !(nsv$nzv)]

# Missing data
missing <- rep(0, times = ncol(complete))
for (i in 1:ncol(complete)) {
		# Total count of missing values in every column
		missing[i] <- sum(is.na(complete[i]))
}

training <- complete[1:nrow(training_ID), ]
testing <- complete[(nrow(training_ID)+1):nrow(complete), ]
training <- cbind(training_ID, training)
testing <- cbind(testing_ID, testing)
names(testing)[1] <- "ID"
names(testing_ID)[1] <- "ID"

# write.csv(training, "C:/training.csv", row.names = FALSE)
# write.csv(testing, "C:/testing.csv", row.names = FALSE)
# write.csv(training_ID, "C:/training_ID.csv", row.names = FALSE)
# write.csv(testing_ID, "C:/testing_ID.csv", row.names = FALSE)
# write.csv(complete, "C:/complete.csv", row.names = FALSE)

# Prediction models
# Creating sub-training and validation set
# training <- read.csv("C:/training.csv")
training$TARGET[training$TARGET == 0] <- "satisfied"
training$TARGET[training$TARGET == 1] <- "unsatisfied"
set.seed(1234)
ind <- createDataPartition(y = training$TARGET, p = 0.7, list = FALSE)
tr <- training[ind, ]							# sub-training set
# write.csv(tr, "C:/tr.csv", row.names = FALSE)					
cv <- training[-ind, ]							# validation set for testing training set
# write.csv(cv, "C:/cv.csv", row.names = FALSE)	


# Gradient Boosting Model with class weights
# Assigning weights to classes in target 
count <- mean((sum(tr$TARGET == "satisfied"))*100/nrow(tr), (sum(cv$TARGET == "satisfied"))*100/nrow(cv))
tr$weights <- count
tr$weights[tr$TARGET == "satisfied"] <- 100 - count

# Training Gradient Boosting Model on training set with 10 fold cross-validation
library(caret)
set.seed(1234)
modFit_gbm_weights <- train(TARGET ~ ., method = "gbm", data = tr[, -c(1,ncol(tr))], weights = tr$weights, metric = "ROC", trControl = trainControl(method = "cv", number = 10, summaryFunction = twoClassSummary, classProbs = TRUE))
modFit_gbm_weights
modFit_gbm_weights$finalModel

tr <- tr[, -ncol(tr)]
# Predicting on training set
predtr_gbm_prob_weights <- predict(modFit_gbm_weights, tr, type = "prob")
predtr_gbm_raw_weights <- predict(modFit_gbm_weights, tr, type = "raw")
# write.csv(predtr_gbm_prob_weights, "C:/predtr_gbm_prob_weights.csv", row.names = FALSE)
# write.csv(predtr_gbm_raw_weights, "C:/predtr_gbm_raw_weights.csv", row.names = FALSE)
confusionMatrix(predtr_gbm_raw_weights, tr$TARGET)
              # Accuracy : 0.7763          
                 # 95% CI : (0.7727, 0.7798)
    # No Information Rate : 0.9604          
    # P-Value [Acc > NIR] : 1               
                                          
                  # Kappa : 0.1569          
 # Mcnemar's Test P-Value : <2e-16          
                                          
            # Sensitivity : 0.7765          
            # Specificity : 0.7711          
         # Pos Pred Value : 0.9880          
         # Neg Pred Value : 0.1245          
             # Prevalence : 0.9604          
         # Detection Rate : 0.7458          
   # Detection Prevalence : 0.7548          
      # Balanced Accuracy : 0.7738   

# Predicting on validation set
predcv_gbm_prob_weights <- predict(modFit_gbm_weights, cv, type = "prob")
predcv_gbm_raw_weights <- predict(modFit_gbm_weights, cv, type = "raw")
# write.csv(predcv_gbm_prob_weights, "C:/predcv_gbm_prob_weights.csv", row.names = FALSE)
# write.csv(predcv_gbm_raw_weights, "C:/predcv_gbm_raw_weights.csv", row.names = FALSE)
confusionMatrix(predcv_gbm_raw_weights, cv$TARGET)
               # Accuracy : 0.7728          
                 # 95% CI : (0.7673, 0.7782)
    # No Information Rate : 0.9604          
    # P-Value [Acc > NIR] : 1               
                                          
                  # Kappa : 0.1458          
 # Mcnemar's Test P-Value : <2e-16          
                                          
            # Sensitivity : 0.7743          
            # Specificity : 0.7361          
         # Pos Pred Value : 0.9862          
         # Neg Pred Value : 0.1184          
             # Prevalence : 0.9604          
         # Detection Rate : 0.7437          
   # Detection Prevalence : 0.7541          
      # Balanced Accuracy : 0.7552    

# Predicting outcome for testing set
pred_gbm_prob_weights <- predict(modFit_gbm_weights, testing, type = "prob")
pred_gbm_raw_weights <- predict(modFit_gbm_weights, testing, type = "raw")
# write.csv(pred_gbm_prob_weights, "C:/pred_gbm_prob_weights.csv", row.names = FALSE)
# write.csv(pred_gbm_raw_weights, "C:/pred_gbm_raw_weights.csv", row.names = FALSE)



# Gradient Boosting Model with downSample
tr$TARGET <- as.factor(tr$TARGET)
set.seed(1000)
down_train <- downSample(x = tr[, -2], y = tr$TARGET)
names(down_train)[length(names(down_train))] <- "TARGET"
table(down_train$TARGET)

set.seed(1234)
modFit_gbm_downsample <- train(TARGET ~ ., method = "gbm", data = down_train[, -1], metric = "ROC", trControl = trainControl(method = "cv", number = 10, summaryFunction = twoClassSummary, classProbs = TRUE))
modFit_gbm_downsample
modFit_gbm_downsample$finalModel

# Predicting on training set
preddt_gbm_prob_downsample <- predict(modFit_gbm_downsample, down_train, type = "prob")
preddt_gbm_raw_downsample <- predict(modFit_gbm_downsample, down_train, type = "raw")
# write.csv(preddt_gbm_prob_downsample, "C:/preddt_gbm_prob_downsample.csv", row.names = FALSE)
# write.csv(preddt_gbm_raw_downsample, "C:/preddt_gbm_raw_downsample.csv", row.names = FALSE)
confusionMatrix(preddt_gbm_raw_downsample, down_train$TARGET)
               # Accuracy : 0.7761          
                 # 95% CI : (0.7632, 0.7886)
    # No Information Rate : 0.5             
    # P-Value [Acc > NIR] : <2e-16          
                                          
                  # Kappa : 0.5522          
 # Mcnemar's Test P-Value : 0.0268          
                                          
            # Sensitivity : 0.7925          
            # Specificity : 0.7597          
         # Pos Pred Value : 0.7674          
         # Neg Pred Value : 0.7855          
             # Prevalence : 0.5000          
         # Detection Rate : 0.3962          
   # Detection Prevalence : 0.5164          
      # Balanced Accuracy : 0.7761   

# Predicting on 'tr' training set
predtr_gbm_prob_downsample <- predict(modFit_gbm_downsample, tr, type = "prob")
predtr_gbm_raw_downsample <- predict(modFit_gbm_downsample, tr, type = "raw")
# write.csv(predtr_gbm_prob_downsample, "C:/predtr_gbm_prob_downsample.csv", row.names = FALSE)
# write.csv(predtr_gbm_raw_downsample, "C:/predtr_gbm_raw_downsample.csv", row.names = FALSE)
confusionMatrix(predtr_gbm_raw_downsample, tr$TARGET)
               # Accuracy : 0.7759          
                 # 95% CI : (0.7724, 0.7795)
    # No Information Rate : 0.9604          
    # P-Value [Acc > NIR] : 1               
                                          
                  # Kappa : 0.1539          
 # Mcnemar's Test P-Value : <2e-16          
                                          
            # Sensitivity : 0.7766          
            # Specificity : 0.7597          
         # Pos Pred Value : 0.9874          
         # Neg Pred Value : 0.1229          
             # Prevalence : 0.9604          
         # Detection Rate : 0.7459          
   # Detection Prevalence : 0.7554          
      # Balanced Accuracy : 0.7682       

# Predicting on validation set
predcv_gbm_prob_downsample <- predict(modFit_gbm_downsample, cv, type = "prob")
predcv_gbm_raw_downsample <- predict(modFit_gbm_downsample, cv, type = "raw")
# write.csv(predcv_gbm_prob_downsample, "C:/predcv_gbm_prob_downsample.csv", row.names = FALSE)
# write.csv(predcv_gbm_raw_downsample, "C:/predcv_gbm_raw_downsample.csv", row.names = FALSE)
confusionMatrix(predcv_gbm_raw_downsample, cv$TARGET)
               # Accuracy : 0.7734          
                 # 95% CI : (0.7679, 0.7788)
    # No Information Rate : 0.9604          
    # P-Value [Acc > NIR] : 1               
                                          
                  # Kappa : 0.1445          
 # Mcnemar's Test P-Value : <2e-16          
                                          
            # Sensitivity : 0.7752          
            # Specificity : 0.7284          
         # Pos Pred Value : 0.9858          
         # Neg Pred Value : 0.1177          
             # Prevalence : 0.9604          
         # Detection Rate : 0.7446          
   # Detection Prevalence : 0.7553          
      # Balanced Accuracy : 0.7518   

# Predicting outcome for testing set
pred_gbm_prob_downsample <- predict(modFit_gbm_downsample, testing, type = "prob")
pred_gbm_raw_downsample <- predict(modFit_gbm_downsample, testing, type = "raw")
# write.csv(pred_gbm_prob_downsample, "C:/pred_gbm_prob_downsample.csv", row.names = FALSE)
# write.csv(pred_gbm_raw_downsample, "C:/pred_gbm_raw_downsample.csv", row.names = FALSE)


# Gradient Boosting Model with upSample
tr$TARGET <- as.factor(tr$TARGET)
set.seed(1000)
up_train <- upSample(x = tr[, -2], y = tr$TARGET)
names(up_train)[length(names(up_train))] <- "TARGET"
table(up_train$TARGET)

set.seed(1234)
modFit_gbm_upsample <- train(TARGET ~ ., method = "gbm", data = up_train[, -1], metric = "ROC", trControl = trainControl(method = "cv", number = 10, summaryFunction = twoClassSummary, classProbs = TRUE))
modFit_gbm_upsample
modFit_gbm_upsample$finalModel

# Predicting on training set
predut_gbm_prob_upsample <- predict(modFit_gbm_upsample, up_train, type = "prob")
predut_gbm_raw_upsample <- predict(modFit_gbm_downsample, down_train, type = "raw")
# write.csv(preddt_gbm_prob_downsample, "C:/preddt_gbm_prob_downsample.csv", row.names = FALSE)
# write.csv(preddt_gbm_raw_downsample, "C:/preddt_gbm_raw_downsample.csv", row.names = FALSE)
confusionMatrix(preddt_gbm_raw_downsample, down_train$TARGET)
               # Accuracy : 0.7761          
                 # 95% CI : (0.7632, 0.7886)
    # No Information Rate : 0.5             
    # P-Value [Acc > NIR] : <2e-16          
                                          
                  # Kappa : 0.5522          
 # Mcnemar's Test P-Value : 0.0268          
                                          
            # Sensitivity : 0.7925          
            # Specificity : 0.7597          
         # Pos Pred Value : 0.7674          
         # Neg Pred Value : 0.7855          
             # Prevalence : 0.5000          
         # Detection Rate : 0.3962          
   # Detection Prevalence : 0.5164          
      # Balanced Accuracy : 0.7761   

# Predicting on 'tr' training set
predtr_gbm_prob_downsample <- predict(modFit_gbm_downsample, tr, type = "prob")
predtr_gbm_raw_downsample <- predict(modFit_gbm_downsample, tr, type = "raw")
# write.csv(predtr_gbm_prob_downsample, "C:/predtr_gbm_prob_downsample.csv", row.names = FALSE)
# write.csv(predtr_gbm_raw_downsample, "C:/predtr_gbm_raw_downsample.csv", row.names = FALSE)
confusionMatrix(predtr_gbm_raw_downsample, tr$TARGET)
               # Accuracy : 0.7759          
                 # 95% CI : (0.7724, 0.7795)
    # No Information Rate : 0.9604          
    # P-Value [Acc > NIR] : 1               
                                          
                  # Kappa : 0.1539          
 # Mcnemar's Test P-Value : <2e-16          
                                          
            # Sensitivity : 0.7766          
            # Specificity : 0.7597          
         # Pos Pred Value : 0.9874          
         # Neg Pred Value : 0.1229          
             # Prevalence : 0.9604          
         # Detection Rate : 0.7459          
   # Detection Prevalence : 0.7554          
      # Balanced Accuracy : 0.7682       

# Predicting on validation set
predcv_gbm_prob_downsample <- predict(modFit_gbm_downsample, cv, type = "prob")
predcv_gbm_raw_downsample <- predict(modFit_gbm_downsample, cv, type = "raw")
# write.csv(predcv_gbm_prob_downsample, "C:/predcv_gbm_prob_downsample.csv", row.names = FALSE)
# write.csv(predcv_gbm_raw_downsample, "C:/predcv_gbm_raw_downsample.csv", row.names = FALSE)
confusionMatrix(predcv_gbm_raw_downsample, cv$TARGET)
               # Accuracy : 0.7734          
                 # 95% CI : (0.7679, 0.7788)
    # No Information Rate : 0.9604          
    # P-Value [Acc > NIR] : 1               
                                          
                  # Kappa : 0.1445          
 # Mcnemar's Test P-Value : <2e-16          
                                          
            # Sensitivity : 0.7752          
            # Specificity : 0.7284          
         # Pos Pred Value : 0.9858          
         # Neg Pred Value : 0.1177          
             # Prevalence : 0.9604          
         # Detection Rate : 0.7446          
   # Detection Prevalence : 0.7553          
      # Balanced Accuracy : 0.7518   

# Predicting outcome for testing set
pred_gbm_prob_downsample <- predict(modFit_gbm_downsample, testing, type = "prob")
pred_gbm_raw_downsample <- predict(modFit_gbm_downsample, testing, type = "raw")
# write.csv(pred_gbm_prob_downsample, "C:/pred_gbm_prob_downsample.csv", row.names = FALSE)
# write.csv(pred_gbm_raw_downsample, "C:/pred_gbm_raw_downsample.csv", row.names = FALSE)



