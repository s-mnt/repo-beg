# Read the data
complete <- read.table("C:/complete.txt", sep = ",")
dim(complete)
# [1] 690  16

# Correcting classes of columns
complete$V2 <- as.numeric(as.character(complete$V2))
complete$V14 <- as.numeric(as.character(complete$V14))

names(complete)[length(names(complete))] <- "class"

# Removing near zero variance variables
library(caret)
nsv <- nearZeroVar(complete, saveMetrics = TRUE)
# no zero variance predictor

# Mssing data
missing <- NULL
for (i in 1:ncol(complete)) {
		complete[, i][complete[,i] == "?"] <- NA
		missing[i] <- sum(is.na(complete[, i]))
}
complete <- complete[complete.cases(complete), ]
dim(complete)
# [1] 653  16
# write.csv(complete, "C:/complete.csv", row.names = FALSE)

# Data splitting into training and test sets
set.seed(1234)
ind <- createDataPartition(y = complete$class, p = 0.7, list = FALSE)
training <- complete[ind, ]
testing <- complete[-ind, ]

# Sub-training and validation set
set.seed(1234)
ind <- createDataPartition(y = training$class, p = 0.7, list = FALSE)
tr <- training[ind, ]
val <- training[-ind, ]

# write.csv(training, "C:/training.csv", row.names = FALSE)
# write.csv(testing, "C:/testing.csv", row.names = FALSE)
# write.csv(tr, "C:/tr.csv", row.names = FALSE)
# write.csv(val, "C:/val.csv", row.names = FALSE)

# Centering and scaling continous predictors in sub-training set
sub_tr <- NULL
pos <- NULL
j = 1
for (i in 1:(ncol(tr)-1)) {
		if (class(tr[[i]]) == "numeric") {
				pos[j] <- i
				j = j + 1
		}	
}
sub_tr <- tr[, pos]

preObj <- preProcess(sub_tr, method = c("center", "scale"))
sub_tr <- predict(preObj, sub_tr)
tr2 <- cbind(tr[, -pos], sub_tr)

# Centering and scaling continous predictors in validation set
sub_val <- val[, pos]
sub_val <- predict(preObj, sub_val)
val2 <- cbind(val[, -pos], sub_val)

# Centering and scaling continous predictors in testing set
sub_testing <- testing[, pos]
sub_testing <- predict(preObj, sub_testing)
testing2 <- cbind(testing[, -pos], sub_testing)

# write.csv(tr2, "C:/tr2.csv", row.names = FALSE)
# write.csv(val2, "C:/val2.csv", row.names = FALSE)
# write.csv(testing2, "C:/testing2.csv", row.names = FALSE)

# Bagged Adaboost Model
set.seed(1234)
modFit_bag_adaboost <- train(class ~ ., method = "AdaBag", data = tr2, metric = "Accuracy", trControl = trainControl(method = "cv", number = 10))
modFit_bag_adaboost
# maxdepth  mfinal  Accuracy   Kappa      Accuracy SD  Kappa SD  
# 3          50     0.8749633  0.7498399  0.04795267   0.09525355

# Prediction on training set
predtr2_ada <- predict(modFit_bag_adaboost, tr2)
# write.csv(predtr2_ada, "C:/predtr2_ada.csv", row.names = FALSE)
confusionMatrix(predtr2_ada, tr2$class)
               # Accuracy : 0.9034          
                 # 95% CI : (0.8657, 0.9334)
    # No Information Rate : 0.5452          
    # P-Value [Acc > NIR] : <2e-16          
                                          
                  # Kappa : 0.8063          
 # Mcnemar's Test P-Value : 0.1508          
                                          
            # Sensitivity : 0.8857          
            # Specificity : 0.9247          
         # Pos Pred Value : 0.9337          
         # Neg Pred Value : 0.8710          
             # Prevalence : 0.5452          
         # Detection Rate : 0.4829          
   # Detection Prevalence : 0.5171          
      # Balanced Accuracy : 0.9052     

# Prediction on validation set
predval2_ada <- predict(modFit_bag_adaboost, val2)
# write.csv(predval2_ada, "C:/predval2_ada.csv", row.names = FALSE)
confusionMatrix(predval2_ada, val2$class)
               # Accuracy : 0.8832          
                 # 95% CI : (0.8173, 0.9318)
    # No Information Rate : 0.5474          
    # P-Value [Acc > NIR] : <2e-16          
                                          
                  # Kappa : 0.7663          
 # Mcnemar's Test P-Value : 0.2113          
                                          
            # Sensitivity : 0.8533          
            # Specificity : 0.9194          
         # Pos Pred Value : 0.9275          
         # Neg Pred Value : 0.8382          
             # Prevalence : 0.5474          
         # Detection Rate : 0.4672          
   # Detection Prevalence : 0.5036          
      # Balanced Accuracy : 0.8863      
	  
# Prediction on testing set
pred_ada <- predict(modFit_bag_adaboost, testing2)
# write.csv(pred_ada, "C:/pred_ada.csv", row.names = FALSE)
confusionMatrix(pred_ada, testing2$class)
               # Accuracy : 0.8667         
                 # 95% CI : (0.8108, 0.911)
    # No Information Rate : 0.5487         
    # P-Value [Acc > NIR] : < 2e-16        
                                         
                  # Kappa : 0.7335         
 # Mcnemar's Test P-Value : 0.07756        
                                         
            # Sensitivity : 0.8318         
            # Specificity : 0.9091         
         # Pos Pred Value : 0.9175         
         # Neg Pred Value : 0.8163         
             # Prevalence : 0.5487         
         # Detection Rate : 0.4564         
   # Detection Prevalence : 0.4974         
      # Balanced Accuracy : 0.8704     