# Read the data
complete <- read.table("C:/complete.txt")

# Correcting variable classes
complete$V2 <- factor(complete$V2)
complete$V7 <- factor(complete$V7)
complete$V8 <- factor(complete$V8)
complete$V4 <- as.numeric(as.character(complete$V4))

# Correcting variable names
names(complete) <- c("mpg", "cylinders", "displacement", "horsepower", "weight", "acceleration", "modelyear", "origin", "carname")

# Near zero variance variables
library(caret)
nsv <- nearZeroVar(complete[,-1], saveMetrics = TRUE)
# no zero variance variable

# Removing missing data
complete <- complete[complete.cases(complete), ]
# write.csv(complete, "C:/complete.csv", row.names = FALSE)

# Data splitting into training and test set
set.seed(1234)
ind <- createDataPartition(y = complete$mpg, p = 0.7, list = FALSE)
training <- complete[ind, ]
testing <- complete[-ind, ]
# write.csv(training, "C:/training.csv", row.names = FALSE)
# write.csv(testing, "C:/testing.csv", row.names = FALSE)

# Sub-training and validation set
set.seed(1234)
ind <- createDataPartition(y = training$mpg, p = 0.7, list = FALSE)
tr <- training[ind, ]
val <- training[-ind, ]
# write.csv(tr, "C:/tr.csv", row.names = FALSE)
# write.csv(val, "C:/val.csv", row.names = FALSE)

# Centering and scaling continous predictors in sub-training set
sub_tr <- NULL
pos <- NULL
j = 1
for (i in 2:ncol(tr)) {
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

# Bagged Regression Splines
# Pass 1
set.seed(1234)
modFit_bagEarth <- train(mpg ~ ., method = "bagEarth", data = tr2, metric = "RMSE", trControl = trainControl(method = "cv", number = 10))
modFit_bagEarth
# nprune  RMSE      Rsquared   RMSE SD    Rsquared SD
# 31      2.675929  0.8913437  0.5211322  0.03655647 
# The final values used for the model were nprune = 31 and degree = 1

# Pass 2
set.seed(1234)
modFit_bagEarth2 <- train(mpg ~ ., method = "bagEarth", data = tr2, metric = "RMSE", trControl = trainControl(method = "cv", number = 10), tuneLength = 9)
# nprune  RMSE      Rsquared   RMSE SD    Rsquared SD
# 20      2.649175  0.8925316  0.4658696  0.03461550 
# The final values used for the model were nprune = 20 and degree = 1

# Prediction on sub-training set
predtr2_bagEarth <- predict(modFit_bagEarth2, tr2)
mean(tr2$mpg)
# [1] 23.64639
RMSE(predtr2_bagEarth, tr2$mpg)
# [1] 1.731186
# write.csv(predtr2_bagEarth, "C:/predtr2_bagEarth.csv", row.names = FALSE)

# Prediction on validation set
predval2_bagEarth <- predict(modFit_bagEarth2, val2)
mean(val2$mpg)
# [1] 23.64634
RMSE(predval2_bagEarth, val2$mpg)
# [1] 2.636828
# write.csv(predval2_bagEarth, "C:/predval2_bagEarth.csv", row.names = FALSE)

# Prediction on testing set
pred_bagEarth <- predict(modFit_bagEarth2, testing2)
mean(testing2$mpg)
# [1] 22.96897
RMSE(pred_bagEarth, testing2$mpg)
# [1] 3.418738
# write.csv(pred_bagEarth, "C:/pred_bagEarth.csv", row.names = FALSE)
