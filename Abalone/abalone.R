# Read the file
complete <- read.table("C:/data.txt", sep = ",")
names(complete) <- c("Sex", "Length", "Diameter", "Height", "Wholeweight", "Shuckedweight", "Visceraweight", "Shellweight", "Rings")

# exploratory data analysis
library(psych)
describe(complete[-1], skew = FALSE)
plot(complete[,-1])

# Zero Variance variables
library(caret)
nsv <- nearZeroVar(complete[,-1], saveMetrics = TRUE)
# No zero variance variable

# Missing data
sum(is.na(complete))
# 0

# Split into training and test set
library(caret)
set.seed(1234)
ind <- createDataPartition(y = complete$Rings, p = 0.7, list = FALSE)
training <- complete[ind, ]
# write.csv(training, "C:/training.csv", row.names = FALSE)
testing <- complete[-ind, ]
# write.csv(testing, "C:/testing.csv", row.names = FALSE)

# Split training into sub-training and validation set
set.seed(1234)
ind <- createDataPartition(y = training$Rings, p = 0.7, list = FALSE)
tr <- training[ind, ]
# write.csv(tr, "C:/tr.csv", row.names = FALSE)
val <- training[-ind, ]
# write.csv(val, "C:/val.csv", row.names = FALSE)

# Train Linear fit model
set.seed(1234)
modFit_lm <- train(Rings ~ ., method = "lm", data = tr, metric = "RMSE", trControl = trainControl(method = "cv", number = 10) )
modFit_lm$finalModel
summary(modFit_lm)

# Predictions on training set
predtr_lm <- predict(modFit_lm, tr)
# write.csv(predtr_lm, "C:/predtr_lm.csv", row.names = FALSE)
RMSE(predtr_lm, tr$Rings)
# 2.182999

# Predictions on validation set
predval_lm <- predict(modFit_lm, val)
# write.csv(predval_lm, "C:/predval_lm.csv", row.names = FALSE)
RMSE(predval_lm, val$Rings)
# 2.112335

# Predictions on testing set
pred_lm <- predict(modFit_lm, testing)
# write.csv(pred_lm, "C:/pred_lm.csv", row.names = FALSE)
RMSE(pred_lm, testing$Rings)
# 2.268784


# Train Gradient Boosting Model
set.seed(1234)
modFit_gbm <- train(Rings ~ ., method = "gbm", data = tr, metric = "RMSE", trControl = trainControl(method = "cv", number = 10) )
modFit_gbm$finalModel
summary(modFit_gbm)
                        # var    rel.inf
# Shellweight     Shellweight 56.2654177
# Shuckedweight Shuckedweight 19.4810696
# Wholeweight     Wholeweight  6.7214579
# Height               Height  5.3266887
# Visceraweight Visceraweight  4.1980313
# SexI                   SexI  3.4597432
# Diameter           Diameter  2.2891308
# Length               Length  2.1498940
# SexM                   SexM  0.1085669

# Predictions on training set
predtr_gbm <- predict(modFit_gbm, tr)
# write.csv(predtr_gbm, "C:/predtr_gbm.csv", row.names = FALSE)
RMSE(predtr_gbm, tr$Rings)
# 1.919456

# Predictions on validation set
predval_gbm <- predict(modFit_gbm, val)
# write.csv(predval_gbm, "C:/predval_gbm.csv", row.names = FALSE)
RMSE(predval_gbm, val$Rings)
# 2.134467

# Predictions on testing set
pred_gbm <- predict(modFit_gbm, testing)
# write.csv(pred_gbm, "C:/pred_gbm.csv", row.names = FALSE)
RMSE(pred_gbm, testing$Rings)
# 2.2218


# Ensembling linear fit and gradient boosting models
library(caret)
library(mlbench)
library(caretEnsemble)
set.seed(1234)
model_list <- caretList(Rings ~ ., data = tr, trControl = trainControl(method = "cv", number = 10), methodList = c("lm", "gbm") )

# Predictions on training set
predtr_ensemb <- predict(model_list, tr)
# write.csv(predtr_ensemb, "C:/predtr_ensemb.csv", row.names = FALSE)
RMSE(predtr_ensemb, tr$Rings)
# 2.083385

# Prediction on validation set
predval_ensemb <- predict(model_list, val)
# write.csv(predval_ensemb, "C:/predval_ensemb.csv", row.names = FALSE)
RMSE(predval_ensemb, val$Rings)
# 2.12623

# Prediction on testing set
pred_ensemb <- predict(model_list, testing)
# write.csv(pred_ensemb, "C:/pred_ensemb.csv", row.names = FALSE)
RMSE(pred_ensemb, testing$Rings)
# 2.251405