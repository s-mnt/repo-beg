# Load the datasets
training2hour <- read.csv("C:/Training set - Microclimate - 2 hour intervals.csv")
training5min <- read.csv("C:/Training set - Microclimate - 5 minute intervals.csv")			# not useful
testing2hour <- read.csv("C:/Test set - Microclimate - 2 hour intervals.csv")
testing5min <- read.csv("C:/Test set - Microclimate - 5 min intervals.csv")
target <- read.csv("C:/Target Variable Water Yield.csv")
climateagadir <- read.csv("C:/Macroclimate - Agadir Airport.csv")
climateguelmim <- read.csv("C:/Macroclimate - Guelmim Airport.csv")
climatesidi <- read.csv("C:/Macroclimate - Sidi Ifni Weather Station.csv")

dim(training2hour)
# [1] 5802   10

dim(testing2hour)
# [1] 1110   10

dim(testing5min)
# [1] 26619    10

# Append above 3 datasets
complete <- rbind(training2hour, testing2hour, testing5min)
dim(complete)
# [1] 33531    10

# Merge with climate data
names(climateagadir) <- paste("agadir", names(climateagadir))
names(climateagadir)[1] <- "X"
names(climateguelmim) <- paste("guelmim", names(climateguelmim))
names(climateguelmim)[1] <- "X"
names(climatesidi) <- paste("sidi", names(climatesidi))
names(climatesidi)[1] <- "X"

library(plyr)
dfList <- list(complete, climateagadir, climateguelmim, climatesidi)
complete <- join_all(dfList)

dim(complete)
# [1] 33531    60

summary(complete)

# Replacing blank spaces with NA
for (i in 1:ncol(complete)) {
		complete[[i]][complete[[i]] == ""] <- NA
}

# Missing data
missing <- function(x) {
		sum(is.na(x))
}
missing <- apply(complete, 2, missing)
# Removing columns with missing values where more than 60% of values are missing
complete <- complete[, !(missing > (0.6 * nrow(complete)))]

# Removing near zero variance variables
library(caret)
nsv <- nearZeroVar(complete, freqCut = 95/5, uniqueCut = 10, saveMetrics = TRUE)
complete <- complete[, !(nsv$nzv)]

# write.csv(complete, "C:/complete.csv", row.names = FALSE)

# Extracting training and testing sets
training <- complete[1:nrow(training2hour),]
testing2hour <- complete[(nrow(training2hour)+1):(nrow(training2hour)+nrow(testing2hour)), ]
testing5min <- complete[(nrow(training2hour)+nrow(testing2hour)+1):(nrow(training2hour)+nrow(testing2hour)+nrow(testing5min)), ]

# write.csv(testing2hour, "C:/testing2hour.csv", row.names = FALSE)
# write.csv(testing5min, "C:/testing5min.csv", row.names = FALSE)

# Merging with target
training <- merge(training, target)
training <- training[complete.cases(training), ]
# write.csv(training, "C:/training.csv", row.names = FALSE)

# Prediction models
# Creating sub-training and validation set
# training <- read.csv("C:/training.csv")
set.seed(1234)
ind <- createDataPartition(y = training$yield, p = 0.7, list = FALSE)
tr <- training[ind, ]							# sub-training set
# write.csv(tr, "C:/tr.csv", row.names = FALSE)					
cv <- training[-ind, ]							# validation set for testing training set
# write.csv(cv, "C:/cv.csv", row.names = FALSE)	


# yield
library(caret)
set.seed(1234)
modFit_gbm_yield <- train(yield ~ ., method = "gbm", data = tr[, -1], preProcess = c("center", "scale"), metric = "RMSE", trControl = trainControl(method = "cv", number = 10))

set.seed(1234)
modFit_rf_yield <- train(yield ~ ., method = "rf", data = tr[, -1], preProcess = c("center", "scale"), metric = "RMSE", trControl = trainControl(method = "cv", number = 10))

set.seed(1234)
modFit_svm_yield <- train(yield ~ ., method = "svmRadial", data = tr[, -1], preProcess = c("center", "scale"), metric = "RMSE", trControl = trainControl(method = "cv", number = 10))


library(lubridate)
tr2 <- tr
tr2 <- tr2[, -ncol(tr2)]
tr2$X <- ymd_hms(tr2$X)
tr2$month <- month(tr2$X)
tr2$hour <- hour(tr2$X)

cv2 <- cv
cv2 <- cv2[, -ncol(cv2)]
cv2$X <- ymd_hms(cv2$X)
cv2$month <- month(cv2$X)
cv2$hour <- hour(cv2$X)

# temp
library(caret)
set.seed(1234)
predictors <- names(tr2) %in% c("temp", "month", "hour")
modFit_gbm_temp <- train(temp ~ month + hour, method = "gbm", data = tr2[, predictors], preProcess = c("center", "scale"), metric = "RMSE", trControl = trainControl(method = "cv", number = 10))

set.seed(1234)
modFit_rf_temp <- train(temp ~ month + hour, method = "rf", data = tr2[, predictors], preProcess = c("center", "scale"), metric = "RMSE", trControl = trainControl(method = "cv", number = 10))

set.seed(1234)
modFit_svm_temp <- train(temp ~ month + hour, method = "svmRadial", data = tr2[, predictors], preProcess = c("center", "scale"), metric = "RMSE", trControl = trainControl(method = "cv", number = 10))


# wind_ms
set.seed(1234)
predictors <- names(tr2) %in% c("wind_ms", "month", "hour")
modFit_gbm_wind_ms <- train(wind_ms ~ month + hour, method = "gbm", data = tr2[, predictors], preProcess = c("center", "scale"), metric = "RMSE", trControl = trainControl(method = "cv", number = 10))

set.seed(1234)
modFit_rf_wind_ms <- train(wind_ms ~ month + hour, method = "rf", data = tr2[, predictors], preProcess = c("center", "scale"), metric = "RMSE", trControl = trainControl(method = "cv", number = 10))

set.seed(1234)
modFit_svm_wind_ms <- train(wind_ms ~ month + hour, method = "svmRadial", data = tr2[, predictors], preProcess = c("center", "scale"), metric = "RMSE", trControl = trainControl(method = "cv", number = 10))


# wind_dir
set.seed(1234)
predictors <- names(tr2) %in% c("wind_dir", "month", "hour")
modFit_gbm_wind_dir <- train(wind_dir ~ month + hour, method = "gbm", data = tr2[, predictors], preProcess = c("center", "scale"), metric = "RMSE", trControl = trainControl(method = "cv", number = 10))

set.seed(1234)
modFit_rf_wind_dir <- train(wind_dir ~ month + hour, method = "rf", data = tr2[, predictors], preProcess = c("center", "scale"), metric = "RMSE", trControl = trainControl(method = "cv", number = 10))

set.seed(1234)
modFit_svm_wind_dir <- train(wind_dir ~ month + hour, method = "svmRadial", data = tr2[, predictors], preProcess = c("center", "scale"), metric = "RMSE", trControl = trainControl(method = "cv", number = 10))


# humidity
set.seed(1234)
predictors <- names(tr2) %in% c("humidity", "month", "hour", "temp", "wind_ms", "wind_dir")
modFit_gbm_humidity <- train(humidity ~ month + hour + temp + wind_ms + wind_dir, method = "gbm", data = tr2[, predictors], preProcess = c("center", "scale"), metric = "RMSE", trControl = trainControl(method = "cv", number = 10))

set.seed(1234)
modFit_rf_humidity <- train(humidity ~ month + hour + temp + wind_ms + wind_dir, method = "rf", data = tr2[, predictors], preProcess = c("center", "scale"), metric = "RMSE", trControl = trainControl(method = "cv", number = 10))

set.seed(1234)
modFit_svm_humidity <- train(humidity ~ month + hour + temp + wind_ms + wind_dir, method = "svmRadial", data = tr2[, predictors], preProcess = c("center", "scale"), metric = "RMSE", trControl = trainControl(method = "cv", number = 10))


# gusts_ms
set.seed(1234)
predictors <- names(tr2) %in% c("gusts_ms", "humidity", "month", "hour", "temp", "wind_ms", "wind_dir")
modFit_gbm_gusts_ms <- train(gusts_ms ~ month + hour + temp + wind_ms + wind_dir + humidity, method = "gbm", data = tr2[, predictors], preProcess = c("center", "scale"), metric = "RMSE", trControl = trainControl(method = "cv", number = 10))

set.seed(1234)
modFit_rf_gusts_ms <- train(gusts_ms ~ month + hour + temp + wind_ms + wind_dir + humidity, method = "rf", data = tr2[, predictors], preProcess = c("center", "scale"), metric = "RMSE", trControl = trainControl(method = "cv", number = 10))

set.seed(1234)
modFit_svm_gusts_ms <- train(gusts_ms ~ month + hour + temp + wind_ms + wind_dir + humidity, method = "svmRadial", data = tr2[, predictors], preProcess = c("center", "scale"), metric = "RMSE", trControl = trainControl(method = "cv", number = 10))


# leafwet_lwscnt
set.seed(1234)
predictors <- names(tr2) %in% c("leafwet_lwscnt", "gusts_ms", "humidity", "month", "hour", "temp", "wind_ms", "wind_dir")
modFit_gbm_leafwet_lwscnt <- train(leafwet_lwscnt ~ month + hour + temp + wind_ms + wind_dir + humidity + gusts_ms, method = "gbm", data = tr2[, predictors], preProcess = c("center", "scale"), metric = "RMSE", trControl = trainControl(method = "cv", number = 10))

set.seed(1234)
modFit_rf_leafwet_lwscnt <- train(leafwet_lwscnt ~ month + hour + temp + wind_ms + wind_dir + humidity + gusts_ms, method = "rf", data = tr2[, predictors], preProcess = c("center", "scale"), metric = "RMSE", trControl = trainControl(method = "cv", number = 10))

set.seed(1234)
modFit_svm_leafwet_lwscnt <- train(leafwet_lwscnt ~ month + hour + temp + wind_ms + wind_dir + humidity + gusts_ms, method = "svmRadial", data = tr2[, predictors], preProcess = c("center", "scale"), metric = "RMSE", trControl = trainControl(method = "cv", number = 10))


# leafwet460_min
set.seed(1234)
predictors <- names(tr2) %in% c("leafwet460_min", "leafwet_lwscnt", "gusts_ms", "humidity", "month", "hour", "temp", "wind_ms", "wind_dir")
modFit_gbm_leafwet460_min <- train(leafwet460_min ~ month + hour + temp + wind_ms + wind_dir + humidity + gusts_ms + leafwet_lwscnt, method = "gbm", data = tr2[, predictors], preProcess = c("center", "scale"), metric = "RMSE", trControl = trainControl(method = "cv", number = 10))

set.seed(1234)
modFit_rf_leafwet460_min <- train(leafwet460_min ~ month + hour + temp + wind_ms + wind_dir + humidity + gusts_ms + leafwet_lwscnt, method = "rf", data = tr2[, predictors], preProcess = c("center", "scale"), metric = "RMSE", trControl = trainControl(method = "cv", number = 10))

set.seed(1234)
modFit_svm_leafwet460_min <- train(leafwet460_min ~ month + hour + temp + wind_ms + wind_dir + humidity + gusts_ms + leafwet_lwscnt, method = "svmRadial", data = tr2[, predictors], preProcess = c("center", "scale"), metric = "RMSE", trControl = trainControl(method = "cv", number = 10))


# leafwet450_min
set.seed(1234)
predictors <- names(tr2) %in% c("leafwet450_min", "leafwet460_min", "leafwet_lwscnt", "gusts_ms", "humidity", "month", "hour", "temp", "wind_ms", "wind_dir")
modFit_gbm_leafwet450_min <- train(leafwet450_min ~ month + hour + temp + wind_ms + wind_dir + humidity + gusts_ms + leafwet_lwscnt + leafwet460_min, method = "gbm", data = tr2[, predictors], preProcess = c("center", "scale"), metric = "RMSE", trControl = trainControl(method = "cv", number = 10))

set.seed(1234)
modFit_rf_leafwet450_min <- train(leafwet450_min ~ month + hour + temp + wind_ms + wind_dir + humidity + gusts_ms + leafwet_lwscnt + leafwet460_min, method = "rf", data = tr2[, predictors], preProcess = c("center", "scale"), metric = "RMSE", trControl = trainControl(method = "cv", number = 10))

set.seed(1234)
modFit_svm_leafwet450_min <- train(leafwet450_min ~ month + hour + temp + wind_ms + wind_dir + humidity + gusts_ms + leafwet_lwscnt + leafwet460_min, method = "svmRadial", data = tr2[, predictors], preProcess = c("center", "scale"), metric = "RMSE", trControl = trainControl(method = "cv", number = 10))


# Submission format
# Preparing submission format
submissionformat <- read.csv("C:/submission_format.csv")
submissionformat <- submissionformat[,-2]
submissionformat <- data.frame(submissionformat)
names(submissionformat) <- "X"

# Merging with testing2hour_new and training2hour
submissionformat_new <- merge(submissionformat, testing2hour, by.x = "X", by.y = "X", all.x = TRUE)
dim(submissionformat_new)
# [1] 1590    9

length(unique(submissionformat_new$X))

sum(is.na(submissionformat_new))
# [1] 4032


# Filling in missing values in test set
library(lubridate)
library(dplyr)
submissionformat_new$X <- ymd_hms(submissionformat_new$X)
submissionformat_new$month <- month(submissionformat_new$X)
submissionformat_new$hour <- hour(submissionformat_new$X)

missingrows <- which(!(complete.cases(submissionformat_new)))

for (i in 1:length(missingrows)) {
			k <- missingrows[i]
			if (is.na(submissionformat_new[k, ])[, 3] == TRUE) {
						submissionformat_new[k, 3] <- mean(predict(modFit_gbm_temp, submissionformat_new[k, ]), predict(modFit_rf_temp, submissionformat_new[k, ]), predict(modFit_svm_temp, submissionformat_new[k, ]))
			}
			if (is.na(submissionformat_new[k, ])[, 9] == TRUE) {
						submissionformat_new[k, 9] <- mean(predict(modFit_gbm_wind_ms, submissionformat_new[k, ]), predict(modFit_rf_wind_ms, submissionformat_new[k, ]), predict(modFit_svm_wind_ms, submissionformat_new[k, ]))
			}
			if (is.na(submissionformat_new[k, ])[, 8] == TRUE) {
						submissionformat_new[k, 8] <- mean(predict(modFit_gbm_wind_dir, submissionformat_new[k, ]), predict(modFit_rf_wind_dir, submissionformat_new[k, ]), predict(modFit_svm_wind_dir, submissionformat_new[k, ]))
			}
			if (is.na(submissionformat_new[k, ])[, 2] == TRUE) {
						submissionformat_new[k, 2] <- mean(predict(modFit_gbm_humidity, submissionformat_new[k, ]), predict(modFit_rf_humidity, submissionformat_new[k, ]), predict(modFit_svm_humidity, submissionformat_new[k, ]))
			}
			if (is.na(submissionformat_new[k, ])[, 7] == TRUE) {
						submissionformat_new[k, 7] <- mean(predict(modFit_gbm_gusts_ms, submissionformat_new[k, ]), predict(modFit_rf_gusts_ms, submissionformat_new[k, ]), predict(modFit_svm_gusts_ms, submissionformat_new[k, ]))
			}
			if (is.na(submissionformat_new[k, ])[, 6] == TRUE) {
						submissionformat_new[k, 6] <- mean(predict(modFit_gbm_leafwet_lwscnt, submissionformat_new[k, ]), predict(modFit_rf_leafwet_lwscnt, submissionformat_new[k, ]), predict(modFit_svm_leafwet_lwscnt, submissionformat_new[k, ]))
			}
			if (is.na(submissionformat_new[k, ])[, 5] == TRUE) {
						submissionformat_new[k, 5] <- mean(predict(modFit_gbm_leafwet460_min, submissionformat_new[k, ]), predict(modFit_rf_leafwet460_min, submissionformat_new[k, ]), predict(modFit_svm_leafwet460_min, submissionformat_new[k, ]))
			}
			if (is.na(submissionformat_new[k, ])[, 4] == TRUE) {
						submissionformat_new[k, 4] <- mean(predict(modFit_gbm_leafwet450_min, submissionformat_new[k, ]), predict(modFit_rf_leafwet450_min, submissionformat_new[k, ]), predict(modFit_svm_leafwet450_min, submissionformat_new[k, ]))
			}
}
			                                                               

sum(is.na(submissionformat_new))
# 0
sum(complete.cases(submissionformat_new))
# [1] 1590

# write.csv(submissionformat_new, "C:/submissionformat_new.csv", row.names = FALSE)

# Final dataset for final prediction
submissionformat_new$X <- factor(submissionformat_new$X)
finaldata <- merge(submissionformat, submissionformat_new, by.x = "X", by.y = "X", all.x = TRUE)

# Final results
predfinal_gbm <- predict(modFit_gbm_yield, finaldata)
predfinal_rf <- predict(modFit_rf_yield, finaldata)
predfinal_svm <- predict(modFit_svm_yield, finaldata)
predfinal <- predfinal_svm
predfinal[predfinal < 0] = 0
mean(predfinal)
# [1] 0.7848628

timestamp <- finaldata[,1]
timestamp <- data.frame(timestamp)
final <- cbind(timestamp, predfinal)
names(final) <- c("", "yield")

# write.csv(final, "C:/final.csv", row.names = FALSE)



