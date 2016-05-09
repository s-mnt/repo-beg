# Download the data in 2 separate files and read the first file containing data
file1 <- read.table("C:/adult.txt", sep = ",")
file2 <- read.table("C:/adult2.txt", sep = ",")

# Removing unnecessary . from income column of file2
file2[["V15"]] <- gsub("[.]", "", file2[["V15"]])

# Combining both data files
nrow(file1)
# 32561
nrow(file2)
# 16281
file <- rbind(file1, file2)
nrow(file)
# 48842

# Assigning names to all variables in the dataset
names(file) <- c("age", "workclass", "fnlwgt", "education", "education-num", "marital-status", "occupation", "relationship", "race", "sex", "capital-gain", "capital-loss", "hours-per-week", "native-country", "class")

# Removing unnecessary spaces from factor variable values in the dataset
for (i in 1:ncol(file)) {
		if (class(file[[i]]) == "factor") {
				file[[i]] <- gsub("[ ]", "", file[[i]])
				file[[i]] <- as.factor(file[[i]])
		}
}

# Replacing "?" with NA
for (i in 1:ncol(file)) {
		file[[i]][file[[i]] == "?"] <- NA
}

# Finding number of observations with no missing values
sum(complete.cases(file))
# 45222

# Removing observations with missing data
file <- file[complete.cases(file), ]
nrow(file)
# 45222

# Removing unused levels of the factor variables in the dataset
for (i in 1:ncol(file)) {
		if (class(file[[i]]) == "factor") {
				file[[i]] <- factor(file[[i]])
				}
}

# Viewing the descriptive statistics of variables
# Extracting the names of all numeric variables
j = 1
intvars <- "Null"
for (i in 1:ncol(file)) {
		if (class(file[[i]]) == "integer") {
				intvars[j] <- i
				j = j + 1
				}
}
intvars <- as.numeric(intvars)
library(psych)
describe(subset(file, select = intvars), skew = FALSE)
summary(subset(file, select = -intvars))

# Descriptive statistics in both income groups separately
file_sub1 <- subset(file, class == "<=50K", select = c(intvars, class))
describe(file_sub1, skew = FALSE)
file_sub2 <- subset(file, class == ">50K", select = c(intvars, class))
describe(file_sub2, skew = FALSE)

# Removing unnecessary dashes from file names
names(file) <- gsub("[-]", "", names(file))

# Percentage distribution within different workclasses in both income classes
t <- table(file$workclass, file$class)
prop.table(t, 2)*100

# Classifying into Highly-qualified abd Less-qualified
x <- (file[["education"]] == "Bachelors" | file[["education"]] == "Masters" | file[["education"]] == "Doctorate")
file[["educationgroup"]] <- "Less-qualified"
file[x,"educationgroup"] <- "Highly-qualified"
file[["educationgroup"]] <- factor(file[["educationgroup"]])

# Percentage distribution within different education groups in both income classes
t <- table(file$educationgroup, file$class)
prop.table(t, 2)*100

# Percentage distribution by marital status within both income classes
t <- table(file$maritalstatus, file$class)
prop.table(t, 2)*100 

# Percentage distribution by occupation within both income classes
file[["occupationgroup"]] <- "others"
x <- (file[["occupation"]] == "Exec-managerial" | file[["occupation"]] == "Prof-specialty")
file[x, "occupationgroup"] <- "executive-skilled"
t <- table(file$occupationgroup, file$class)
prop.table(t, 2)*100 

# Percentage distribution by relationship within both income classes
t <- table(file$relationship, file$class)
prop.table(t, 2)*100 

# Percentage distribution by race within both income classes
t <- table(file$race, file$class)
prop.table(t, 2)*100 

# Percentage distribution by sex within both income classes
t <- table(file$sex, file$class)
prop.table(t, 2)*100

# Percentage distribution by native country within both income classes
t <- table(file$nativecountry, file$class)
prop.table(t, 2)*100
file[["nativecountrygroup"]] <- "Other"
x <- (file[["nativecountry"]] == "United-States")
file[x, "nativecountrygroup"] <- "US"
t <- table(file$nativecountrygroup, file$class)
prop.table(t, 2)*100

# Exploratory data analysis
library(ggplot2)
qplot(age, data = file, geom = "density", facets = . ~ class)			# density plot
qplot(fnlwgt, data = file, geom = "density", facets = . ~ class)
qplot(educationnum, data = file, geom = "density", facets = . ~ class)
qplot(capitalgain, data = file, facets = . ~ class)						# histogram
qplot(capitalloss, data = file, facets = . ~ class)
qplot(hoursperweek, data = file, geom = "density", facets = . ~ class)
p <- qplot(workclass, data = file, fill = workclass, facets = . ~ class)		# barplot
p + scale_x_discrete(breaks=NULL)												# removes x axis ticks
p <- qplot(educationgroup, data = file, fill = educationgroup, facets = . ~ class)
p + scale_x_discrete(breaks=NULL)
p <- qplot(maritalstatus, data = file, fill = maritalstatus, facets = class ~ .)
p + scale_x_discrete(breaks=NULL)
p <- qplot(occupationgroup, data = file, fill = occupationgroup, facets = . ~ class)
p + scale_x_discrete(breaks=NULL)
p <- qplot(relationship, data = file, fill = relationship, facets = class ~ .)
p + scale_x_discrete(breaks=NULL)
p <- qplot(race, data = file, fill = race, facets = class ~ .)
p + scale_x_discrete(breaks=NULL)
p <- qplot(sex, data = file, fill = sex, facets = . ~ class)
p + scale_x_discrete(breaks=NULL)
p <- qplot(nativecountrygroup, data = file, fill = nativecountrygroup, facets = . ~ class)
p + scale_x_discrete(breaks=NULL)

file[["educationgroup"]] <- NULL
file[["occupationgroup"]] <- NULL
file[["nativecountrygroup"]] <- NULL

# important predictors - age, educationnum, capitalgain, capitalloss, hoursperweek, education, maritalstatus, occupation, relationship, sex
# highly skewed variable - capitalgain, capitalloss

# Methodology
library(caret)
# Checking for near zero variance predictors
nearZeroVar(file, saveMetrics = TRUE)

# Create training and test sets from the dataset
set.seed(1234)
intrain <- createDataPartition(y = file$class, p = 0.7, list = FALSE)
training <- file[intrain,]
testing <- file[-intrain,]

# Standardizing highly skewed variables - capitalgain and capitalloss by centering and scaling transformation
preObj <- preProcess(training[,c("capitalgain", "capitalloss")], method = c("center", "scale"))
x <- predict(preObj, training[, c("capitalgain", "capitalloss")])
training[["capitalgain"]] <- x[["capitalgain"]]
training[["capitalloss"]] <- x[["capitalloss"]]
mean(training$capitalgain)
sd(training$capitalgain)
mean(training$capitalloss)
sd(training$capitalloss)
y <- predict(preObj, testing[, c("capitalgain", "capitalloss")])
testing[["capitalgain"]] <- y[["capitalgain"]]
testing[["capitalloss"]] <- y[["capitalloss"]]
mean(testing$capitalgain)
sd(testing$capitalgain)
mean(testing$capitalloss)
sd(testing$capitalloss)

# Finding correlation between predictors
M <- abs(cor(training[, intvars]))
diag(M) <- 0
which(M > 0.8, arr.ind = T)

# Splitting training set into training and validation set
set.seed(1234)
ind <- createDataPartition(training$class, p = 0.7, list = FALSE)
tr <- training[ind, ]
cv <- training[-ind, ]
dim(tr)
dim(cv)

# write.csv(training, "C:/training.csv", row.names = FALSE)
# write.csv(testing, "C:/testing.csv", row.names = FALSE)
# write.csv(tr, "C:/tr.csv", row.names = FALSE)
# write.csv(cv, "C:/cv.csv", row.names = FALSE)

# Generalized Linear Model
# Training Generalized Linear Model on training set with 10 fold cross-validation
set.seed(1234)
modFit_glm <- train(class ~ ., method = "glm", data = tr, trControl = trainControl(method = "cv", number = 10))
modFit_glm$finalModel
# Accuracy  Kappa     Accuracy SD  Kappa SD  
  # 0.846435  0.561095  0.008769436  0.02440463

# Predicting on training set
predtr_glm <- predict(modFit_glm, tr)
# write.csv(predtr_glm, "C:/predtr_glm.csv", row.names = FALSE)

# Measuring the accuracy of model on training set
confusionMatrix(predtr_glm, tr$class)
 # Accuracy : 0.8488         
                 # 95% CI : (0.844, 0.8535)
    # No Information Rate : 0.7521         
    # P-Value [Acc > NIR] : < 2.2e-16      
                                         
                  # Kappa : 0.5677         
 # Mcnemar's Test P-Value : < 2.2e-16      
                                         
            # Sensitivity : 0.9300         
            # Specificity : 0.6024         
         # Pos Pred Value : 0.8765         
         # Neg Pred Value : 0.7394         
             # Prevalence : 0.7521         
         # Detection Rate : 0.6995         
   # Detection Prevalence : 0.7981         
      # Balanced Accuracy : 0.7662  

# Predicting on validation set
predcv_glm <- predict(modFit_glm, cv)
# write.csv(predcv_glm, "C:/predcv_glm.csv", row.names = FALSE)

# Measuring the accuracy of model on validation set
confusionMatrix(predcv_glm, cv$class)
# Accuracy : 0.8487          
                 # 95% CI : (0.8413, 0.8558)
    # No Information Rate : 0.7522          
    # P-Value [Acc > NIR] : < 2.2e-16       
                                          
                  # Kappa : 0.5655          
 # Mcnemar's Test P-Value : < 2.2e-16       
                                          
            # Sensitivity : 0.9317          
            # Specificity : 0.5967          
         # Pos Pred Value : 0.8752          
         # Neg Pred Value : 0.7421          
             # Prevalence : 0.7522          
         # Detection Rate : 0.7008          
   # Detection Prevalence : 0.8008          
     # Balanced Accuracy : 0.7642

# Predicting outcome for test set
pred_glm <- predict(modFit_glm, testing)
# write.csv(pred_glm, "C:/pred_glm.csv", row.names = FALSE)
               # Accuracy : 0.8493          
                 # 95% CI : (0.8432, 0.8553)
    # No Information Rate : 0.7522          
    # P-Value [Acc > NIR] : < 2.2e-16       
                                          
                  # Kappa : 0.5657          
 # Mcnemar's Test P-Value : < 2.2e-16       
                                          
            # Sensitivity : 0.9339          
            # Specificity : 0.5925          
         # Pos Pred Value : 0.8743          
         # Neg Pred Value : 0.7472          
             # Prevalence : 0.7522          
         # Detection Rate : 0.7025          
   # Detection Prevalence : 0.8035          
      # Balanced Accuracy : 0.7632         

# Random Forests
# Training Random Forests on training set with 10 fold cross-validation
set.seed(1234)
modFit_rf <- train(class ~ ., method = "rf", data = tr, trControl = trainControl(method = "cv", number = 10))
modFit_rf$finalModel
# Accuracy     Kappa  AccuracySD    KappaSD
# 0.8526170 0.5831297 0.006603512 0.01808939

# Predicting on training set
predtr_rf <- predict(modFit_rf, tr)
# write.csv(predtr_rf, "C:/predtr_rf.csv", row.names = FALSE)

# Measuring the accuracy of model on training set
confusionMatrix(predtr_rf, tr$class)
# Accuracy : 0.9997          
                 # 95% CI : (0.9994, 0.9999)
    # No Information Rate : 0.7521          
    # P-Value [Acc > NIR] : < 2e-16         
                                          
                  # Kappa : 0.9993          
 # Mcnemar's Test P-Value : 0.04123         
                                          
            # Sensitivity : 1.0000          
            # Specificity : 0.9989          
         # Pos Pred Value : 0.9996          
         # Neg Pred Value : 1.0000          
             # Prevalence : 0.7521          
         # Detection Rate : 0.7521          
   # Detection Prevalence : 0.7524          
      # Balanced Accuracy : 0.9995    

# Predicting on validation set
predcv_rf <- predict(modFit_rf, cv)
# write.csv(predcv_rf, "C:/predcv_rf.csv", row.names = FALSE)

# Measuring the accuracy of model on validation set
confusionMatrix(predcv_rf, cv$class)
# Accuracy : 0.8548          
                 # 95% CI : (0.8475, 0.8618)
    # No Information Rate : 0.7522          
    # P-Value [Acc > NIR] : < 2.2e-16       
                                          
                  # Kappa : 0.5873          
 # Mcnemar's Test P-Value : < 2.2e-16       
                                          
            # Sensitivity : 0.9310          
            # Specificity : 0.6235          
         # Pos Pred Value : 0.8824          
         # Neg Pred Value : 0.7485          
             # Prevalence : 0.7522          
         # Detection Rate : 0.7003          
   # Detection Prevalence : 0.7936          
      # Balanced Accuracy : 0.7772   

# Predicting outcome for test set
pred_rf <- predict(modFit_rf, testing)
# write.csv(pred_rf, "C:/pred_rf.csv", row.names = FALSE)
               # Accuracy : 0.8535          
                 # 95% CI : (0.8474, 0.8594)
    # No Information Rate : 0.7522          
    # P-Value [Acc > NIR] : < 2.2e-16       
                                          
                  # Kappa : 0.5847          
 # Mcnemar's Test P-Value : < 2.2e-16       
                                          
            # Sensitivity : 0.9289          
            # Specificity : 0.6246          
         # Pos Pred Value : 0.8825          
         # Neg Pred Value : 0.7431          
             # Prevalence : 0.7522          
         # Detection Rate : 0.6987          
   # Detection Prevalence : 0.7917          
      # Balanced Accuracy : 0.7767   


# Gradient Boosting Model
# Training Gradient Boosting model on training set with 10 fold cross-validation
set.seed(1234)
modFit_gbm <- train(class ~ ., method = "gbm", data = tr, verbose = FALSE, trControl = trainControl(method = "cv", number = 10))
modFit_gbm$finalModel

# Predicting on training set
predtr_gbm <- predict(modFit_gbm, tr)
# write.csv(predtr_gbm, "C:/predtr_gbm.csv", row.names = FALSE)

# Measuring the accuracy of model on training set
confusionMatrix(predtr_gbm, tr$class)
              # Accuracy : 0.8623          
                 # 95% CI : (0.8577, 0.8668)
    # No Information Rate : 0.7521          
    # P-Value [Acc > NIR] : < 2.2e-16       
                                          
                  # Kappa : 0.5994          
 # Mcnemar's Test P-Value : < 2.2e-16       
                                          
            # Sensitivity : 0.9467          
            # Specificity : 0.6060          
         # Pos Pred Value : 0.8794          
         # Neg Pred Value : 0.7894          
             # Prevalence : 0.7521          
         # Detection Rate : 0.7120          
   # Detection Prevalence : 0.8097          
      # Balanced Accuracy : 0.7764  

# Predicting on validation set
predcv_gbm <- predict(modFit_gbm, cv)
# write.csv(predcv_gbm, "C:/predcv_gbm.csv", row.names = FALSE)

# Measuring the accuracy of model on validation set
confusionMatrix(predcv_gbm, cv$class)
              # Accuracy : 0.8604          
                 # 95% CI : (0.8532, 0.8673)
    # No Information Rate : 0.7522          
    # P-Value [Acc > NIR] : < 2.2e-16       
                                          
                  # Kappa : 0.5922          
 # Mcnemar's Test P-Value : < 2.2e-16       
                                          
            # Sensitivity : 0.9472          
            # Specificity : 0.5967          
         # Pos Pred Value : 0.8770          
         # Neg Pred Value : 0.7883          
             # Prevalence : 0.7522          
         # Detection Rate : 0.7125          
   # Detection Prevalence : 0.8124          
      # Balanced Accuracy : 0.7720   

# Predicting outcome for test set
pred_gbm <- predict(modFit_gbm, testing)
# write.csv(pred_gbm, "C:/pred_gbm.csv", row.names = FALSE)
              # Accuracy : 0.8638          
                 # 95% CI : (0.8579, 0.8695)
    # No Information Rate : 0.7522          
    # P-Value [Acc > NIR] : < 2.2e-16       
                                          
                  # Kappa : 0.6029          
 # Mcnemar's Test P-Value : < 2.2e-16       
                                          
            # Sensitivity : 0.9486          
            # Specificity : 0.6062          
         # Pos Pred Value : 0.8797          
         # Neg Pred Value : 0.7955          
             # Prevalence : 0.7522          
         # Detection Rate : 0.7135          
   # Detection Prevalence : 0.8111          
      # Balanced Accuracy : 0.7774  


# Stacking GLM, RF and GBM models
glmvar <- read.csv("C:/predtr_glm.csv")
rfvar <- read.csv("C:/predtr_rf.csv")
gbmvar <- read.csv("C:/predtr_gbm.csv")
stacked <- cbind(glmvar, rfvar, gbmvar, tr$class)
# write.csv(stacked, "C:/stackedtr.csv", row.names = FALSE)
names(stacked) <- c("glmpred", "rfpred", "gbmpred", "class")

for (i in 1:nrow(stacked)) {
		freq <- stacked[i, c("glmpred", "rfpred", "gbmpred")]
		freq <- as.vector(freq)
		if ((sum(freq == ">50K")) > (sum(freq == "<=50K")))
				predtr_stacked[i] = ">50K"
		else
				predtr_stacked[i] = "<=50K"
}

# write.csv(predtr_stacked, "C:/predtr_stacked.csv", row.names = FALSE)

# Measuring the accuracy on training set
confusionMatrix(predtr_stacked, stacked$class)
               # Accuracy : 0.8805          
                 # 95% CI : (0.8761, 0.8847)
    # No Information Rate : 0.7521          
    # P-Value [Acc > NIR] : < 2.2e-16       
                                          
                  # Kappa : 0.6544          
 # Mcnemar's Test P-Value : < 2.2e-16       
                                          
            # Sensitivity : 0.9561          
            # Specificity : 0.6508          
         # Pos Pred Value : 0.8926          
         # Neg Pred Value : 0.8302          
             # Prevalence : 0.7521          
         # Detection Rate : 0.7191          
   # Detection Prevalence : 0.8057          
      # Balanced Accuracy : 0.8035  
	  
glmvar_cv <- read.csv("C:/predcv_glm.csv")
rfvar_cv <- read.csv("C:/predcv_rf.csv")
gbmvar_cv <- read.csv("C:/predcv_gbm.csv")
stackedcv <- cbind(glmvar_cv, rfvar_cv, gbmvar_cv, cv$class)
names(stackedcv) <- c("glmpred", "rfpred", "gbmpred", "class")

for (i in 1:nrow(stackedcv)) {
		freq <- stackedcv[i, c("glmpred", "rfpred", "gbmpred")]
		freq <- as.vector(freq)
		if ((sum(freq == ">50K")) > (sum(freq == "<=50K")))
				predcv_stacked[i] = ">50K"
		else
				predcv_stacked[i] = "<=50K"
}

# write.csv(predcv_stacked, "C:/predcv_stacked.csv", row.names = FALSE)

# Measuring the accuracy on validation set
confusionMatrix(predcv_stacked, stackedcv$class)
              # Accuracy : 0.8612          
                 # 95% CI : (0.8541, 0.8681)
    # No Information Rate : 0.7522          
    # P-Value [Acc > NIR] : < 2.2e-16       
                                          
                  # Kappa : 0.5955          
 # Mcnemar's Test P-Value : < 2.2e-16       
                                          
            # Sensitivity : 0.9468          
            # Specificity : 0.6014          
         # Pos Pred Value : 0.8782          
         # Neg Pred Value : 0.7883          
             # Prevalence : 0.7522          
         # Detection Rate : 0.7122          
   # Detection Prevalence : 0.8110          
      # Balanced Accuracy : 0.7741  
	  
glmvartest <- read.csv("C:/pred_glm.csv")
rfvartest <- read.csv("C:/pred_rf.csv")
gbmvartest <- read.csv("C:/pred_gbm.csv")
stackedtest <- cbind(glmvartest, rfvartest, gbmvartest, testing$class)
names(stackedtest) <- c("glmpred", "rfpred", "gbmpred", "class")
predtest_stacked <- NULL

for (i in 1:nrow(stackedtest)) {
		freq <- stackedtest[i, c("glmpred", "rfpred", "gbmpred")]
		freq <- as.vector(freq)
		if ((sum(freq == ">50K")) > (sum(freq == "<=50K")))
				predtest_stacked[i] = ">50K"
		else
				predtest_stacked[i] = "<=50K"
}

# write.csv(predtest_stacked, "C:/predtest_stacked.csv", row.names = FALSE)

# Measuring the accuracy on testing set
confusionMatrix(predtest_stacked, stackedtest$class)
             # Accuracy : 0.8641          
                 # 95% CI : (0.8582, 0.8698)
    # No Information Rate : 0.7522          
    # P-Value [Acc > NIR] : < 2.2e-16       
                                          
                  # Kappa : 0.6046          
 # Mcnemar's Test P-Value : < 2.2e-16       
                                          
            # Sensitivity : 0.9480          
            # Specificity : 0.6095          
         # Pos Pred Value : 0.8805          
         # Neg Pred Value : 0.7942          
             # Prevalence : 0.7522          
         # Detection Rate : 0.7130          
   # Detection Prevalence : 0.8098          
      # Balanced Accuracy : 0.7787   

