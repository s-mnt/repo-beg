con <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(con, "C:/assignment.zip")
dir.create("assignment")
# Extracting all files in zip folder into above created "assignment" directory
unzip("assignment.zip", exdir = "C:/assignment")
# Listing all files in all sub-directories
list.files("C:/assignment", recursive = TRUE)
library(dplyr)

subject_test <- read.table("C:/assignment/UCI HAR Dataset/test/subject_test.txt")
# this table contains list of subject IDs participated in activities 
subject_test <- rename(subject_test, subjectid = V1)

X_test <- read.table("C:/assignment/UCI HAR Dataset/test/X_test.txt")	
# this table contains data for all 561 features in test dataset

y_test <- read.table("C:/assignment/UCI HAR Dataset/test/y_test.txt")
# this table contains Activity ID for any of six activities performed
y_test <- rename(y_test, activityid = V1)

test_data <- cbind(subject_test, y_test, X_test)
# This is the test dataset obtained by merging above 3 tables

# Merging the subject_train, X_train and y_train tables in the similar way as above to obtain the training set 
subject_train <- read.table("C:/assignment/UCI HAR Dataset/train/subject_train.txt")
subject_train <- rename(subject_train, subjectid = V1)
X_train <- read.table("C:/assignment/UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("C:/assignment/UCI HAR Dataset/train/y_train.txt")
y_train <- rename(y_train, activityid = V1)
train_data <- cbind(subject_train, y_train, X_train)

# Appending test set to training set
complete_dataset <- bind_rows(train_data, test_data)

# Sorting on subject ID 
complete_dataset <- arrange(complete_dataset, subjectid)

features <- read.table("C:/assignment/UCI HAR Dataset/features.txt")
# this table contains feature names for all 561 features in the dataset

# Extracting only the measurements on the mean and standard deviation for each measurement
extract <- grep("[Mm]ean|[Ss]td", features[["V2"]])
features2 <- features[extract, ]
extr <- c(1, 2, extract+2)
complete_dataset <- complete_dataset[ , extr]

# Introducing name of each activity in the dataset
activity <- read.table("C:/assignment/UCI HAR Dataset/activity_labels.txt")
complete_dataset <- merge(complete_dataset, activity, by.x = "activityid", by.y = "V1", all = TRUE)
complete_dataset <- rename(complete_dataset, activityname = V2.y)
len <- length(complete_dataset)
complete_dataset <- complete_dataset[ ,c(1,len,2,3:(len-1))]

# Introducing descriptive names of all variables in the dataset
varnames <- features2[[2]]
varnames <- gsub("-", "", varnames)
varnames <- gsub("\\(", "", varnames)
varnames <- gsub("\\)", "", varnames)
varnames <- gsub(",", "", varnames)
names(complete_dataset)[4:len] <- varnames

# Creating a second, independent tidy data set with the average of each variable for each activity and each subject.
grouped <- group_by(complete_dataset, activityname, subjectid)
grouped <- summarize_each(grouped, funs(mean)) 
grouped <- arrange(grouped, subjectid, activityid)
write.table(grouped, "C:/final.txt", sep="\t", row.names = FALSE)