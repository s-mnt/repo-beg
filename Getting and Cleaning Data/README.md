## Getting and Cleaning Data Course Project

The purpose of this project is to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis.
One of the most exciting areas in all of data science right now is wearable computing - see for example this article
(http://www.insideactivitytracking.com/data-science-activity-tracking-and-the-battle-for-the-worlds-top-sports-brand/). Companies like Fitbit, Nike, and
Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data from the below links represent data collected
from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained:
http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
Here are the data for the project:
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

The R script 'run_analysis.R' does the following:

1. Downloads the dataset zip file and extract all its contents to save in a newly created directory
2. Merges subject ID and activity data with data on 561 features of test data to obtain complete test set
3. Similarly, obtains the training set by merging data on subject, activity and all features of training data
4. Merges both the training set and test set to obtain complete dataset
5. Keeps only the variables denoting mean and standard deviation of measurements in the dataset
6. Introduces name of each activity in the dataset
7. Introduces descriptive names of all variables in the dataset
8. Creates a second, independent tidy data set with the average of each variable for each activity and each subject
