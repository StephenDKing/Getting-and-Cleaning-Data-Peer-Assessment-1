# Getting and Cleaning Data - Peer Assessment 1
# You should create one R script called run_analysis.R that does the following. 
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names. 
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

## 1. Merges the training and the test sets to create one data set.
### Training
xTrain <- read.table("train/X_train.txt", header=FALSE)
yTrain <- read.table("train/Y_train.txt", header=FALSE)

### Test
xTest <- read.table("test/X_test.txt", header=FALSE)
yTest <- read.table("test/Y_test.txt", header=FALSE)

### Subjects
subjectTrain <- read.table("train/subject_train.txt", header=FALSE)
subjectTest <- read.table("test/subject_test.txt", header=FALSE)

### Merge data
xMerged <- rbind(xTrain, xTest)
yMerged <- rbind(yTrain, yTest)
subject <- rbind(subjectTrain, subjectTest)


# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names. 
### Build Features and Labels then apply
features <- scan("features.txt", what = "character", sep = " ")
labels <- scan("activity_labels.txt", what = "character", sep=" ")

ID <- seq(2,length(features), by= 2)
Features <- features[ID]
ID <- seq(2, length(labels), by= 2)
Labels <- labels[ID]

colnames(xMerged) <- Features
colnames(yMerged) <- c("Activity")
colnames(subject) <- c("Subject")
yMerged$Activity <- factor(yMerged$Activity, labels = Labels)

allData <- cbind(yMerged, subject, xMerged)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 

meanLower <- grep("mean", names(allData))
meanUpper <- grep("Mean", names(allData))

mean <- append(meanLower, meanUpper, after = length(meanLower))

sDev <- grep("std", names(allData))  # select std's
vars <- append(mean, sDev, after = length(mean))
vars <- append(c(1,2), vars, after = length(c(1,2)))
vars <- sort(vars)
tidyDataMean <- allData[,vars]

# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
## Load Libs
library(plyr)
library(reshape)
library(reshape2)

unPivoted <- melt(tidyDataMean, id = c("Subject", "Activity"))  
unPivoted$value <- as.numeric(unPivoted$value)
activtySubject <- aggregate(unPivoted$value, list(unPivoted$Activity, unPivoted$variable), mean)

# Write out the data file
write.csv(tidyDataMean, file= "tidyData.txt", row.names=FALSE)
