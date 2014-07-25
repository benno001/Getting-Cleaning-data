# Getting and Cleaning data course project
# Ben de Haan
# 
# This R script:
# 0. Downloads and unzips the data and sets the working directory
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names. 
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 


### 0. Download and unzip data, set working directory ###

## If file not present in working directory, download file
if(!file.exists("dataset.zip")){
    download.file(
        url = "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", 
        destfile = "dataset.zip", 
        method = "curl")}

## If not unzipped, unzip
if(!file.exists("UCI HAR Dataset")){
    unzip("dataset.zip")
}

## Set working directory to location of unzipped files
setwd("UCI HAR Dataset")

### 1. Merge training and test set ###
## Read labels and their descriptions
activityLabels = read.table("activity_labels.txt", sep = " ", header = FALSE)
features = read.table("features.txt", sep = " ", header = FALSE)

## Read test data
subjectTest = read.table("test/subject_test.txt", header = FALSE)
xTest = read.table("test/X_test.txt", header = FALSE)
yTest = read.table("test/Y_test.txt", header = FALSE)

## Read train data
subjectTrain = read.table("train/subject_train.txt", header = FALSE)
xTrain = read.table("train/X_train.txt", header = FALSE)
yTrain = read.table("train/Y_train.txt", header = FALSE)

## Assign column names
colnames(activityLabels) <- c("activityID", "activityDescription")
colnames(features) <- c("featureID", "featureDescription")

colnames(subjectTest) <- "subjectID"
colnames(subjectTrain) <- "subjectID"

colnames(xTest) <- features$featureDescription
colnames(xTrain) <- features$featureDescription

colnames(yTest) <- "activityID"
colnames(yTrain) <-"activityID"

## Bind test data
test <- cbind(subjectTest, xTest, yTest)

## Bind training data
train <- cbind(subjectTrain, xTrain, yTrain)

## Bind test and training data
total <- rbind(test, train)

### 2. Extract only the measurements on the mean and standard deviation for each measurement. ###

## Retrieve all column names
totalNames <- colnames(total)

## To identify needed columns, create a logical vector 
columnIsNeeded <- (grepl("subjectID", totalNames) | 
                       grepl("activityID", totalNames) | 
                       (grepl(".*-mean..", totalNames) & 
                            !grepl(".*-mean..-", totalNames) &
                            !grepl(".*meanFreq", totalNames)) | 
                       (grepl(".*-std..", totalNames) & 
                            !grepl(".*-std..-.*", totalNames))
)

## Create final total dataset
finalTotal <- total[columnIsNeeded]

### 3. Use descriptive activity names to name the activities in the data set ###

totalData <- merge(finalTotal, activityLabels, by = "activityID")

## Remove unneccessary activityID column
totalData <- totalData[,2:21]

### 4. Appropriately label the data set with descriptive variable names. ###

## Get column names to view needed changes
colnames(totalData)

## Set column names - maybe a bit overdone on the 'descriptive' part?
colnames(totalData) <- c("subjectID", 
                         "AverageTimeOfBodyAccelerationMagnitude", 
                         "StandardDeviationOfTimeOfBodyAccelerationMagnitude",
                         "AverageTimeOfGravityAccelerationMagnitude",
                         "StandardDeviationOfTimeOfGravityAccelerationMagnitude",
                         "AverageTimeOfBodyAccelerationJerkMagnitude",
                         "StandardDeviationOfTimeOfBodyAccelerationJerkMagnitude",
                         "AverageTimeOfBodyGyroMagnitude",
                         "StandardDeviationOfTimeOfBodyGyroMagnitude",
                         "AverageTimeOfBodyGyroJerkMagnitude",
                         "StandardDeviationOfTimeOfBodyGyroJerkMagnitude",
                         "AverageFrequencyOfBodyAccelerationMagnitude",
                         "StandardDeviationOfFrequencyOfBodyAccelerationMagnitude",
                         "AverageFrequencyOfBodyAccelerationJerkMagnitude",
                         "StandardDeviationOfFrequencyOfBodyAccelerationJerkMagnitude",
                         "AverageFrequencyOfBodyGyroMagnitude",
                         "StandardDeviationOfFrequencyOfBodyGyroMagnitude",
                         "AverageFrequencyOfBodyGyroJerkMagnitude",
                         "StandardDeviationOfFrequencyOfBodyGyroJerkMagnitude",
                         "activityDescription")

### 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. ###

## Aggregate data. This creates a dataset ordered by subset. Alternatively, 
## it can be ordered by activityDescription by swapping subjectID and activityDescription
tidyData <- aggregate(. ~ subjectID + activityDescription, data = totalData, mean)

## Create file
write.table(tidyData, "tidyData.txt", sep="\t")
