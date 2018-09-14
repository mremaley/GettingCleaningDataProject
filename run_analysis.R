library(data.table)
library(dplyr)

# Collect the data & unzip the folder
path <- getwd()
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url, file.path(path, "projectFiles.zip"))
unzip(zipfile = "projectFiles.zip")


# Read in activity labels & features
activityLabels <- fread(file.path(path, "UCI HAR Dataset/activity_labels.txt")) %>%
        rename(classLabels = V1, activityName = V2)
features <- fread(file.path(path, "UCI HAR Dataset/features.txt")) %>%
        rename(index = V1, featureName = V2)

# Extract only mean & standard deviation measurements
featuresMeanStdev <- grep("(mean|std)\\(\\)", features[, featureName])
measurements <- features[featuresMeanStdev, featureName]
measurements <- gsub('[()]', '', measurements )


# Read in train datasets
# X_train contains the training set
# y_train contains training labels
# subject_train identifies the subject who performed the activity 
train <- fread(file.path(path, "UCI HAR Dataset/train/X_train.txt"))[, featuresMeanStdev, with = F] 
train <- setnames(train, colnames(train), measurements)
trainActivities <- fread(file.path(path, "UCI HAR Dataset/train/y_train.txt")) %>%
        rename(Activity = V1)
trainSubjects <- fread(file.path(path, "UCI HAR Dataset/train/subject_train.txt")) %>%
        rename(subjectId = V1)

# Combine train datasets
train <- cbind(trainSubjects, trainActivities, train)


# Read in test datasets
# X_test contains the test set
# y_test contains test labels
# subject_test identifies the subject who performed the activity 
test <- fread(file.path(path, "UCI HAR Dataset/test/X_test.txt"))[, featuresMeanStdev, with = F] 
test <- setnames(test, colnames(test), measurements)
testActivities <- fread(file.path(path, "UCI HAR Dataset/test/y_test.txt")) %>%
        rename(Activity = V1)
testSubjects <- fread(file.path(path, "UCI HAR Dataset/test/subject_test.txt")) %>%
        rename(subjectId = V1)

# Combine test datasets
test <- cbind(testSubjects, testActivities, test)


# Merge training & test datasets to create one data set
combinedResults <- rbind(train, test) 

# Label data set with descriptive variable names
combinedResults[["Activity"]] <- factor(combinedResults[, Activity]
                                 , levels = activityLabels[["classLabels"]]
                                 , labels = activityLabels[["activityName"]])


# Create a function to replace column names with more descriptive variable names
colNames <- names(combinedResults)
colRename <- function(x) {
        x <- gsub("^(t)", "time", x)
        x <- gsub("^(f)", "frequency", x)
        x <- gsub("-mean\\()$", "Mean", x)
        x <- gsub("-std\\()$", "StDev", x)
        x <- gsub("Acc", "Accelerometer", x)
        x <- gsub("BodyBody", "Body", x)
        x <- gsub("Gyro", "Gyroscope", x)
        x <- gsub("Mag", "Magnitude", x)
}
names(combinedResults) <- sapply(colNames, colRename)


# Summarize data set by average of each variable for each activity and each subject
finalData <- combinedResults %>%
        group_by(subjectId, Activity) %>%
        summarize_each(funs(mean(., na.rm=TRUE))) %>%
        arrange(subjectId)


write.table(finalData, file = "tidyData.txt", row.names=FALSE, sep=" ")



        
     