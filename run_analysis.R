library(dplyr)

# reading data from UCI HAR Dataset

# First test data
xtest <- read.table("./UCI HAR Dataset/test/X_test.txt")
ytest <- read.table("./UCI HAR Dataset/test/Y_test.txt")
subjecttest <- read.table("./UCI HAR Dataset/test/subject_test.txt")

# Second train data
xtrain <- read.table("./UCI HAR Dataset/train/X_train.txt")
ytrain <- read.table("./UCI HAR Dataset/train/Y_train.txt")
subjecttrain <- read.table("./UCI HAR Dataset/train/subject_train.txt")

# Activity labels
activitylabels <- read.table("./UCI HAR Dataset/activity_labels.txt")

# Features data
features <- read.table("./UCI HAR Dataset/features.txt")


# Merges the training and the test sets to create one data set.
xtotal <- rbind(xtrain, xtest)
ytotal <- rbind(ytrain, ytest)
subjecttotal <- rbind(subjecttrain, subjecttest)

# Extracts only the measurements on the mean and standard deviation for each measurement.
var <- features[grep("mean\\(\\)|std\\(\\)",features[,2]),]
xtotal <- xtotal[,var[,1]]

# Uses descriptive activity names to name the activities in the data set
colnames(ytotal) <- "activity"
ytotal$activitylabel <- factor(ytotal$activity, labels = as.character(activitylabels[,2]))
activitylabel <- ytotal[,-1]

# Appropriately labels the data set with descriptive variable names.
colnames(xtotal) <- features[var[,1],2]

# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
colnames(subjecttotal) <- "subject"
total <- cbind(xtotal, activitylabel, subjecttotal)
totalmean <- total %>% group_by(activitylabel, subject) %>% summarize_all(funs(mean))
write.table(totalmean, file = "./tidydata.txt", row.names = FALSE, col.names = TRUE)
