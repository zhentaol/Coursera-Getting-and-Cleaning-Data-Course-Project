
library(dplyr)

#1. Downloading and unzipping dataset#

zipUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipFile <- "UCI HAR Dataset.zip"

if (!file.exists(zipFile)) {
   download.file(zipUrl, zipFile, mode = "wb")
}

#Unzip
datapath <- "UCI HAR Dataset"
if(!file.exists(datapath)){
unzip(zipFile)
}

#1. Merges the training and test sets to create one data set

xtrain <- read.table(file.path(datapath , "train" , "X_train.txt"), col.names = features$functions)
ytrain <- read.table(file.path(datapath , "train" , "y_train.txt"), col.names = "activityId")
subjecttrain <- read.table(file.path(datapath , "train" , "subject_train.txt"), col.names = "subjectId")

xtest <- read.table(file.path(datapath , "test" , "X_test.txt"), col.names = features$functions)
ytest <- read.table(file.path(datapath , "test" , "y_test.txt"), col.names = "activityId")
subjecttest <- read.table(file.path(datapath , "test" , "subject_test.txt"), col.names = "subjectId")

activitylabels <- read.table(file.path(datapath , "activity_labels.txt"), col.names = c("activityId" , "activityType"))
features <- read.table(file.path(datapath , "features.txt") , col.names = c("n" , "functions"))

#Merging the train and test data
X <- rbind(xtrain, xtest)
Y <- rbind(ytrain, ytest)
Subject <- rbind(subjecttrain, subjecttest)

merge_data <- cbind(Subject, X, Y)

#2. Extracting only the measurements on the mean and standard deviation for each measurement

Tidy_Data <- merge_data %>% select(subjectId, activityId, contains("mean"), contains("std"))

#3. Uses descriptive activity names to name the activities in the data set
Tidy_Data$activityId <- activitylabels[Tidy_Data$activityId, 2]

#Step 4: Appropriately labels the data set with descriptive variable names.
names(Tidy_Data)[2] = "activityId"
names(Tidy_Data)<-gsub("Acc", "Accelerometer", names(Tidy_Data))
names(Tidy_Data)<-gsub("Gyro", "Gyroscope", names(Tidy_Data))
names(Tidy_Data)<-gsub("BodyBody", "Body", names(Tidy_Data))
names(Tidy_Data)<-gsub("Mag", "Magnitude", names(Tidy_Data))
names(Tidy_Data)<-gsub("^t", "Time", names(Tidy_Data))
names(Tidy_Data)<-gsub("^f", "Frequency", names(Tidy_Data))
names(Tidy_Data)<-gsub("tBody", "TimeBody", names(Tidy_Data))
names(Tidy_Data)<-gsub("-mean()", "Mean", names(Tidy_Data), ignore.case = TRUE)
names(Tidy_Data)<-gsub("-std()", "STD", names(Tidy_Data), ignore.case = TRUE)
names(Tidy_Data)<-gsub("-freq()", "Frequency", names(Tidy_Data), ignore.case = TRUE)
names(Tidy_Data)<-gsub("angle", "Angle", names(Tidy_Data))
names(Tidy_Data)<-gsub("gravity", "Gravity", names(Tidy_Data))

#Step 5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# Create a dataset with the mean of each column for 'subjectId' and 'activityId'
Final_Data <- Tidy_Data %>%
	group_by(subjectId,activityId) %>%
	summarise_all(funs(mean))

# Save the data frame created as a text file in working directory
write.table(Final_Data,"Final_Data.txt",row.name=FALSE)

message("The script was executed successfully. A new tidy data set "Final_Data.txt" was successfully created in the working directory.")
