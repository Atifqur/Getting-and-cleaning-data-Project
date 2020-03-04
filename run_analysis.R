# Loading libraries
library(data.table)
library(dplyr)

# Setting working Directory
setwd("C:\\Users\\Documents\\Data")

# Creating folder and Downloading UCI data files from the web
folder<-"./cleaning_n_getting_Dataset_project"
if(!dir.exists(folder)) {
dir.create(folder)}

# Setting working Directory
setwd("C:\\Users\\Documents\\Data\\cleaning_n_getting_Dataset_project")

URL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
destFile <- "./project_Dataset.zip"
if(!file.exists(destFile))
{
download.file(URL, destfile = destFile, mode="wb")
}

# Unzipping data files
if (!file.exists("UCI HAR Dataset")){
  unzip(destFile)
}

# Specifying time/date
dateDownloaded <- date()


# Reading Activity labels code for test and train data. code ranges from 1 to 6
# 1 WALKING
# 2 WALKING_UPSTAIRS
# 3 WALKING_DOWNSTAIRS
# 4 SITTING
# 5 STANDING
# 6 LAYING
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt", header = FALSE)
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt", header = FALSE)


# Reading 561 features data in coloumns which is normalized and bounded within [-1,1]
feature_test <- read.table("./UCI HAR Dataset/test/X_test.txt", header = FALSE)
feature_train <- read.table("./UCI HAR Dataset/train/X_train.txt", header = FALSE)


# Reading Each row of subject who performed the activity for each window sample, ranges from 1 to 30
# Train contains 70% and Test contains 30% subjects
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt", header = FALSE)
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt", header = FALSE)


# Reading six Activity Label Names & 561 Features Name
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt", header = FALSE)
features_name <- read.table("./UCI HAR Dataset/features.txt", header = FALSE)


# Merging the training (70%) and the test (30%) data sets to create one data set (100%)
feature_dat <- rbind(feature_test, feature_train)   # Data comprised of 561 features name
subject_dat <- rbind(subject_test, subject_train)   # Subject ranges from 1 to 30
activity_dat <- rbind(y_test, y_train)              # activity ranges from 1 to 6

# Step-3 Uses descriptive activity names to name the activities in the data set
names(activity_dat) <- "activity_code"
names(activity_labels) <- c("activity_code", "Activity")

# Joining Activity data and activity names through activity code
Activity <- left_join(activity_dat, activity_labels, "activity_code")[, 2]

# Renaming subject_dat & feature_dat
names(subject_dat) <- "Subject"
names(feature_dat) <- features_name$V2


# Step-1 Create one large Dataset with only these variables: subject_dat,  Activity,  feature_dat
Large_dataset <- cbind(subject_dat, Activity,feature_dat)

# Step-2 Extracts only the measurements on the mean and standard deviation for each measurement.
features_name_mean_std <- features_name$V2[grep("\\bmean()\\b|\\bstd()\\b", features_name$V2)]
DataNames <- c("Subject", "Activity", as.character(features_name_mean_std))
Large_dataset <- subset(Large_dataset, select=DataNames)

# Step-4 Appropriately labels the data set with descriptive variable names. 
names(Large_dataset)<-gsub("^t", "Time", names(Large_dataset))
names(Large_dataset)<-gsub("^f", "Frequency", names(Large_dataset))
names(Large_dataset)<-gsub("Acc", "Accelerometer", names(Large_dataset))
names(Large_dataset)<-gsub("Gyro", "Gyroscope", names(Large_dataset))
names(Large_dataset)<-gsub("Mag", "Magnitude", names(Large_dataset))
names(Large_dataset)<-gsub("BodyBody", "Body", names(Large_dataset))

# Step-5 From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
SecondLarge_dataset<-aggregate(. ~Subject + Activity, Large_dataset, mean)
SecondLarge_dataset<-SecondLarge_dataset[order(SecondLarge_dataset$Subject,SecondLarge_dataset$Activity),]

# writing tidy dataset and saving as text file
write.table(SecondLarge_dataset, file = "tidydata.txt",row.name=FALSE)