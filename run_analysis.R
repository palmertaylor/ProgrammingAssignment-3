##################################################################################################

# I completed the "Getting and Cleaning Data" project in 6 steps: 1 introductory step and the 5 
# steps to follow from the assignment instructions

##################################################################################################

# STEP 1

##################################################################################################

# I downloaded the data through the zip file's URL
zipUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipFile <- "UCI HAR Dataset.zip"

if (!file.exists(zipFile)) {
  download.file(zipUrl, zipFile, mode = "wb")
}

# I unzipped the zip file
dataPath <- "UCI HAR Dataset"
if (!file.exists(dataPath)) {
  unzip(zipFile)
}

# I read in the training data
trainingSubjects <- read.table(file.path(dataPath, "train", "subject_train.txt"))
trainingValues <- read.table(file.path(dataPath, "train", "X_train.txt"))
trainingActivity <- read.table(file.path(dataPath, "train", "y_train.txt"))

# I read in the test data
testSubjects <- read.table(file.path(dataPath, "test", "subject_test.txt"))
testValues <- read.table(file.path(dataPath, "test", "X_test.txt"))
testActivity <- read.table(file.path(dataPath, "test", "y_test.txt"))

# I read in the feature vector
features <- read.table(file.path(dataPath, "features.txt"), as.is = TRUE)

# I read in the activity labels
activities <- read.table(file.path(dataPath, "activity_labels.txt"))
colnames(activities) <- c("activityId", "activityLabel")

##################################################################################################

# STEP 2

##################################################################################################

# I merged the training and test data sets to create one data set
humanActivity <- rbind(
  cbind(trainingSubjects, trainingValues, trainingActivity),
  cbind(testSubjects, testValues, testActivity)
)

rm(trainingSubjects, trainingValues, trainingActivity, 
   testSubjects, testValues, testActivity)

colnames(humanActivity) <- c("subject", features[, 2], "activity")

##################################################################################################

# STEP 3

##################################################################################################

# I extracted only the measurements on the mean and standard deviation for each measurement
columnsToKeep <- grepl("subject|activity|mean|std", colnames(humanActivity))

humanActivity <- humanActivity[, columnsToKeep]

##################################################################################################

# STEP 4

##################################################################################################

# I used descriptive activity names to name the activities in the data set
humanActivity$activity <- factor(humanActivity$activity, 
                                 levels = activities[, 1], labels = activities[, 2])

##################################################################################################

# STEP 5

##################################################################################################

# I appropriately labbelled the data set with descriptive variable names
humanActivityCols <- colnames(humanActivity)

humanActivityCols <- gsub("[\\(\\)-]", "", humanActivityCols)

humanActivityCols <- gsub("^f", "frequencyDomain", humanActivityCols)
humanActivityCols <- gsub("^t", "timeDomain", humanActivityCols)
humanActivityCols <- gsub("Acc", "Accelerometer", humanActivityCols)
humanActivityCols <- gsub("Gyro", "Gyroscope", humanActivityCols)
humanActivityCols <- gsub("Mag", "Magnitude", humanActivityCols)
humanActivityCols <- gsub("Freq", "Frequency", humanActivityCols)
humanActivityCols <- gsub("mean", "Mean", humanActivityCols)
humanActivityCols <- gsub("std", "StandardDeviation", humanActivityCols)
humanActivityCols <- gsub("BodyBody", "Body", humanActivityCols)

colnames(humanActivity) <- humanActivityCols

##################################################################################################

# STEP 6

##################################################################################################

# From the data set in step 5, I created a second, independent tidy data set with the average of
# each variable for each activity and each subject
humanActivityMeans <- humanActivity %>% 
  group_by(subject, activity) %>%
  summarize_all(funs(mean))

write.table(humanActivityMeans, "tidy_data.txt", row.names = FALSE, 
            quote = FALSE)

##################################################################################################