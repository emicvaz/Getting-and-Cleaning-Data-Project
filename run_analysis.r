library(data.table)
library(dplyr)

#Changing my directory
setwd("/Users/myname/Downloads/UCI HAR Dataset") #Note that is important to extract the file into the UCI HAR Dataset folder.

#Reaging suporting metadata
featureNames <- read.table("features.txt")
activityLabels <- read.table("activity_labels.txt", header = FALSE) 

#Reading training and testing data
subjectTrain <- read.table("train/subject_train.txt", header = FALSE)
subjectTest <- read.table("test/subject_test.txt", header = FALSE)
activityTrain <- read.table("train/y_train.txt", header = FALSE)
activityTest <- read.table("test/y_test.txt", header = FALSE)
featuresTrain <- read.table("train/X_train.txt", header = FALSE)
featuresTest <- read.table("test/X_test.txt", header = FALSE)
#Preparating Data
subject <- rbind(subjectTrain, subjectTest)
activity <- rbind(activityTrain, activityTest)
features <- rbind(featuresTrain, featuresTest)

colnames(features) <-t(featureNames[2])
colnames(activity) <- "Activity"
colnames(subject) <- "Subject"

#Part 1 Merges the training and the test sets to create one data set.

Data <- cbind(activity,subject,features)

#Part 2 Extracts only the measurements on the mean and standard deviation for each measurement.

columnsWithmeanORstd <- grep(".*mean.*|.*std.*", names(Data), ignore.case=TRUE)
columnsWithmeanORstd
requiredColumns <- c(1, 2, columnsWithmeanORstd)
extractedData <- Data[,requiredColumns]
head(extractedData,15)
dim(Data);dim(extractedData)

#Part 3 - Uses descriptive activity names to name the activities in the data set
dim(Data);dim(extractedData)
for (i in 1:length(activityLabels)){
        extractedData$Activity[extractedData$Activity == i] <- as.character(activityLabels[i,2])
}
extractedData$Activity <- as.factor(extractedData$Activity)

#Part 4 - Appropriately labels the data set with descriptive variable names.
names(extractedData)
names(extractedData)<-gsub("Acc", "Accelerometer", names(extractedData))
names(extractedData)<-gsub("Gyro", "Gyroscope", names(extractedData))
names(extractedData)<-gsub("BodyBody", "Body", names(extractedData))
names(extractedData)<-gsub("Mag", "Magnitude", names(extractedData))
names(extractedData)<-gsub("^t", "Time", names(extractedData))
names(extractedData)<-gsub("^f", "Frequency", names(extractedData))
names(extractedData)<-gsub("tBody", "TimeBody", names(extractedData))
names(extractedData)<-gsub("-mean()", "Mean", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-std()", "STD", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-freq()", "Frequency", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("angle", "Angle", names(extractedData))
names(extractedData)<-gsub("gravity", "Gravity", names(extractedData))
names(extractedData)
#Part 5 From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

tidyData<-extractedData %>% group_by(Subject,Activity) %>%
        summarise_each(mean) 
tidyData <- tidyData[order(tidyData$Subject,tidyData$Activity,decreasing = F),]

write.table(tidyData, file = "tidy_data.txt", row.names = FALSE)


