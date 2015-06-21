library(dplyr)

#Reading training and test sets from files
X_train <- read.table("./UCI HAR Dataset/train/X_train.txt", quote="\"")
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt", quote="\"")
X_test <- read.table("./UCI HAR Dataset/test/X_test.txt", quote="\"")
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt", quote="\"")
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt", quote="\"")
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt", quote="\"")
features <- read.table("./UCI HAR Dataset/features.txt", quote="\"", stringsAsFactors=FALSE)
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt", quote="\"", stringsAsFactors=FALSE)

#Merging the training and the test sets to create one data set
#Mergeing X_train and X_test
X_merged <- rbind(X_train, X_test)
#Merging Y_train and Y_test
y_merged <- rbind(y_train, y_test)
#Merging subject_train and subject_test
subject_merged <- rbind(subject_train, subject_test)
#Merging X_merged, subject_merged and y_merged
Merged <- cbind (X_merged, subject_merged, y_merged)

#Labeling the data set with descriptive variable names
names(Merged) <- c(features[, 2], "Subject", "Activity")

#Extracting the measurements on the mean and standard deviation for each measurement
Ext <- Merged[, c(1:6, 41:46, 81:86, 121:126, 161:166, 201, 202, 214, 215, 227, 
                  228, 240, 241, 253, 254, 266:271, 345:350, 424:429, 503, 504, 
                  516, 517, 529, 530, 542, 543, 562, 563)]

#Naming the activities in the data set with descriptive activity names
Temp <- character(dim(Ext)[1])
for (i in 1:dim(Ext)[1]){
    Temp[i] <- activity_labels[Ext[i, "Activity"], 2]
}
Ext$Activity <- Temp

#Forming independent tidy data set with the average of each variable for each activity and each subject
#Aggregating by Subject and Activity with mean fuction
Tidy_data_set <- aggregate(select(Ext, -(Subject:Activity)), list(Ext$Subject, Ext$Activity), FUN = "mean")
#"Reparing" names of the first two columns
names(Tidy_data_set)[1:2] <- c("Subject", "Activity")
#Rearranging rows
Tidy_data_set <- arrange(Tidy_data_set, Subject, Activity)

#Creating a txt file
write.table(Tidy_data_set, "Tidy_data_set.txt", row.name=FALSE)