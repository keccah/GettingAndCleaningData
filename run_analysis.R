#This script assumes you have all the data for the project in a folder 
#in your working directory called "UCI_HAR_Dataset"

#STEP 0 I'm joining all info about train

train_subjects = read.table("./UCI_HAR_Dataset/train/subject_train.txt", col.names=c("subject"))
train_subjects$ID <- as.integer(1:nrow(train_subjects))
train_set <- read.table("./UCI_HAR_Dataset/train/X_train.txt")
train_set$ID <- as.integer(1:nrow(train_set))
train_labels = read.table("./UCI_HAR_Dataset/train/y_train.txt", col.names=c("activity"))
train_labels$ID <- as.integer(1:nrow(train_labels))
library(plyr)
train <- join_all(list(train_subjects, train_labels))
train <- join_all(list(train, train_set))

#STEP 0 joining all info about test

test_subjects = read.table("./UCI_HAR_Dataset/test/subject_test.txt", col.names=c("subject"))
test_subjects$ID <- as.integer(1:nrow(test_subjects))
test_set = read.table("./UCI_HAR_Dataset/test/X_test.txt")
test_set$ID <- as.integer(1:nrow(test_set))
test_labels = read.table("./UCI_HAR_Dataset/test/y_test.txt", col.names=c("activity"))  
test_labels$ID <- as.integer(1:nrow(test_labels))
test <- join_all(list(test_subjects, test_labels)) 
test <- join_all(list(test, test_set)) 

#STEP 1 Merges the training and the test sets to create one data set.

library(dplyr)
train_test <- rbind(train, test)

#STEP 2 Extracts only the measurements on the mean and standard deviation for each measurement.

features <- read.table("./UCI_HAR_Dataset/features.txt", col.names=c("feature", "label"))
mean_sd_labels <- features[grepl("mean\\(\\)", features$label) | grepl("std\\(\\)", features$label), ]
mean_sd <- train_test[, c(c(1, 2, 3), mean_sd_labels$feature + 3) ]
activity_labels <- read.table("./UCI_HAR_Dataset/activity_labels.txt", col.names=c("activity", "label")) 
train_test_v2 <- join_all(list(mean_sd, activity_labels))

#STEP 3 Uses descriptive activity names to name the activities in the data set

mean_sd_labels$label <- gsub("\\(\\)", "", mean_sd_labels$label)
mean_sd_labels$label <- gsub("-", ".", mean_sd_labels$label)
for (i in 1:length(mean_sd_labels$label)) {
        colnames(train_test_v2)[i + 3] <- mean_sd_labels$label[i]
}
final_train_test <- train_test_v2

#sTEP 4 Appropriately labels the data set with descriptive variable names.

remove <- c("ID","label")
onlynumbers <- final_train_test[,!(names(final_train_test) %in% remove)]
mean_numbers <- aggregate(onlynumbers, by=list(subject_id = onlynumbers$subject, activity_id = onlynumbers$activity), FUN=mean, na.rm=TRUE)
remove <- c("subject_id","activity_id")
mean_numbers <- mean_numbers[,!(names(mean_numbers) %in% remove)]
mean_numbers <- join_all(list(mean_numbers, activity_labels))
mean_numbers <- mean_numbers[c(1,2,69,3:68)]

#STEP 5 From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

library(plyr)
average_numbers <- ddply(mean_numbers, .(subject, activity), function(x) colMeans(x[, 4:69]))

write.csv(file="samsung_wearables.csv", x=average_numbers)
