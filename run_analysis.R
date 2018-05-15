#Load needed libraries
library("dplyr")

#Read Train Data
X_train <- read.table("./Dataset/train/X_train.txt")
y_train <- read.table("./Dataset/train/y_train.txt")
subject_train <- read.table("./Dataset/train/subject_train.txt")

#Read Test Data
X_test <- read.table("./Dataset/test/X_test.txt")
y_test <- read.table("./Dataset/test/y_test.txt")
subject_test <- read.table("./Dataset/test/subject_test.txt")

#1. Merge training and test datasets
X_all <- rbind(X_train, X_test)
y_all <- rbind(y_train, y_test)
subject_all <- rbind(subject_train, subject_test) 
names(subject_all) <- c("subject")
# Clean up unwanted data as a best practice
rm("X_train", "y_train", "subject_train", "X_test", "y_test", "subject_test")
#####################################################################################################

#2. Extract only the measurements on the mean and standard deviation for each measurement

#All data is in X_all. Column names only show V1..V561. To figure out which columns represent
#mean and standard deviation, read features.txt file
col_names_all <- read.table("./Dataset/features.txt")

#Subset column names that represent mean and standard deviation
#Second column of col_names_all has column name details
col_indices_mean_std <- grep("mean\\(\\)|std\\(\\)", col_names_all[,2])

#Subset X; extract measurements and standard deviations
X_all <- X_all[, col_indices_mean_std]
#####################################################################################################

# 3. Use descriptive activity names to name the activities in the data set

#Read activity labels 
activity_labels <- read.table("./Dataset/activity_labels.txt")

#Add column "Activity" that will have description of activity
y_all$activitylabel <- factor(y_all$V1, labels = as.character(activity_labels[,2]))
y_all <- select(y_all, matches("activitylabel"))
#####################################################################################################

#4. Appropriately labels the data set with descriptive variable names
names(X_all) <- as.character(col_names_all[col_indices_mean_std,]$V2)
names(X_all) <- tolower(names(X_all))
names(X_all) <- gsub("-", "", names(X_all))
names(X_all) <- gsub("\\(\\)", "", names(X_all))
#####################################################################################################


#5. From the data set in step 4, creates a second, independent tidy data set 
# with the average of each variable for each activity and each subject

X_final <- cbind(X_all, y_all, subject_all)
output <- X_final %>% 
    group_by(activitylabel, subject) %>%    #Groups by activity and subject
    summarise_all(mean)                     #gives average of each variable

write.table(output, file = "./tidydata.txt", row.names = FALSE, col.names = TRUE)


