library(data.table)
library(dplyr)

#Read in train data
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt", header = FALSE)
Feature_train <- read.table("./UCI HAR Dataset/train/X_train.txt", header = FALSE)
Activity_train <- read.table("./UCI HAR Dataset/train/y_train.txt", header = FALSE)

#Read in test data
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt", header = FALSE)
Feature_test <- read.table("./UCI HAR Dataset/test/X_test.txt", header = FALSE)
Activity_test <- read.table("./UCI HAR Dataset/test/y_test.txt", header = FALSE)

#Read in Metadata
Feature_Variable <- read.table("./UCI HAR Dataset/features.txt", header = FALSE)
Activity_Variable <- read.table("./UCI HAR Dataset/activity_labels.txt", header = FALSE)
names(Activity_Variable) <- c("Activity_Label", "Activity_Name")
 
#Part 1 - Merge the training and the test sets to create one data set
Subject <- rbind(subject_train, subject_test)
names(Subject) <- "Subject"

Activity <- rbind(Activity_train, Activity_test)
names(Activity) <- "Activity"

Feature <- rbind(Feature_train, Feature_test)
names(Feature) <- Feature_Variable$V2

AllData <- cbind(Subject, Activity, Feature)

#Part 2 - Extracts only the measurements on the mean and standard deviation for each measurement
row_Mean_Std <- grep(".*Mean.*|*Std", names(AllData),ignore.case = T, value = T)
row_Required <- c("Subject", "Activity", row_Mean_Std) 
Required_Data <- AllData[ ,row_Required]  

#Part 3 - Uses descriptive activity names to name the activities in the data set
Required_Data <- merge(Activity_Variable, Required_Data , 
                       by.x = "Activity_Label", by.y = "Activity", all.x = TRUE)

#Part 4 - Appropriately labels the data set with descriptive variable names
names(Required_Data) <- gsub("-mean", "Mean", names(Required_Data))
names(Required_Data) <- gsub("-std", "Std", names(Required_Data)) 
names(Required_Data) <- gsub("^f", "Frequency", names(Required_Data))
names(Required_Data) <- gsub("^t", "Time", names(Required_Data))
names(Required_Data) <- gsub("^angle.t", "angle(Time", names(Required_Data))
names(Required_Data)<-gsub("Acc", "Accelerometer", names(Required_Data))
names(Required_Data)<-gsub("Gyro", "Gyroscope", names(Required_Data))
names(Required_Data)<-gsub("BodyBody", "Body", names(Required_Data))
names(Required_Data)<-gsub("Mag", "Magnitude", names(Required_Data))

#Part 5 - From the data set in step 4, creates a second, independent tidy data set 
#with the average of each variable for each activity and each subject
TidyData <- aggregate(. ~ Subject + Activity_Name, data = Required_Data, mean) 
TidyData <- TidyData[order(TidyData$Subject,TidyData$Activity_Name),]
write.table(TidyData, file = "TidyData.txt", row.names = FALSE)



