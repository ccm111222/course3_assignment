#load library
library(dplyr)

#Load all the data
features <- read.table("features.txt", col.names = c("Number", "feature_type"))
activity_label <- read.table("activity_labels.txt", col.names = c("code", "activities"))
subject_train <- read.table("./train/subject_train.txt", col.names = "subjects")
subject_test <- read.table("./test/subject_test.txt", col.names = "subjects")
x_train <- read.table("./train/X_train.txt", col.names = features$feature_type)
y_train <- read.table("./train/y_train.txt", col.names = "code")
x_test <- read.table("./test/X_test.txt", col.names = features$feature_type)
y_test <- read.table("./test/y_test.txt", col.names = "code")

#Combine the loaded data into a single data frame
test <- cbind(subject_test, y_test, x_test)
train <- cbind(subject_train, y_train, x_train)
train_test <- rbind(train, test)

#Select the columns that contains "mean" or "std"
mean_std <- c(TRUE,TRUE,grepl("mean|std",features$feature_type))
selected_file <-train_test[,mean_std]

#Change the activity code into activity name
activity_name=vector()
for (i in 1:nrow(selected_file)){
  aaa<-selected_file$code[i]==activity_label$code
  activity_name<-c(activity_name, activity_label$activities[aaa])
}
selected_file$code <- activity_name

#Change the column names to descriptive names
names(selected_file)[1] <- "People_object_ID"
names(selected_file)[2] <- "Activity"
names(selected_file)<-gsub("std","StandardDeviation", names(selected_file))
names(selected_file)<-gsub("Acc","_Accelerometer", names(selected_file))
names(selected_file)<-gsub("Gyro","_Gyroscope", names(selected_file))
names(selected_file)<-gsub("Mag","_Magnitude", names(selected_file))
names(selected_file)<-gsub("Freq","Frequency", names(selected_file))
names(selected_file)<-gsub("^f","Frequency_", names(selected_file))
names(selected_file)<-gsub("^t","Time_", names(selected_file))
names(selected_file)<-gsub("X$","X_axis", names(selected_file))
names(selected_file)<-gsub("Y$","Y_axis", names(selected_file))
names(selected_file)<-gsub("Z$","Z_axis", names(selected_file))
names(selected_file)<-gsub(".mean","_mean", names(selected_file))
names(selected_file)<-gsub(".StandardDeviation","_StandardDeviation", names(selected_file))
names(selected_file)<-gsub("BodyBody","Body", names(selected_file))
names(selected_file)<-gsub("Jerk","_Jerk", names(selected_file))

#Save the data as a data frame called "tidyTable"
tidyTable<-selected_file
write.csv(tidyTable, "tidyTable.csv")

#Creates a second, independent tidy data set (named "tidyTable2") with the average of 
#each variable for each activity and each subject.
mean_activity_object<-tidyTable %>%
  group_by(Activity, People_object_ID) %>%
  summarize_all(mean)
tidyTable2 <- mean_activity_object
write.csv(tidyTable2, "tidyTable2.csv")
