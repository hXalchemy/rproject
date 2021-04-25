library(dplyr)
library(stringr)
library(reshape2)



#1. Merges the training and the test sets to create one data set.
Labs = read.table("UCI HAR Dataset/features.txt", col.names = c("Id","Features"))
Train = read.table("UCI HAR Dataset/train/X_train.txt", col.names = Labs$Features)
Test = read.table("UCI HAR Dataset/test/X_test.txt", col.names = Labs$Features)
ActTrain = read.table("UCI HAR Dataset/train/y_train.txt", col.names = "Features")
ActTest = read.table("UCI HAR Dataset/test/y_test.txt",col.names = "Features") 
Df = rbind(Train,Test)
feats = rbind(ActTrain,ActTest)
Df = cbind(Df,feats)
Df = cbind(Df, rbind(read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "Subject"),read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "Subject")))

rm(feats,ActTrain,ActTest, Test, Train, Labs)


#2. Extracts only the measurements on the mean and standard deviation for each measurement. 
Df2 <- Df %>% select(c(colnames(Df)[sort(c(grep("mean()",colnames(Df)), grep("std()",colnames(Df))))],"Features","Subject"))



#3. Uses descriptive activity names to name the activities in the data set

#4. Appropriately labels the data set with descriptive variable names. 

activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("Features", "DescriptiveFeatures"))
Df3 <- merge(Df2,activities, by = "Features") %>% select(-c("Features"))
rm(activities, Df, Df2)

unique(Df3$DescriptiveFeatures)

#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


PivotDF <- melt(Df3, id=c("Subject","DescriptiveFeatures"))
colnames(PivotDF) <- c("Subject", "Activity", "MeasurementName", "Observation")
tidy <- PivotDF %>% group_by(Subject,Activity,MeasurementName) %>% summarise(AverageObservation = mean(Observation))
tidy2 <- dcast(tidy, Subject+Activity ~ MeasurementName)

# write the tidy data set to a file
write.table(tidy2, "tidy.txt", row.names=FALSE)
print ("Succesfully created tidy.csv")
