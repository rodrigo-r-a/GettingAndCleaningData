#Course Project
setwd("~/Desktop/UFM/coursera/run_analysis") #set the working directory
#read all of the sets that will be used
s_test <- read.table("subject_test.txt") 
s_train <- read.table("subject_train.txt")
x_test <- read.table("X_test.txt")
x_train <- read.table("X_train.txt")
y_test <- read.table("y_test.txt")
y_train <- read.table("y_train.txt")
activity <- read.table("activity_labels.txt")
features <- read.table("features.txt")
#checkout names and dims for the data sets
names(s_test)
names(s_train)
names(x_test)
dim(x_test)
names(x_train)
dim(x_train)
names(y_test)
names(y_train)
names(activity)
names(features)
#asign names to the columns of the sets
colnames(activity)  = c('activityId','activityType');
colnames(s_train)  = "subjectId";
colnames(x_train)        = features[,2]; 
colnames(y_train)        = "activityId";
colnames(s_test) = "subjectId";
colnames(x_test)       = features[,2]; 
colnames(y_test)       = "activityId";
#create the train and test datasets thorugh cbind
train <- cbind(s_train, x_train, y_train)
test <- cbind(s_test, x_test, y_test)
#1. Merges the training and the test sets to create one data set.
data_u <- rbind(test, train)
#create a vector for the names of the data set that'll be used to extract the mean and sd from the columns
colNames <- names(data_u)
#2. Extracts only the measurements on the mean and standard deviation for each measurement.
v_msd <- (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames));
data_final <- data_u[v_msd == TRUE]
#3. Uses descriptive activity names to name the activities in the data set
#merge data_final with activity to have descriptive sctivity names
df_a <- merge(data_final, activity, by="activityId")
colNames <- names(df_a)
# 4. Appropriately label the data set with descriptive activity names.
for (i in 1:length(colNames)) 
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StdDev",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","time",colNames[i])
  colNames[i] = gsub("^(f)","freq",colNames[i])
  colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
}
names(df_a) = colNames
#5. Create a second, independent tidy data set with the average of each variable for each activity and each subject.
df_noAct  = df_a[,names(df_a) != 'activityType']

# Summarizing the finalDataNoActivityType table to include just the mean of each variable for each activity and each subject
tidyData    = aggregate(df_noAct[,names(df_noAct) != c('activityId','subjectId')],by=list(activityId=df_noAct$activityId,subjectId = df_noAct$subjectId),mean)

# Merging the tidyData with activityType to include descriptive acitvity names
tidyData    = merge(tidyData,activity,by='activityId',all.x=TRUE)

# Export the tidyData set 
write.table(tidyData, './tidyData.txt',row.names=TRUE,sep='\t')
dim(tidyData)
dim(df_a)
dim(data_final)
