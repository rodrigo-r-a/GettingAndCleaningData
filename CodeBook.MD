#Getting and Cleaning Data
###Rodrigo Rivera
### Description

### Data Information
The full description of the dataset can be found [here.](http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones)

The data for download can be found [here.](https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip)

#Features
The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 

Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 

Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals). 

These signals were used to estimate variables of the feature vector for each pattern:  
'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.

### 1, merge the training and the test sets to create one data set.
First of, the tables were read from the working directory (features.txt; activity_labels.txt; subject_train.txt; x_train.txt; y_train.txt; subject_test.txt; x_test.txt ;y_test.txt).
After, the columns where named to get the data tidy. Then, the data sets for train and test where joined by "rbind".

## 2, extract only the measurements on the mean and standard deviation for each measurement. 
Create a logcal vector that contains TRUE values for the ID, mean and stdev columns and FALSE values for the other columns to eliminate non mean and non sd values.

## 3, use descriptive activity names to name the activities in the data set
Merge the new data set with the activityType table to get the activities named.

## Section 4. Appropriately label the data set with descriptive activity names.
Gsub function to clean up the dataset.

## Section 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 
Per the project instructions, we need to produce only a data set with the average of each veriable for each activity and subject
