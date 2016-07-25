#fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
#download.file(fileUrl,destfile="./Dataset.zip")
#setwd('C:/Users/Ranjana/Documents/Coursera/HumanActRecog');

#unzip(zipfile="./Dataset.zip")
# 	path <- file.path("." , "UCI HAR Dataset")
#	files<-list.files(path)

library(reshape2)
##1. Merges the training and the test sets to create one data set.
# Read Data
features     = read.table('./features.txt',header=FALSE); #imports features.txt
activityType = read.table('./activity_labels.txt',header=FALSE); #imports activity_labels.txt
subjectTrain = read.table('./train/subject_train.txt',header=FALSE); #imports subject_train.txt
xTrain       = read.table('./train/x_train.txt',header=FALSE); #imports x_train.txt
yTrain       = read.table('./train/y_train.txt',header=FALSE); #imports y_train.txt

# Assign Column Names
colnames(activityType)  = c('activityId','activityType');
colnames(subjectTrain)  = "subjectId";
colnames(xTrain)        = features[,2]; 
colnames(yTrain)        = "activityId";

#Create final training set
trainingData = cbind(yTrain,subjectTrain,xTrain);
#head(trainingData,n=3)

# Read from Test Data
subjectTest = read.table('./test/subject_test.txt',header=FALSE); #imports subject_test.txt
xTest       = read.table('./test/x_test.txt',header=FALSE); #imports x_test.txt
yTest       = read.table('./test/y_test.txt',header=FALSE); #imports y_test.txt

# Assign column names 
colnames(subjectTest) = "subjectId";
colnames(xTest)       = features[,2]; 
colnames(yTest)       = "activityId";

#Create final test set 
testData = cbind(yTest,subjectTest,xTest);
#head(testData,n=3)

# Combine training and test data to create a final data set
finalData = rbind(trainingData,testData);

#Create a vector for the column names from the finalData 
colNames = colnames(finalData); 

##2. Extracts only the measurements on the mean and standard deviation for each measurement.

logicalVector = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames));

# Subset finalData table based on the logicalVector 
finalData = finalData[logicalVector==TRUE];

##3. Uses descriptive activity names to name the activities in the data set
##4. Appropriately labels the data set with descriptive variable names.

# convert the activity column from integer to factor
finalData$activity <- factor(finalData$activity, labels=c("Walking",
    "Walking Upstairs", "Walking Downstairs", "Sitting", "Standing", "Laying"))

##5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# create the tidy data set
melted <- melt(finalData, id=c("subjectId","activity"))
tidy <- dcast(melted, subjectId+activity ~ variable, mean)

# write the tidy data set to a file
write.csv(tidy, "tidy.csv", row.names=FALSE)




