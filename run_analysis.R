
# Set working directory, Download and unzip Dataset:
setwd("C:/Users/pjc/Documents/GettingData/UCI HAR Dataset/")

library(downloader)

download("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", destfile="Getting_Dataset.zip")
filename <- "Getting_Dataset.zip"

if (!file.exists("GettingData")) { 
  unzip(filename) 
}
##unzipped files are in the folderUCI HAR Dataset

# #imports features.txt
features     = read.table('./features.txt',header=FALSE); 
#imports activity_labels.txt
activityType = read.table('./activity_labels.txt',header=FALSE);
#imports subject_train.txt
subjectTrain = read.table('./train/subject_train.txt',header=FALSE); 
#imports x_train.txt
xTrain = read.table('./train/x_train.txt',header=FALSE);
#imports y_train.txt
yTrain = read.table('./train/y_train.txt',header=FALSE); 

# Set column names for imported data
colnames(activityType)  = c('activityId','activityType');
colnames(subjectTrain)  = "subjectId";
colnames(xTrain)        = features[,2]; 
colnames(yTrain)        = "activityId";

# Create final training set by merging yTrain, subjectTrain, and xTrain
trainingData = cbind(yTrain,subjectTrain,xTrain);

#imports subject_test.txt
subjectTest = read.table('./test/subject_test.txt',header=FALSE);
#imports x_test.txt
xTest       = read.table('./test/x_test.txt',header=FALSE);
#imports y_test.txt
yTest       = read.table('./test/y_test.txt',header=FALSE); 

# Set column names for imported test data 
colnames(subjectTest) = "subjectId";
colnames(xTest)       = features[,2]; 
colnames(yTest)       = "activityId";

# Create final test set by merging xTest, yTest and subjectTest data
testData = cbind(yTest,subjectTest,xTest);

# Merge training and test data 
finalData = rbind(trainingData,testData);

#Subset Name of Features by measurements on the mean and standard deviation
# 2. Extract only the measurements on the mean and standard deviation for each measurement. 
# Create a logicalVector that contains TRUE values for the ID, mean() & stddev() columns and FALSE for others

colNames = colnames(finalData); 
logicalVector = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames));

# Subset finalData table based on the logicalVector to keep only desired columns
finalData = finalData[logicalVector==TRUE];

# 3. Use descriptive activity names to name the activities in the data set
# Merge the finalData set with the acitivityType table to include descriptive activity names
finalData = merge(finalData,activityType,by='activityId',all.x=TRUE);

# Updating the colNames vector to include the new column names after merge
colNames  = colnames(finalData); 

# 4. Appropriately label the data set with descriptive activity names. 

# Cleaning up the variable names
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
};

# Set new descriptive column names for finalData set
colnames(finalData) = colNames;

# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 
# Create a new table, finalDataNoActivityType without the activityType column
finalDataNoActivityType  = finalData[,names(finalData) != 'activityType'];

# Summarize finalDataNoActivityType table to include just the mean of each variable for each activity and each subject
tidyData = aggregate(finalDataNoActivityType[,names(finalDataNoActivityType) != c('activityId','subjectId')],by=list(activityId=finalDataNoActivityType$activityId,subjectId = finalDataNoActivityType$subjectId),mean);

# Merge tidyData with activityType to include descriptive acitvity names
tidyData = merge(tidyData,activityType,by='activityId',all.x=TRUE);

# Export the tidyData set 
write.table(tidyData, './tidyData.txt',row.names=TRUE,sep='\t');