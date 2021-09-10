#******************************************************************
#Step 0. Downloading and unzipping dataset
#******************************************************************

if(!file.exists("./data")){dir.create("./data")}

fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

download.file(fileUrl,destfile="./data/Dataset.zip",method="curl")

#Unziping the file

unzip(zipfile="./data/Dataset.zip",exdir="./data")

#getting the list of the files

path_rf <- file.path("./data" , "UCI HAR Dataset")
files<-list.files(path_rf, recursive=TRUE)
files

#******************************************************************
#Step 1.Merges the training and the test sets to create one data set.
#******************************************************************

#Reading files

##Reading Activity files for lables:

dataActivityTest  <- read.table(file.path(path_rf, "test" , "Y_test.txt" ),header = FALSE)
dataActivityTrain <- read.table(file.path(path_rf, "train", "Y_train.txt"),header = FALSE)

## Reading Subject files:

dataSubjectTrain <- read.table(file.path(path_rf, "train", "subject_train.txt"),header = FALSE)
dataSubjectTest  <- read.table(file.path(path_rf, "test" , "subject_test.txt"),header = FALSE)

##Read Fearures files

dataFeaturesTest  <- read.table(file.path(path_rf, "test" , "X_test.txt" ),header = FALSE)
dataFeaturesTrain <- read.table(file.path(path_rf, "train", "X_train.txt"),header = FALSE)

#Look at the properties of the variables

str(dataActivityTest)

str(dataActivityTrain)

str(dataSubjectTest)

str(dataSubjectTrain)

str(dataFeaturesTest)

str(dataFeaturesTrain)

#Merging all data in one set:

dataSubject <- rbind(dataSubjectTrain, dataSubjectTest)

dataActivity<- rbind(dataActivityTrain, dataActivityTest)

dataFeatures<- rbind(dataFeaturesTrain, dataFeaturesTest)

#******************************************************************
#Step 2.-Extracts only the measurements on the mean and standard deviation for each measurement.
#******************************************************************


#set names to variables

names(dataSubject)<-c("subject")

names(dataActivity)<- c("activity")

dataFeaturesNames <- read.table(file.path(path_rf, "features.txt"),head=FALSE)

names(dataFeatures)<- dataFeaturesNames$V2

#Merge columns to data frame

dataCombine <- cbind(dataSubject, dataActivity)

Data <- cbind(dataFeatures, dataCombine)

#Create vector for defining ID, mean and standard deviation:

subdataFeaturesNames<-dataFeaturesNames$V2[grep("mean\\(\\)|std\\(\\)", dataFeaturesNames$V2)]

selectedNames<-c(as.character(subdataFeaturesNames), "subject", "activity" )

Data<-subset(Data,select=selectedNames)

#Check the data frame.

str(Data)

#******************************************************************
#Step 3. Uses descriptive activity names to name the activities in the data set
#******************************************************************

activityLabels <- read.table(file.path(path_rf, "activity_labels.txt"),header = FALSE)

#check
head(Data$activity,30)

#******************************************************************
#Step 4. Appropriately labels the data set with descriptive variable names.
#******************************************************************
#*
names(Data)<-gsub("^t", "time", names(Data))
names(Data)<-gsub("^f", "frequency", names(Data))
names(Data)<-gsub("Acc", "Accelerometer", names(Data))
names(Data)<-gsub("Gyro", "Gyroscope", names(Data))
names(Data)<-gsub("Mag", "Magnitude", names(Data))
names(Data)<-gsub("BodyBody", "Body", names(Data))

#check
names(Data)

#******************************************************************
#Step 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
#******************************************************************


library(dplyr)

Data2<-aggregate(. ~subject + activity, Data, mean)
Data2<-Data2[order(Data2$subject,Data2$activity),]
write.table(Data2, file = "tidydata.txt",row.name=FALSE)
