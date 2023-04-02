
library(dplyr)

#Read the train data
trainx<-read.table("UCI HAR Dataset/train/X_train.txt")


#Read and store variable names for train data
trainnames<-read.table("UCI HAR Dataset/features.txt")
colnames(trainx)<- trainnames$V2


#Read subject for each record
subjectTrain<-read.table("UCI HAR Dataset/train/subject_train.txt")

#Add subjects to training data
trainx<-cbind(Subject = subjectTrain[,1], trainx)


## Remove all columns except subject, mean, 
## and standard deviation for each measure
trainx <- trainx[,c(1:7,42:47,82:87,122:127,162:167,
                  202,203,215,216,228,229,241,242,254,255,
                  267:272,346:351,425:430,504,505,517,518,
                  530,531,543,544)]

# Relabel columns to be more readable
library(stringr)

measureNames <- as.character(colnames(trainx))
replacements <- c("Body"="Body ","Gravity"="Gravity ", 
                  "Acc"="Accelerometer ","Gyro"="Gyroscope ", "Jerk"="Jerk "
                  ,"Mag"="Magnitude ",'mean'=" Mean"
                  ,'std'=" Standard Deviation")

measureNames <- gsub("[()]","",measureNames)
measureNames[2:41] <-str_replace(measureNames[2:41],"t","Time ")
measureNames[42:length(measureNames)] <-str_replace(measureNames[42:length(measureNames)],"f" , "FFT ")
measureNames <- str_replace_all(measureNames,replacements)

measureNames

colnames(trainx)<-measureNames

#Read activity Labels and the activities for each measure
actLabels<-read.table("UCI HAR Dataset/activity_labels.txt")
trainy<-read.table("UCI HAR Dataset/train/y_train.txt")

trainx$Activity<-trainy$V1

trainx$Activity

for (i in 1:length(trainx$Activity)){
  trainx$Activity[i]<- actLabels$V2[match(trainx$Activity[i],actLabels$V1)]
}

##############################################################################
#Do the same but for the test data
#Read the train data
testx<-read.table("UCI HAR Dataset/test/X_test.txt")


#Read and store variable names for train data
testnames<-read.table("UCI HAR Dataset/features.txt")
colnames(testx)<- testnames$V2


#Read subject for each record
subjectTest<-read.table("UCI HAR Dataset/test/subject_test.txt")

#Add subjects to training data
testx<-cbind(Subject = subjectTest[,1], testx)


## Remove all columns except subject, mean, 
## and standard deviation for each measure
testx <- testx[,c(1:7,42:47,82:87,122:127,162:167,
                    202,203,215,216,228,229,241,242,254,255,
                    267:272,346:351,425:430,504,505,517,518,
                    530,531,543,544)]

# Relabel columns to be more readable

measureNamesTest <- as.character(colnames(testx))
replacementsTest <- c("Body"="Body ","Gravity"="Gravity ", 
                  "Acc"="Accelerometer ","Gyro"="Gyroscope ", "Jerk"="Jerk "
                  ,"Mag"="Magnitude ",'mean'=" Mean"
                  ,'std'=" Standard Deviation")

measureNamesTest <- gsub("[()]","",measureNamesTest)
measureNamesTest[2:41] <-str_replace(measureNamesTest[2:41],"t","Time ")
measureNamesTest[42:length(measureNamesTest)] <-str_replace(measureNamesTest[42:length(measureNamesTest)],"f" , "FFT ")
measureNamesTest <- str_replace_all(measureNamesTest,replacementsTest)

colnames(testx)<-measureNamesTest

#Read activity Labels and the activities for each measure
actLabelsTest<-read.table("UCI HAR Dataset/activity_labels.txt")
testy<-read.table("UCI HAR Dataset/test/y_test.txt")

testx$Activity<-testy$V1

for (i in 1:length(testx$Activity)){
  testx$Activity[i]<- actLabelsTest$V2[match(testx$Activity[i],actLabelsTest$V1)]
}

###############################################################################
#Merge the train and test data
fullx<-rbind(trainx,testx)

###############################################################################
#Create a dataset with the averages for each subject and activity

#Create column names
fullColNames<-c("meanSubAct",colnames(fullx[2:67]))

#create a dataframe with all the variables that we will take the mean of
temp<-data.frame(matrix(ncol=66,nrow = 0))
colnames(temp)<-fullColNames[2:67]

#take the mean of all the variables for all the subjects
for (i in 1:30){
 
  temp[i,]<- apply(fullx[fullx$Subject==i,2:67], MARGIN = 2, mean, na.rm=TRUE)
  
}
#take the mean of all the variables for all the activities
for (i in 31:36){
  
  temp[i,]<- apply(fullx[fullx$Activity == actLabels[(i-30),2],2:67], MARGIN = 2, mean, na.rm=TRUE)
  
}

#add a new column labeling the rows with their respective subject or activity
temp<-cbind(meanSubAct = c(1:30,actLabels$V2), temp)
for (i in 1:36){
  temp[i,1]<-paste(temp[i,1]," Mean")
}

#export the data
write.table(temp,file = "tidy.txt",row.names = FALSE)
