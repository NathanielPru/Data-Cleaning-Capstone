getwd()
setwd("C:/Users/gfdas/OneDrive/Desktop/Data Science/R Projects/JHU---Data-Science-Cert/Data Cleaning Capstone")

trainx<-read.table("UCI HAR Dataset/train/X_train.txt")

trainnames<-read.table("UCI HAR Dataset/features.txt")
colnames(trainx)<- trainnames$V2
