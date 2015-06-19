#GetData Project Script 
#Dale Millar - Riyadh, Saudi Arabia
#Set the working directory

if(!file.exists("./Project")){dir.create("./Project")}

#setwd("~/Coursera/GetData/Project")
setwd("./Project")

#Create a factor variable called activities for labeling purposes

activities<-c("WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING")
activities<-as.factor(activities)


#factor_lookup<-function(activity_level=1){
#  return (levels(activities)[activity_level])
#}

#Put the train data sets into dataframes, seperator between variables is ""
#and there are no headers

subject_train<-read.csv("./train/subject_train.txt",sep="",header=FALSE)
X_train<-read.csv("./train/X_train.txt",sep="",header=FALSE)
y_train<-read.csv("./train/y_train.txt",sep="",header=FALSE)

#Put the train data sets into dataframes, seperator between variables is ""
#and there are no headers

subject_test<-read.csv("./test/subject_test.txt",sep="",header=FALSE)
X_test<-read.csv("./test/X_test.txt",sep="",header=FALSE)
y_test<-read.csv("./test/y_test.txt",sep="",header=FALSE)

#Make the subject_train and subject_test dataframes factors and not integers to help
#with identification.  There are 21 in train group and 9 in test group - total 30 as per README.txt 

#subject_test[,1]<-as.factor(subject_test[,1])
#subject_train[,1]<-as.factor(subject_train[,1])

#Make y_train factors to reflect that they refer to activity levels

#y_train[,1]<-as.factor(y_train[,1])
#y_test[,1]<-as.factor(y_test[,1])

#col bind y_train to X_train and put in traintemp variable

traintemp<-cbind(y_train,X_train)

#Put in Activities column



#and now col bind subject_train to traintemp

traintemp<-cbind(subject_train,traintemp)

#col bind y_test to X_test and put in testtemp variable

testtemp<-cbind(y_test,X_test)

#and now col bind subject_test to testtemp

testtemp<-cbind(subject_test,testtemp)

#and bind rows together into combotemp dataframe

combotemp<-rbind(traintemp,testtemp)

#renumber columns

colnames(combotemp)<-1:ncol(combotemp)
#make.names(colnames(combotemp))

#Get the column names, strip off second column and make it a vector, then add names for 
#subject and activity and then use the joined vector to name the columns

columnnames<-read.csv("features.txt",sep="",header=FALSE,stringsAsFactors=FALSE)
columnnames<-columnnames[,2]
columnnames<-gsub("(","",columnnames,fixed=TRUE)
columnnames<-gsub(")","",columnnames,fixed=TRUE)
columnnames<-gsub("-","",columnnames,fixed=TRUE)
columnnames<-gsub(" ","",columnnames,fixed=TRUE)
columnnames<-gsub(',','',columnnames)

newnames<-c("Subject_Person","Activity_Label")
columnnames<-c(newnames,columnnames)
colnames(combotemp)<-columnnames

#Use grep to get column numbers for all the columns you what to keep - subject, activity, M(m)eans and std's
#Add all the column numbers into a keepers vector and reorder the vector for neatness.

wantedSubject<-grep("Subject_Person",names(combotemp))
wantedActivity<-grep("Activity_Label",names(combotemp))
wantedMean<-grep("Mean",names(combotemp))
wantedmean<-grep("mean",names(combotemp))
wantedstd<-grep("std",names(combotemp))
keepers<-c(wantedSubject,wantedActivity,wantedMean,wantedmean,wantedstd)
make.names(keepers,unique=TRUE,allow_=TRUE)
keepers<-sort(keepers)

#Create new data frame with only the means, std's and of course subject and activity names

combotemp1<-combotemp[,keepers]

#mutate a new column that calculates the activity name based on looking up the ActivityLevel column
#and call the new dataframe combotemp2

library(Hmisc)
library(plyr)

combotemp2<-mutate(combotemp1,Activities=activities[combotemp1[,2]])

#Now use dplyr

library(dplyr)
#create a table dataframe called final
final<-tbl_df(combotemp2)
#select Subject Person, Activities and all of the data columns
final<-select(final,Subject_Person,Activities,3:88)
#Use group_by to group by Subject_Person and Activities
final<-group_by(final,Subject_Person,Activities)
#Finally use summarise_each to get the mean of all the other columns
final<-summarise_each(final,funs(mean))
