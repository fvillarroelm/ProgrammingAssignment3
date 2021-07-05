library(tidyverse)
library(data.table)

rm(list=ls())

#downloading datasets.
url="https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url,"./data.zip", method="curl")
unzip("./data.zip")


# Part 1 ----

#test dataset
setwd("./UCI HAR Dataset/test")
files=list.files(path=getwd(), pattern=".txt")
data_test=map(files,read.table)
data_test[[1]]=data_test[[1]]%>%rename(subject_id=V1)
data_test[[3]]=data_test[[3]]%>%rename(labels=V1)
data_test=bind_cols(data_test)%>%relocate(labels,.after=subject_id)

#train dataset
setwd("../train")
files=list.files(path=getwd(), pattern=".txt")
data_train=map(files,read.table)
data_train[[1]]=data_train[[1]]%>%rename(subject_id=V1)
data_train[[3]]=data_train[[3]]%>%rename(labels=V1)
data_train=bind_cols(data_train)%>%relocate(labels,.after=subject_id)

#join all data
data_all=bind_rows(data_test,data_train)%>%arrange(subject_id)

# Part 4 ----

#importing variable names. I cannot keep only mean and std variables without 
#importing their names first!
setwd("../")
colNames=read.table("features.txt")%>%pluck(2)
#assigning names
names(data_all)[3:563]=colNames

# Part 2 ----

#I extract "mean()" (it excludes "meanFreq()")
data_all=data_all%>%select(c("subject_id","labels",str_subset(names(data_all),"(mean\\(\\))|(std\\(\\))")))

# Part 3 ----

#importing activity labels
activity_labels=read.table("activity_labels.txt")%>%
  rename(labels=V1, activity=V2)%>%
  mutate(activity=str_to_lower(activity))
#assigning activity labels
data_all=left_join(data_all,activity_labels,by="labels")%>%
  select(-labels)%>%
  relocate(activity,.after=subject_id)

# Part 5 ----

#second dataset: mean of variables by subject, activity, and measure.
#first, from wide to long.
data_long=data_all%>%gather("measure", "value", -c(subject_id, activity))
#computation of mean.
data_long=data_long%>%group_by(subject_id, activity, measure)%>%
                 mutate(value_mean_bySubjectAndActivity=mean(value))%>%
                 ungroup()%>%
                 distinct(subject_id,activity,measure,.keep_all=TRUE)%>%
                 select(-value)%>%
                 arrange(subject_id,activity,measure)
