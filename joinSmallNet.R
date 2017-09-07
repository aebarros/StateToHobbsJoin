library(plyr)
library(dplyr)
library(lubridate)
library(reshape2)
library(purrr)

hobbsSmallNet<-read.table("data/hobbsSmallNetFlat.txt", header= T, sep= "\t")
head(hobbsSmallNet)
state20<-readRDS("data/state20clean.rds")
stateSLS<-readRDS("data/stateSLSclean.rds")

#join state data
stateSmallNet<-state20%>%
  rbind(stateSLS)

#add agency columns
stateSmallNet$agency<-rep("CDFW",nrow(stateSmallNet))
hobbsSmallNet$agency<-rep("Hobbs",nrow(hobbsSmallNet))

#format hobbs dates
hobbsSmallNet$towdate <- as.Date(hobbsSmallNet$towdate , "%m/%d/%Y")

#format state polstn into factor class
stateSmallNet$polstn<-as.factor(stateSmallNet$polstn)

#convert hobbs dur to just numbers and integer format#
hobbsSmallNet$dur<-strptime(hobbsSmallNet$dur, "%m/%d/%Y %H:%M:%S")
hobbsSmallNet$dur<-strftime(hobbsSmallNet$dur, format="%H:%M:%S")
hobbsSmallNet$dur <- gsub("00", "", hobbsSmallNet$dur)
hobbsSmallNet$dur<-gsub(":","",hobbsSmallNet$dur)
hobbsSmallNet<-transform(hobbsSmallNet, dur = as.integer(dur))
head(hobbsSmallNet)

#convert hobbs survey to integer
hobbsSmallNet$survey<-as.integer(hobbsSmallNet$survey)

#convert state code to factor
stateSmallNet$code<-as.factor(stateSmallNet$code)

#join all data
allSmallNet<-hobbsSmallNet%>%
  bind_rows(stateSmallNet)
head(allSmallNet)

#write to .txt
write.table(allSmallNet, "C:\\Users\\hobbs\\Desktop\\R for work\\StateToHobbsDataClean\\allSmallNet.txt", sep="\t", col.names = T, row.names = F)
