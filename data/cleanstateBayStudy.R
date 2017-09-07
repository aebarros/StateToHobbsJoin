library(plyr)
library(dplyr)
library(lubridate)
library(reshape2)
library(purrr)

#read in all the state BayStudy data
stateBSCatch<-read.table("data/stateBayStudy/stateBayStudyCatch.txt",header=TRUE,sep="\t")
stateBSFishCodes<-read.table("data/stateBayStudy/stateBayStudySpeciesCodes.txt",header=TRUE,sep="\t")
stateBSStations<-read.table("data/stateBayStudy/stateBayStudyStations.txt",header=TRUE,sep="\t")
stateBSSalinTemp<-read.table("data/stateBayStudy/stateBayStudySalinTemp.txt",header=TRUE,sep="\t")
stateBSDepthSecchi<-read.table("data/stateBayStudy/stateBayStudyMeters.txt",header=TRUE,sep="\t")
stateBSNetCodes<-read.table("data/stateBayStudy/stateBayStudyNetCodes.txt",header=TRUE,sep="\t")
stateBSLengths<-read.table("data/stateBayStudy/stateBayStudyLengths.txt",header=TRUE,sep="\t")
stateBSTides<-read.table("data/stateBayStudy/stateBayStudyTides.txt",header=TRUE,sep="\t")

str(stateBSCatch)
str(stateBSFishCodes)
str(stateBSStations)
str(stateBSSalinTemp)
str(stateBSDepthSecchi)
str(stateBSNetCodes)

#remove comments from stateBSDepthSecchi
stateBSDepthSecchi<-select(stateBSDepthSecchi, -Comment,-Tide)

#add id to Stations df to allow id of individual tows
stateBSStations$id<-seq.int(nrow(stateBSStations))

#join all the data
stateBSdata<-stateBSCatch%>%
  full_join(stateBSFishCodes)%>%
  full_join(stateBSStations)%>%
  full_join(stateBSSalinTemp)%>%
  full_join(stateBSDepthSecchi)%>%
  full_join(stateBSNetCodes)%>%
  full_join(stateBSTides)
str(stateBSdata)
head(stateBSdata)

#get catch counts because bay study doesn't put this most basic of measurements in their database
#sum count from length data#
head(stateBSLengths)
data.lengthcount=stateBSLengths%>%
  group_by_("Year","Survey","Station","Net","AlphaCode")%>%
  summarise(Catch=n())
head(data.lengthcount)

data.catch=stateBSCatch

#join data.catch to data.lengthcount#
total.catch=data.catch%>%
  inner_join(data.lengthcount)
head(total.catch)
str(total.catch)

#sum catch with pluscount, then divide by qtssubsampled and multiply by qts caught and round down#
total.catch$TotalCatch<-((total.catch$Catch + total.catch$PlusCount)/total.catch$QtsSubsampled)*total.catch$QtsCaught
head(total.catch)

#join total.catch to rest of data
stateBSdata<-stateBSdata%>%
  full_join(total.catch)
head(stateBSdata)
str(stateBSdata)

#format Date
stateBSdata$Date <- as.Date(stateBSdata$Date , "%m/%d/%Y")

#select vectors to keep
stateBSselected<-select(stateBSdata, Date, Time, Method, Station, Tow, AlphaCode, CommonName, Comments,
                        Tide, Depth,Duration, StartMeter, EndMeter, TotalMeter, StartLat, StartLong, Distance, Comment, ECSurf,
                        ECBott, TempSurf, TempBott, Secchi, TotalCatch, Description, ScientificName, Family, Method)
head(stateBSselected)

#strip time data to HH:MM:SS
stateBSclean<-stateBSselected
stateBSclean$Time<-strptime(stateBSclean$Time, "%m/%d/%Y %H:%M:%S")
stateBSclean$Time<-strftime(stateBSclean$Time, format="%H:%M:%S")
head(stateBSclean)
str(stateBSclean)

#split the ScientificName vector into two new columns "genus" and "species"
stateBSclean$genus<- sapply(strsplit(as.character(stateBSclean$ScientificName),' '), "[", 1)
stateBSclean$species <- sapply(strsplit(as.character(stateBSclean$ScientificName),' '), "[", 2)

#rename vectors to match hobbs flatfile data
stateBSclean<-rename(stateBSclean, polystn=Station, code=AlphaCode, comname=CommonName, notes=Comments, dur=Duration, tidet=Description,
                     lat=StartLat, long=StartLong, dist=Distance, bflow=StartMeter, eflow=EndMeter, dflow=TotalMeter,
                     bscond=ECSurf, depth=Depth,bbcond=ECBott, bstemp=TempSurf, bbtemp=TempBott, bsecchi=Secchi, Count=TotalCatch, gensp=ScientificName, family=Family,method=Method)
str(stateBSclean)



#write TO rds, this is to make sure the formats stay the same, as saving as a txt doesn't save formats correctly
#make sure to change the destination file path for your computer when doing this at home
saveRDS(stateBSclean,file="C:\\Users\\hobbs\\Desktop\\R for work\\StateToHobbsDataClean\\data\\stateBSclean.rds")
