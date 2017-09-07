library(plyr)
library(dplyr)
library(lubridate)
library(reshape2)
library(purrr)

#read in all the state 20mm data
stateSLSCatch<-read.table("data/stateSLS/stateSLSCatch.txt",header=TRUE,sep="\t")
stateSLSSpeciesCodes<-read.table("data/stateSLS/stateSLSFishCodes.txt",header=TRUE,sep="\t")
stateSLSStationInfo<-read.table("data/stateSLS/stateSLSStations.txt",header=TRUE,sep="\t")
stateSLSTowInfo<-read.table("data/stateSLS/stateSLSTowInfo.txt",header=TRUE,sep="\t")
stateSLSWaterInfo<-read.table("data/stateSLS/stateSLSWaterInfo.txt",header=TRUE,sep="\t")

#add id to TowInfo to create unique id for each tow
stateSLSTowInfo$id<-seq.int(nrow(stateSLSTowInfo))

#join fishcodes to catch
#but first fix column name in catch
stateSLSCatch<-rename(stateSLSCatch, Fish.Code=FishCode)
stateSLSCatch<-stateSLSCatch%>%
  inner_join(stateSLSSpeciesCodes)
head(stateSLSCatch)

#join Catch to TowInfo and WaterInfo and StationInfo
head(stateSLSTowInfo)
head(stateSLSWaterInfo)

  #drop comments from TowInfo because it conflicts with joining
stateSLSTowInfo<-select(stateSLSTowInfo, -Comments)

stateSLSTows<-stateSLSCatch%>%
  full_join(stateSLSTowInfo)%>%
  full_join(stateSLSWaterInfo)%>%
  full_join(stateSLSStationInfo)
head(stateSLSTows)

#format gps because state lat long is in a ridiculous format
str(stateSLSTows)
#transform into decimal degrees
stateSLSTows$LatS<-stateSLSTows$LatS/3600
stateSLSTows$LatM<-stateSLSTows$LatM/60
stateSLSTows$LonS<-stateSLSTows$LonS/3600
stateSLSTows$LonM<-stateSLSTows$LonM/60
#add minutes and seconds together
stateSLSTows$LatMS<-stateSLSTows$LatM+stateSLSTows$LatS
stateSLSTows$LonMS<-stateSLSTows$LonM+stateSLSTows$LonS
#combine data
stateSLSTows$Latitude <- paste(stateSLSTows$LatD,stateSLSTows$LatMS)
stateSLSTows$Longitude <- paste(stateSLSTows$LonD,stateSLSTows$LonMS)
head(stateSLSTows)
#remove spaces and first zero, replace with °
stateSLSTows$Latitude <- gsub(" 0.", ".", stateSLSTows$Latitude)
stateSLSTows$Longitude <- gsub(" 0.", ".", stateSLSTows$Longitude)
head(stateSLSTows)
#add "-" infront of all longitude
stateSLSTows$negative <- rep("-",nrow(stateSLSTows)) # make new column 
stateSLSTows$Longitude<-paste(stateSLSTows$negative, stateSLSTows$Longitude)
stateSLSTows$Longitude <- gsub(" ", "", stateSLSTows$Longitude)
head(stateSLSTows)
#transform Lat and Long to numeric
stateSLSTows<-transform(stateSLSTows, Latitude = as.numeric(Latitude), Longitude = as.numeric(Longitude))
head(stateSLSTows)
str(stateSLSTows)

#remove unnecessary data
stateSLSselected<-select(stateSLSTows, id, Date, Station, Time, Duration, BottomDepth, Survey, Tow, Temp, TopEC,BottomEC, Secchi, Turbidity, Latitude, Longitude, Comments, NetMeterCheck, Fish.Code, Common.Name, Catch)
str(stateSLSselected)

#format dates
stateSLSselected$Date <- as.Date(stateSLSselected$Date , "%m/%d/%Y")

stateSLSclean<-stateSLSselected
#calculate volumes and CPUE sampled
stateSLSclean$vol<-stateSLSclean$NetMeterCheck*.026*.37
stateSLSclean$cpue<-(stateSLSclean$Catch/stateSLSclean$vol)*1000

#add in methods column
stateSLSclean$method<-rep("SLS",nrow(stateSLSclean))

#rename columns to fit hobbs lab column names
stateSLSclean<-rename(stateSLSclean, towdate=Date, polstn=Station, dur=Duration, depth=BottomDepth, survey=Survey, rep=Tow, bstemp=Temp, bscond=TopEC, bbcond=BottomEC,
                     ssecchi=Secchi, sntu=Turbidity, clat=Latitude, clong=Longitude,comments=Comments, code=Fish.Code, comname=Common.Name, corcatch=Catch)
stateSLSclean<-select(stateSLSclean, -NetMeterCheck)
head(stateSLSclean)

#write TO rds, this is to make sure the formats stay the same, as saving as a txt doesn't save formats correctly
#make sure to change the destination file path for your computer when doing this at home
saveRDS(stateSLSclean,file="C:\\Users\\hobbs\\Desktop\\R for work\\StateToHobbsDataClean\\data\\stateSLSclean.rds")
