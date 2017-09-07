library(plyr)
library(dplyr)
library(lubridate)
library(reshape2)
library(purrr)

#read in all the state 20mm data
state20Catch<-read.table("data/state20/state20mmCatch.txt",header=TRUE,sep="\t")
state20FishCodes<-read.table("data/state20/state20mmFishCodes.txt",header=TRUE,sep="\t")
state20Stations<-read.table("data/state20/state20mmStations.txt",header=TRUE,sep="\t")
state20TowInfo<-read.table("data/state20/state20mmTowInfo.txt",header=TRUE,sep="\t")
state20WaterInfo<-read.table("data/state20/state20mmWaterInfo.txt",header=TRUE,sep="\t")

#add id to TowInfo
state20TowInfo$id<-seq.int(nrow(state20TowInfo))

#join fishcodes to catch
state20Catch<-state20Catch%>%
  inner_join(state20FishCodes)
head(state20Catch)
nrow(state20Catch)

#join Catch to TowInfo and WaterInfo and StationInfo
head(state20TowInfo)
nrow(state20TowInfo)
head(state20WaterInfo)
nrow(state20WaterInfo)
  #drop comments from TowInfo because it conflicts with joining
state20TowInfo<-select(state20TowInfo, -Comments)

state20Tows<-state20Catch%>%
  full_join(state20TowInfo)%>%
  full_join(state20WaterInfo)%>%
  full_join(state20Stations)
head(state20Tows)

#format gps because state lat long is in a ridiculous format seperated out as seperate vectors each for Degree, Minute and Second
#Who in their right mind structures gps coordinates like this without having two vectors for just lat and long?
sapply(state20Tows,class)
#transform into decimal degrees
state20Tows$LatS<-state20Tows$LatS/3600
state20Tows$LatM<-state20Tows$LatM/60
state20Tows$LonS<-state20Tows$LonS/3600
state20Tows$LonM<-state20Tows$LonM/60
#add minutes and seconds together
state20Tows$LatMS<-state20Tows$LatM+state20Tows$LatS
state20Tows$LonMS<-state20Tows$LonM+state20Tows$LonS
#combine data
state20Tows$Latitude <- paste(state20Tows$LatD,state20Tows$LatMS)
state20Tows$Longitude <- paste(state20Tows$LonD,state20Tows$LonMS)
head(state20Tows)
#remove spaces and first zero, replace with °
state20Tows$Latitude <- gsub(" 0.", ".", state20Tows$Latitude)
state20Tows$Longitude <- gsub(" 0.", ".", state20Tows$Longitude)
head(state20Tows)
#add "-" infront of all longitude
state20Tows$negative <- rep("-",nrow(state20Tows)) # make new column 
state20Tows$Longitude<-paste(state20Tows$negative, state20Tows$Longitude)
state20Tows$Longitude <- gsub(" ", "", state20Tows$Longitude)
head(state20Tows)
#transform Lat and Long to numeric
state20Tows<-transform(state20Tows, Latitude = as.numeric(Latitude), Longitude = as.numeric(Longitude))
head(state20Tows)
str(state20Tows)

#remove unnecessary data
state20selected<-select(state20Tows, id, Date, Station, Time, Duration, Bottom.Depth, Survey, Tow, Temp, Top.EC,Bottom.EC, Secchi, Turbidity, Latitude, Longitude, Comments, Net.Meter.Check, Fish.Code, Common.Name, Catch)
str(state20selected)

#format dates
state20selected$Date <- as.Date(state20selected$Date , "%m/%d/%Y")

state20clean<-state20selected
#calculate volumes and CPUE sampled
state20clean$vol<-state20clean$Net.Meter.Check*.026*1.51
state20clean$cpue<-(state20clean$Catch/state20clean$vol)*10000

#add in methods column
state20clean$method<-rep("20mm",nrow(state20clean))

#rename columns to fit hobbs lab column names
state20clean<-rename(state20clean, towdate=Date, polstn=Station, dur=Duration, depth=Bottom.Depth, survey=Survey, rep=Tow, bstemp=Temp, bscond=Top.EC, bbcond=Bottom.EC,
                     ssecchi=Secchi, sntu=Turbidity, clat=Latitude, clong=Longitude,comments=Comments, code=Fish.Code, comname=Common.Name, corcatch=Catch)
state20clean<-select(state20clean, -Net.Meter.Check)
head(state20clean)

#write TO rds, this is to make sure the formats stay the same, as saving as a txt doesn't save formats correctly
#make sure to change the destination file path for your computer when doing this at home
saveRDS(state20clean,file="C:\\Users\\hobbs\\Desktop\\R for work\\StateToHobbsDataClean\\data\\state20clean.rds")
