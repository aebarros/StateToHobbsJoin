library(plyr)
library(dplyr)
library(lubridate)
library(reshape2)
library(purrr)

#read in all the state MWT data
stateTNCatch<-read.table("data/stateTowNet/stateTowNetCatch.txt",header=TRUE,sep="\t")
stateTNSpeciesCodes<-read.table("data/stateTowNet/stateTowNetSpeciesCodes.txt",header=TRUE,sep="\t")
stateTNStations<-read.table("data/stateTowNet/stateTowNetStations.txt",header=TRUE,sep="\t")
stateTNTowEffort<-read.table("data/stateTowNet/stateTowNetTowEffort.txt",header=TRUE,sep="\t")
stateTNTowInfo<-read.table("data/stateTowNet/stateTowNetTowInfo.txt",header=TRUE,sep="\t")
stateTNTide<-read.table("data/stateTowNet/stateTowNetTide.txt",header=TRUE,sep="\t")

str(stateTNCatch)
str(stateTNSpeciesCodes)
str(stateTNStations)
str(stateTNTowEffort)
str(stateTNTowInfo)
str(stateTNTide)

#rename ORganismtCode in SpeciesCodes
stateTNSpeciesCodes<-rename(stateTNSpeciesCodes, OrganismCode=OrganismCodeSTN)
stateTNStations<-rename(stateTNStations, StationCode=StationCodeSTN)

#getting memory allocation errors so have to lessen amount of data?
stateTNCatch<-select(stateTNCatch, -CatchRowID)
stateTNSpeciesCodes<-select(stateTNSpeciesCodes, -OrganismRowID,-OrganismCodeMaster,-CommonNameMaster,-FieldNameMWT,-FieldNameSTN,
                            -OrganismCodeMWT,-OrganismSymbol,-Active,-Phylum,-Class,-Order)
stateTNStations<-select(stateTNStations,-StationRowID,-Active,-Core,-Index,-RKI,-TNS.Diet.Area,-STRArea,-WeightingFactor,
                        -YearAdded,-YearEliminated)
stateTNTowInfo<-select(stateTNTowInfo,-UserName,-Survey,-Weather,-Waves,-StartLatDegrees,-StartLatMinutes,-StartLatSeconds,
                       -StartLongDegrees,-StartLongMinutes,-StartLongSeconds,-EndLatDegrees,-EndLatMinutes,-EndLatSeconds,
                       -EndLongDegrees,-EndLongMinutes,-EndLongSeconds)

#Lets join up the tables
stateTNdata<-stateTNTowInfo%>%
  full_join(stateTNTowEffort)%>%
  full_join(stateTNCatch)%>%
  full_join(stateTNSpeciesCodes)%>%
  full_join(stateTNStations)%>%
  full_join(stateTNTide)
head(stateTNdata)
str(stateTNdata)

#format gps because state lat long is in a ridiculous format seperated out as seperate vectors each for Degree, Minute and Second
#Who in their right mind structures gps coordinates like this without having two vectors for just lat and long?
sapply(stateTNdata,class)
#transform into decimal degrees
stateTNdata$LatS<-stateTNdata$LatS/3600
stateTNdata$LatM<-stateTNdata$LatM/60
stateTNdata$LonS<-stateTNdata$LonS/3600
stateTNdata$LonM<-stateTNdata$LonM/60
#add minutes and seconds together
stateTNdata$LatMS<-stateTNdata$LatM+stateTNdata$LatS
stateTNdata$LonMS<-stateTNdata$LonM+stateTNdata$LonS
#combine data
stateTNdata$Latitude <- paste(stateTNdata$LatD,stateTNdata$LatMS)
stateTNdata$Longitude <- paste(stateTNdata$LonD,stateTNdata$LonMS)
head(stateTNdata)
#remove spaces and first zero, replace with °
stateTNdata$Latitude <- gsub(" 0.", ".", stateTNdata$Latitude)
stateTNdata$Longitude <- gsub(" 0.", ".", stateTNdata$Longitude)
head(stateTNdata)
#add "-" infront of all longitude
stateTNdata$negative <- rep("-",nrow(stateTNdata)) # make new column 
stateTNdata$Longitude<-paste(stateTNdata$negative, stateTNdata$Longitude)
stateTNdata$Longitude <- gsub(" ", "", stateTNdata$Longitude)
head(stateTNdata)
#transform Lat and Long to numeric
stateTNdata<-transform(stateTNdata, Latitude = as.numeric(Latitude), Longitude = as.numeric(Longitude))
head(stateTNdata)
str(stateTNdata)

#drop some of the LatLon columns
stateTNdata<-select(stateTNdata,-LatD,-LatM,-LatS,-LonD,-LonM,-LonS,-LatMS,-LonMS,-negative)
head(stateTNdata)

#format Date
stateTNclean<-stateTNdata
stateTNclean$SampleDate <- as.Date(stateTNclean$SampleDate , "%m/%d/%Y")
#restructure SampleTimeStart
stateTNclean$TimeStart<-strptime(stateTNclean$TimeStart, "%m/%d/%Y %H:%M:%S")
stateTNclean$TimeStart<-strftime(stateTNclean$TimeStart, format="%H:%M:%S")

#select needed vectors
str(stateTNclean)
stateTNclean<-select(stateTNclean, -CableOut,-TowDirection,-WindDirection,-Microcystis,-TowRowID,-TimeStop,
                     -MeterSerial,-TowComments,-MeterEstimate,-AreaCode,-Comments,-TideCode)

#rename vectors to match hobbs flatfile data
stateTNclean<-rename(stateTNclean, id=SampleRowID, Date=SampleDate, polystn=StationCode,bstemp=TemperatureTop,
                     bbtemp=TemperatureBottom, bsecchi=Secchi, bscond=ConductivityTop, bbcond=ConductivityBottom,
                     depth=DepthBottom, notes=SampleComments, bntu=TurbidityTop, rep=TowNumber, Time=TimeStart,
                     bflow=MeterIn, eflow=MeterOut, dflow=MeterDifference, code=OrganismCode, Count=Catch,
                     comname=CommonName, family=Family, genus=Genus, species=Species, region=Region, loc=Location,
                     tidet=TideDesc, lat=Latitude, long=Longitude)

#write TO rds, this is to make sure the formats stay the same, as saving as a txt doesn't save formats correctly
#make sure to change the destination file path for your computer when doing this at home
saveRDS(stateTNclean,file="C:\\Users\\hobbs\\Desktop\\R for work\\StateToHobbsDataClean\\data\\stateTNclean.rds")
