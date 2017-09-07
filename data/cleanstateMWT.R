library(plyr)
library(dplyr)
library(lubridate)
library(reshape2)
library(purrr)

#read in all the state MWT data
stateMWTCatch<-read.table("data/stateMWT/stateMWTCatch.txt",header=TRUE,sep="\t")
stateMWTFishCodes<-read.table("data/stateMWT/stateMWTSpeciesCodes.txt",header=TRUE,sep="\t")
stateMWTNetCodes<-read.table("data/stateMWT/stateMWTNetCodes.txt",header=TRUE,sep="\t")
stateMWTStationsLookup<-read.table("data/stateMWT/stateMWTStationsLookup.txt",header=TRUE,sep="\t")
stateMWTWaterInfo<-read.table("data/stateMWT/stateMWTWaterInfo.txt",header=TRUE,sep="\t")
stateMWTVariableLookup<-read.table("data/stateMWT/stateMWTVariableLookup.txt",header=TRUE,sep="\t")

str(stateMWTCatch)
str(stateMWTFishCodes)
str(stateMWTNetCodes)
str(stateMWTStationsLookup)
str(stateMWTWaterInfo)
str(stateMWTVariableLookup)

#first off need to fix this god awful version of a lookup table
head(stateMWTVariableLookup)
#look at that monstrosity^!!!!

#first do a messy dcast to make it a wide format with each vector from one of the "CodeListName" groups
#this is the best method I could think of
stateMWTVariablesWide<-dcast(stateMWTVariableLookup, ValueCode~CodeListName)

#next need to rename vectors to match with related vectors in the data tables because who ever made this has the
#names for the codes different then what is used in the tables
str(stateMWTVariablesWide)
head(stateMWTVariablesWide)
stateMWTVariablesWide<-rename(stateMWTVariablesWide,TideCode=TideCodeList)
#Luckily all we really need is the TideCodes
stateMWTTideCodes<-select(stateMWTVariablesWide, ValueCode, TideCode)
stateMWTTideCodes<-rename(stateMWTTideCodes, TideCode=ValueCode, tidet=TideCode)
head(stateMWTTideCodes)

#fix type in StationCode in stateMWTWaterInfo
stateMWTWaterInfo$StationCode<-as.factor(stateMWTWaterInfo$StationCode)
#fix tidecode type in stateMWTWaterInfo
stateMWTWaterInfo$TideCode<-as.factor(stateMWTWaterInfo$TideCode)

#the final joined table is too large! have to shave some of it down before joining
stateMWTCatch<-select(stateMWTCatch, SampleRowID, OrganismCode, Catch)
stateMWTWaterInfo<-select(stateMWTWaterInfo, -UserName, -TowDirectionCode, -CableOut, -MeterNumber,-WaveCode,-DateEntered)
stateMWTStationsLookup<-select(stateMWTStationsLookup, -Active,-RKI,-Comments,-Channel.Shoal)
stateMWTNetCodes<-select(stateMWTNetCodes, -NormalizedCode,-Active)

#Lets join up the tables
stateMWTdata<-stateMWTCatch%>%
  full_join(stateMWTFishCodes)%>%
  full_join(stateMWTWaterInfo)%>%
  full_join(stateMWTNetCodes)%>%
  full_join(stateMWTStationsLookup)%>%
  full_join(stateMWTTideCodes)
head(stateMWTdata)

#select relevant data
str(stateMWTdata)
stateMWTselected<-select(stateMWTdata,-OrganismRowID,-Microcystis, -Active,-Phylum,-Class,-NameInSAS,-OrganismSymbol,-NormalizedCode,
                         -SecchiEstimated, -TideCode,-MethodType,-SurveyNumber,-MethodName,-StationRowID,-Order,-AlternateName,-SampleTimeEnd,-Lat,-Long,-MethodRowID,-WeatherCode)
str(stateMWTselected)
head(stateMWTselected)

#format Date
stateMWTclean<-stateMWTselected
stateMWTclean$SampleDate <- as.Date(stateMWTclean$SampleDate , "%m/%d/%Y")
stateMWTclean<-stateMWTclean[order(as.Date(stateMWTclean$SampleDate, format="%m/%d/%Y")),]
head(stateMWTclean)

#restructure SampleTimeStart
stateMWTclean$SampleTimeStart<-strptime(stateMWTclean$SampleTimeStart, "%m/%d/%Y %H:%M:%S")
stateMWTclean$SampleTimeStart<-strftime(stateMWTclean$SampleTimeStart, format="%H:%M:%S")

str(stateMWTclean)

#rename vectors to match hobbs flatfile data
stateMWTclean<-rename(stateMWTclean, id=SampleRowID, code=OrganismCode, Count=Catch, comname=CommonName, family=Family,
                      genus=Genus, species=Species, notes=Comments, polystn=StationCode, Date=SampleDate, method=MethodCode,Time=SampleTimeStart,
                      bstemp=WaterTemperature, bntu=Turbidity, bsecchi=Secchi, bscond=ConductivityTop, bbcond=ConductivityBottom,
                      bflow=MeterStart, eflow=MeterEnd,depth=DepthBottom, loc=Location, lat=WGS84.Lat, long=WGS84.Long)
#write TO rds, this is to make sure the formats stay the same, as saving as a txt doesn't save formats correctly
#make sure to change the destination file path for your computer when doing this at home
saveRDS(stateMWTclean,file="C:\\Users\\hobbs\\Desktop\\R for work\\StateToHobbsDataClean\\data\\stateMWTclean.rds")

