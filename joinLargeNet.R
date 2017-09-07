library(plyr)
library(dplyr)
library(lubridate)
library(reshape2)
library(purrr)

hobbsLargeNet<-read.table("data/hobbsLargeNetFlat.txt", header= T, sep= "\t")
head(hobbsLargeNet)

stateBS<-readRDS("data/stateBSclean.rds")
stateMWT<-readRDS("data/stateMWTclean.rds")
stateTN<-readRDS("data/stateTNclean.rds")
str(stateBS)
str(stateMWT)
str(stateTN)

#format columns to have same types between dataframes
stateBS$code<-as.factor(stateBS$code)
stateMWT$code<-as.factor(stateMWT$code)
stateTN$code<-as.factor(stateTN$code)
stateBS$polystn<-as.factor(stateBS$polystn)
stateMWT$polystn<-as.factor(stateMWT$polystn)
stateTN$polystn<-as.factor(stateTN$polystn)

#join state data together
stateLargeNet<-stateBS%>%
  bind_rows(stateMWT)%>%
  bind_rows(stateTN)
head(stateLargeNet)

#rename region>bregion
stateLargeNet<-rename(stateLargeNet, bregion=region)

#dublicate rows were created somewhere in the process, this is easiest fix
stateLargeNet<-unique(stateLargeNet)

#unfortunately the state surveys don't seem to use the same common names or codes for species
#so this is going to take some work renaming everything to our standards

#found this nifty function on stackoverflow that uses toupper to capitilize the first
#letter in each word in a string
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

#use it on comnames
stateLargeNet$comname<-as.character(stateLargeNet$comname)
stateLargeNet$comname<-sapply(stateLargeNet$comname, simpleCap)

#trasnfer comname back to factor
stateLargeNet$comname<-as.factor(stateLargeNet$comname)

statespecies<-unique(stateLargeNet$comname)
str(statespecies)

#add in agencies
stateLargeNet$agency<-rep("CDFW",nrow(stateLargeNet))
hobbsLargeNet$agency<-rep("Hobbs",nrow(hobbsLargeNet))

#format Hobbs dates
hobbsLargeNet$Date <- as.Date(hobbsLargeNet$Date , "%m/%d/%Y")

#join state data to hobbs
allLargeNet<-stateLargeNet%>%
  bind_rows(hobbsLargeNet)
str(allLargeNet)

#next problem is all the species "codes" are different between hobbs, state, and even within state surveys
#first off import the species lookups from hobbs lab
hobbsSpeciesLookUp<-read.table("data/hobbsSpeciesLookUp.txt", header= T, sep= "\t")
head(hobbsSpeciesLookUp)

#next delete the old "code" column from allLargeNet
allLargeNet<-select(allLargeNet, -code)

#lets also only select the code and comname from hobbsSpeciesLookup for use
hobbsSpeciesLookUp<-select(hobbsSpeciesLookUp, code, comname)

#now join in hobbsSpeciesLookup
allLargeNet<-allLargeNet%>%
  full_join(hobbsSpeciesLookUp)
head(allLargeNet)
str(allLargeNet)

#first lets replace null with "No Catch"
allLargeNet$comname <- sub("^$", "No Catch", allLargeNet$comname)

#now lets do a check
allSpecies<-select(allLargeNet, code, comname)
allSpecies<-unique(allSpecies)
head(allSpecies)
View(allSpecies) #"View" call might only work with RStudio, don't know if it works with default rconsole

#first lets replace null with "No Catch"
allLargeNet$comname <- sub("^$", "No Catch", allLargeNet$comname)

#write to .txt
write.table(allLargeNet, "C:\\Users\\hobbs\\Desktop\\R for work\\StateToHobbsDataClean\\allLargeNet.txt", sep="\t", col.names = T, row.names = F)

