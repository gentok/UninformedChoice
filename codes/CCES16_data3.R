################################################################################# 
## File Name: CCES16_data3.R                                                   ##
## Date: 10 Apr 2019                                                           ##
## Author: Gento Kato                                                          ##
## Project: Uninformed Choice                                                  ##
## Purpose: Import and Recode Data                                             ##
################################################################################# 

#################
## Preparation ##
#################

## Clear Workspace
rm(list=ls())

## Library Required Packages
library(rprojroot); library(readstata13); library(foreign)
library(questionr); library(psych); library(haven)

## Set Working Directory (Automatically) ##
if (rstudioapi::isAvailable()==TRUE) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path)); 
} 
projdir <- find_root(has_file("thisishome.txt"))
cat(paste("Working Directory Set to:\n",projdir))
setwd(projdir)

## Initial Data Location
dloc <- "data/cces16s_cxt.rds"
cqloc <- readRDS("data/cqloc_house.rds")
cq0loc <- cqloc[["nation"]]
cq1loc <- cqloc[["state"]]
cq2loc <- cqloc[["district"]]

###############
## Load Data ##
###############

d <- readRDS(dloc)
cq0 <- readRDS(cq0loc)
cq008 <- cq0[which(cq0$year %in% c(2008)),]
cq010 <- cq0[which(cq0$year %in% c(2010)),]
cq012 <- cq0[which(cq0$year %in% c(2012)),]
cq014 <- cq0[which(cq0$year %in% c(2014)),]
cq1 <- readRDS(cq1loc)
cq108 <- cq1[which(cq1$year %in% c(2008)),]
cq110 <- cq1[which(cq1$year %in% c(2010)),]
cq112 <- cq1[which(cq1$year %in% c(2012)),]
cq114 <- cq1[which(cq1$year %in% c(2014)),]
cq108 <- aggregate(cq108[,c("RepVotes","DemVotes")], list(Area=cq108[,"Area"]), sum)
cq108$RepVotesMajorPercent <- cq108$RepVotes/(cq108$RepVotes+cq108$DemVotes)*108
cq110 <- aggregate(cq110[,c("RepVotes","DemVotes")], list(Area=cq110[,"Area"]), sum)
cq110$RepVotesMajorPercent <- cq110$RepVotes/(cq110$RepVotes+cq110$DemVotes)*108
cq112 <- aggregate(cq112[,c("RepVotes","DemVotes")], list(Area=cq112[,"Area"]), sum)
cq112$RepVotesMajorPercent <- cq112$RepVotes/(cq112$RepVotes+cq112$DemVotes)*108
cq114 <- aggregate(cq114[,c("RepVotes","DemVotes")], list(Area=cq114[,"Area"]), sum)
cq114$RepVotesMajorPercent <- cq114$RepVotes/(cq114$RepVotes+cq114$DemVotes)*108
cq2 <- readRDS(cq2loc)
cq2$district <- paste(state.abb[match(cq2$State,state.name)],
                      cq2$AreaNumber)
cq212 <- cq2[which(cq2$year %in% c(2012)),]
cq214 <- cq2[which(cq2$year %in% c(2014)),]

# Missing if Redistricted After 2012 Election
table(cq214$RedistrictedDate)
# (dropdist <- cq214$district[cq214$RedistrictedDate %in% c("20140101")])
# cq212$RepVotesMajorPercent[cq212$district %in% dropdist] <- NA
# cq214$RepVotesMajorPercent[cq214$district %in% dropdist] <- NA

# Missing if Any of the Election Uncontested (or not fought between two parties)
table(cq212$PluralityParty)
table(cq214$PluralityParty)
(dropdist <- cq212$district[cq212$PluralityParty %in% c("I","ROP")])
cq212$RepVotesMajorPercent[cq212$district %in% dropdist] <- NA
cq214$RepVotesMajorPercent[cq214$district %in% dropdist] <- NA
table(cq212$RepVotesMajorPercent %in% c(0,100))
table(cq214$RepVotesMajorPercent %in% c(0,100))
dropdist1 <- cq212$district[cq212$RepVotesMajorPercent %in% c(0,100)]
dropdist2 <- cq212$district[cq214$RepVotesMajorPercent %in% c(0,100)]
(dropdist <- sort(unique(c(dropdist1,dropdist2))))
cq212$RepVotesMajorPercent[cq212$district %in% dropdist] <- NA
cq214$RepVotesMajorPercent[cq214$district %in% dropdist] <- NA

dropped1 <- cq212$district[is.na(cq212$RepVotesMajorPercent)]
dropped2 <- cq214$district[is.na(cq214$RepVotesMajorPercent)]
(dropped <- sort(unique(c(dropped1,dropped2))))
cq212$RepVotesMajorPercent[cq212$district %in% dropped] <- NA
cq214$RepVotesMajorPercent[cq214$district %in% dropped] <- NA

statePVI <- 
  ( (cq112$RepVotesMajorPercent - cq012$RepVotesMajorPercentAll) + 
      (cq114$RepVotesMajorPercent - cq014$RepVotesMajorPercentAll) )/2
districtPVI12 <- cq212$RepVotesMajorPercent - cq112$RepVotesMajorPercent[match(cq212$State,cq112$Area)]
districtPVI12 <- districtPVI12[match(cq214$district,cq212$district)]
districtPVI14 <- cq214$RepVotesMajorPercent - cq114$RepVotesMajorPercent[match(cq214$State,cq114$Area)]
districtPVI <- (districtPVI12 + districtPVI14)/2
statePVI <- statePVI[match(cq214$State,cq114$Area)]
cor(districtPVI,statePVI, use="pairwise")

PVI16h <- data.frame(statePVI, districtPVI,
                     State = cq214$State,
                     district = cq214$district)
saveRDS(PVI16h, "data/PVI16_house.rds")


###########################
## Partisan Voting Index ##
###########################

## Two-Party Republican Proportion by State (2008)
d$hreppr08_state <- 
  cq108$RepVotesMajorPercent[match(d$state,cq108$Area)]
summary(d$hreppr08_state)
unique(d$state[is.na(d$hreppr08_state)]) # Only DC is Good
#cq208$state[grep("",cq208$state)]

## Two-Party Republican Proportion by State (2010)
d$hreppr10_state <- 
  cq110$RepVotesMajorPercent[match(d$state,cq110$Area)]
summary(d$hreppr10_state)
unique(d$state[is.na(d$hreppr10_state)]) # Only DC is Good
#cq210$state[grep("",cq210$state)]

## Two-Party Republican Proportion by State (2012)
d$hreppr12_state <- 
  cq112$RepVotesMajorPercent[match(d$state,cq112$Area)]
summary(d$hreppr12_state)
unique(d$state[is.na(d$hreppr12_state)]) # Only DC is Good
#cq212$state[grep("",cq212$state)]

## Two-Party Republican Proportion by State (2014)
d$hreppr14_state <- 
  cq114$RepVotesMajorPercent[match(d$state,cq114$Area)]
summary(d$hreppr14_state)
unique(d$state[is.na(d$hreppr14_state)]) # Only DC is Good
#cq214$state[grep("",cq214$state)]

## Partisan Voting Index by State (2008-2014)
d$hpvi_state <- (d$hreppr08_state - cq008$RepVotesMajorPercentAll + 
                   d$hreppr10_state - cq010$RepVotesMajorPercentAll + 
                   d$hreppr12_state - cq012$RepVotesMajorPercentAll + 
                   d$hreppr14_state - cq014$RepVotesMajorPercentAll)/4
d$hpvi_state[d$state=="Vermont"] <- NA
d$hpvirep_state <- d$hpvi_state
d$hpvirep_state[which(d$hpvi_state<0)] <- 0
d$hpvidem_state <- -d$hpvi_state
d$hpvidem_state[which(d$hpvi_state>0)] <- 0

## Partisan Change Index by State (2008-2012)
d$hpci_state <- (d$hreppr12_state - d$hreppr08_state) - 
  (cq012$RepVotesMajorPercentAll - cq008$RepVotesMajorPercentAll)
d$hpci_state[d$state=="Vermont"] <- NA
d$hpcirep_state <- d$hpci_state
d$hpcirep_state[which(d$hpci_state<0)] <- 0
d$hpcidem_state <- -d$hpci_state
d$hpcidem_state[which(d$hpci_state>0)] <- 0

## Partisan Voting Index by State (2012-2014)
d$hpvi2_state <- (d$hreppr12_state - cq012$RepVotesMajorPercentAll + 
                   d$hreppr14_state - cq014$RepVotesMajorPercentAll)/2
d$hpvi2_state[d$state=="Vermont"] <- NA
d$hpvi2rep_state <- d$hpvi2_state
d$hpvi2rep_state[which(d$hpvi2_state<0)] <- 0
d$hpvi2dem_state <- -d$hpvi2_state
d$hpvi2dem_state[which(d$hpvi2_state>0)] <- 0

## Partisan Change Index by State (2012-2014)
d$hpci2_state <- (d$hreppr14_state - d$hreppr12_state) - 
  (cq014$RepVotesMajorPercentAll - cq012$RepVotesMajorPercentAll)
d$hpci2_state[d$state=="Vermont"] <- NA
d$hpci2rep_state <- d$hpci2_state
d$hpci2rep_state[which(d$hpci2_state<0)] <- 0
d$hpci2dem_state <- -d$hpci2_state
d$hpci2dem_state[which(d$hpci2_state>0)] <- 0

## Two-Party Republican Proportion by District (2012)
d$hreppr12_district <- 
  cq212$RepVotesMajorPercent[match(d$district,cq212$district)]
summary(d$hreppr12_district)
dropped_cces <- sort(unique(d$district[is.na(d$hreppr12_district)]))
dropped_cces[!dropped_cces %in% dropped] 
#cq212$district[grep("",cq212$district)]

## Two-Party Republican Proportion by District (2014)
d$hreppr14_district <- 
  cq214$RepVotesMajorPercent[match(d$district,cq214$district)]
summary(d$hreppr14_district)
dropped_cces <- sort(unique(d$district[is.na(d$hreppr14_district)]))
dropped_cces[!dropped_cces %in% dropped] 
#cq214$district[grep("",cq214$district)]

## Partisan Voting Index by District (2012-2014)
d$hpvi2_district <- (d$hreppr12_district - d$hreppr12_state + 
                 d$hreppr14_district - d$hreppr14_state)/2
d$hpvi2rep_district <- d$hpvi2_district
d$hpvi2rep_district[which(d$hpvi2_district<0)] <- 0
d$hpvi2dem_district <- -d$hpvi2_district
d$hpvi2dem_district[which(d$hpvi2_district>0)] <- 0

## Partisan Change Index by District (2012-2014)
d$hpci2_district <- (d$hreppr14_district - d$hreppr12_district) - 
  (d$hreppr14_state - d$hreppr12_state) 
d$hpci2rep_district <- d$hpci2_district
d$hpci2rep_district[which(d$hpci2_district<0)] <- 0
d$hpci2dem_district <- -d$hpci2_district
d$hpci2dem_district[which(d$hpci2_district>0)] <- 0

## 3 Category PVI
quantile(abs(d$hpvi_state),probs=c(seq(0,1,by=0.1)),na.rm=T)
d$hpvi_state_3cat <- factor(NA,levels=c("0-5%","5-10%","10%-"))
d$hpvi_state_3cat[which(abs(d$hpvi_state)<5)] <- "0-5%"
d$hpvi_state_3cat[which(abs(d$hpvi_state)>=5)] <- "5-10%"
d$hpvi_state_3cat[which(abs(d$hpvi_state)>=10)] <- "10%-"
round(table(d$hpvi_state_3cat,useNA="always")/
        sum(table(d$hpvi_state_3cat, useNA="always")),3)
quantile(abs(d$hpvi2_state),probs=c(seq(0,1,by=0.1)),na.rm=T)
d$hpvi2_state_3cat <- factor(NA,levels=c("0-5%","5-10%","10%-"))
d$hpvi2_state_3cat[which(abs(d$hpvi2_state)<5)] <- "0-5%"
d$hpvi2_state_3cat[which(abs(d$hpvi2_state)>=5)] <- "5-10%"
d$hpvi2_state_3cat[which(abs(d$hpvi2_state)>=10)] <- "10%-"
round(table(d$hpvi2_state_3cat,useNA="always")/
        sum(table(d$hpvi2_state_3cat, useNA="always")),3)
quantile(abs(d$hpvi2_district),probs=c(seq(0,1,by=0.1)),na.rm=T)
d$hpvi2_district_3cat <- factor(NA,levels=c("0-10%","10-20%","20%-"))
d$hpvi2_district_3cat[which(abs(d$hpvi2_district)<10)] <- "0-10%"
d$hpvi2_district_3cat[which(abs(d$hpvi2_district)>=10)] <- "10-20%"
d$hpvi2_district_3cat[which(abs(d$hpvi2_district)>=20)] <- "20%-"
round(table(d$hpvi2_district_3cat,useNA="always")/
        sum(table(d$hpvi2_district_3cat, useNA="always")),3)

## 3 Category PCI
quantile(abs(d$hpci_state),probs=c(seq(0,1,by=0.1)),na.rm=T)
d$hpci_state_3cat <- factor(NA,levels=c("0-2%","2-5%","5%-"))
d$hpci_state_3cat[which(abs(d$hpci_state)<2)] <- "0-2%"
d$hpci_state_3cat[which(abs(d$hpci_state)>=2)] <- "2-5%"
d$hpci_state_3cat[which(abs(d$hpci_state)>=5)] <- "5%-"
round(table(d$hpci_state_3cat,useNA="always")/
        sum(table(d$hpci_state_3cat, useNA="always")),3)
quantile(abs(d$hpci2_state),probs=c(seq(0,1,by=0.1)),na.rm=T)
d$hpci2_state_3cat <- factor(NA,levels=c("0-2%","2-5%","5%-"))
d$hpci2_state_3cat[which(abs(d$hpci2_state)<2)] <- "0-2%"
d$hpci2_state_3cat[which(abs(d$hpci2_state)>=2)] <- "2-5%"
d$hpci2_state_3cat[which(abs(d$hpci2_state)>=5)] <- "5%-"
round(table(d$hpci2_state_3cat,useNA="always")/
        sum(table(d$hpci2_state_3cat, useNA="always")),3)
quantile(abs(d$hpci2_district),probs=c(seq(0,1,by=0.1)),na.rm=T)
d$hpci2_district_3cat <- factor(NA,levels=c("0-2%","2-5%","5%-"))
d$hpci2_district_3cat[which(abs(d$hpci2_district)<2)] <- "0-2%"
d$hpci2_district_3cat[which(abs(d$hpci2_district)>=2)] <- "2-5%"
d$hpci2_district_3cat[which(abs(d$hpci2_district)>=5)] <- "5%-"
round(table(d$hpci2_district_3cat,useNA="always")/
        sum(table(d$hpci2_district_3cat, useNA="always")),3)

###############
## Save Data ##
###############

saveRDS(d,file="data/cces16s_cxt2.rds")
