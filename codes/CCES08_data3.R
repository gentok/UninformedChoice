################################################################################# 
## File Name: CCES08_data3.R                                                   ##
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
dloc <- "data/cces08s_cxt.rds"
cqloc <- readRDS("data/cqloc_house.rds")
cq0loc <- cqloc[["nation"]]
cq1loc <- cqloc[["state"]]
cq2loc <- cqloc[["district"]]

###############
## Load Data ##
###############

d <- readRDS(dloc)
cq0 <- readRDS(cq0loc)
cq000 <- cq0[which(cq0$year %in% c(2000)),]
cq002 <- cq0[which(cq0$year %in% c(2002)),]
cq004 <- cq0[which(cq0$year %in% c(2004)),]
cq006 <- cq0[which(cq0$year %in% c(2006)),]
cq1 <- readRDS(cq1loc)
cq100 <- cq1[which(cq1$year %in% c(2000)),]
cq102 <- cq1[which(cq1$year %in% c(2002)),]
cq104 <- cq1[which(cq1$year %in% c(2004)),]
cq106 <- cq1[which(cq1$year %in% c(2006)),]
cq100 <- aggregate(cq100[,c("RepVotes","DemVotes")], list(Area=cq100[,"Area"]), sum)
cq100$RepVotesMajorPercent <- cq100$RepVotes/(cq100$RepVotes+cq100$DemVotes)*100
cq102 <- aggregate(cq102[,c("RepVotes","DemVotes")], list(Area=cq102[,"Area"]), sum)
cq102$RepVotesMajorPercent <- cq102$RepVotes/(cq102$RepVotes+cq102$DemVotes)*100
cq104 <- aggregate(cq104[,c("RepVotes","DemVotes")], list(Area=cq104[,"Area"]), sum)
cq104$RepVotesMajorPercent <- cq104$RepVotes/(cq104$RepVotes+cq104$DemVotes)*100
cq106 <- aggregate(cq106[,c("RepVotes","DemVotes")], list(Area=cq106[,"Area"]), sum)
cq106$RepVotesMajorPercent <- cq106$RepVotes/(cq106$RepVotes+cq106$DemVotes)*100
cq2 <- readRDS(cq2loc)
cq2$district <- paste(state.abb[match(cq2$State,state.name)],
                      cq2$AreaNumber)
cq204 <- cq2[which(cq2$year %in% c(2004)),]
cq206 <- cq2[which(cq2$year %in% c(2006)),]

# Missing if Redistricted After 2004 Election
table(cq206$RedistrictedDate)
(dropdist <- cq206$district[cq206$RedistrictedDate %in% c("20060101")])
cq204$RepVotesMajorPercent[cq204$district %in% dropdist] <- NA
cq206$RepVotesMajorPercent[cq206$district %in% dropdist] <- NA

# Missing if Any of the Election Uncontested (or not fought between two parties)
table(cq204$PluralityParty)
table(cq206$PluralityParty)
(dropdist <- cq204$district[cq204$PluralityParty %in% "I"])
cq204$RepVotesMajorPercent[cq204$district %in% dropdist] <- NA
cq206$RepVotesMajorPercent[cq206$district %in% dropdist] <- NA
table(cq204$RepVotesMajorPercent %in% c(0,100))
table(cq206$RepVotesMajorPercent %in% c(0,100))
dropdist1 <- cq204$district[cq204$RepVotesMajorPercent %in% c(0,100)]
dropdist2 <- cq204$district[cq206$RepVotesMajorPercent %in% c(0,100)]
(dropdist <- sort(unique(c(dropdist1,dropdist2))))
cq204$RepVotesMajorPercent[cq204$district %in% dropdist] <- NA
cq206$RepVotesMajorPercent[cq206$district %in% dropdist] <- NA

dropped1 <- cq204$district[is.na(cq204$RepVotesMajorPercent)]
dropped2 <- cq206$district[is.na(cq206$RepVotesMajorPercent)]
(dropped <- sort(unique(c(dropped1,dropped2))))
cq204$RepVotesMajorPercent[cq204$district %in% dropped] <- NA
cq206$RepVotesMajorPercent[cq206$district %in% dropped] <- NA

statePVI <- 
  ( (cq104$RepVotesMajorPercent - cq004$RepVotesMajorPercentAll) + 
      (cq106$RepVotesMajorPercent - cq006$RepVotesMajorPercentAll) )/2
districtPVI04 <- cq204$RepVotesMajorPercent - cq104$RepVotesMajorPercent[match(cq204$State,cq104$Area)]
districtPVI04 <- districtPVI04[match(cq206$district,cq204$district)]
districtPVI06 <- cq206$RepVotesMajorPercent - cq106$RepVotesMajorPercent[match(cq206$State,cq106$Area)]
districtPVI <- (districtPVI04 + districtPVI06)/2
statePVI <- statePVI[match(cq206$State,cq106$Area)]
cor(districtPVI,statePVI, use="pairwise")

PVI08h <- data.frame(statePVI, districtPVI,
                     State = cq206$State,
                     district = cq206$district)
saveRDS(PVI08h, "data/PVI08_house.rds")

###########################
## Partisan Voting Index ##
###########################

## Two-Party Republican Proportion by State (2000)
d$hreppr00_state <- 
  cq100$RepVotesMajorPercent[match(d$state,cq100$Area)]
summary(d$hreppr00_state)
unique(d$state[is.na(d$hreppr00_state)]) # Only DC is Good
#cq200$state[grep("",cq200$state)]

## Two-Party Republican Proportion by State (2002)
d$hreppr02_state <- 
  cq102$RepVotesMajorPercent[match(d$state,cq102$Area)]
summary(d$hreppr02_state)
unique(d$state[is.na(d$hreppr02_state)]) # Only DC is Good
#cq202$state[grep("",cq202$state)]

## Two-Party Republican Proportion by State (2004)
d$hreppr04_state <- 
  cq104$RepVotesMajorPercent[match(d$state,cq104$Area)]
summary(d$hreppr04_state)
unique(d$state[is.na(d$hreppr04_state)]) # Only DC is Good
#cq204$state[grep("",cq204$state)]

## Two-Party Republican Proportion by State (2006)
d$hreppr06_state <- 
  cq106$RepVotesMajorPercent[match(d$state,cq106$Area)]
summary(d$hreppr06_state)
unique(d$state[is.na(d$hreppr06_state)]) # Only DC is Good
#cq206$state[grep("",cq206$state)]

## Partisan Voting Index by State (2000-2006)
d$hpvi_state <- (d$hreppr00_state - cq000$RepVotesMajorPercentAll + 
                   d$hreppr02_state - cq002$RepVotesMajorPercentAll + 
                   d$hreppr04_state - cq004$RepVotesMajorPercentAll + 
                   d$hreppr06_state - cq006$RepVotesMajorPercentAll)/4
d$hpvi_state[d$state=="Vermont"] <- NA
d$hpvirep_state <- d$hpvi_state
d$hpvirep_state[which(d$hpvi_state<0)] <- 0
d$hpvidem_state <- -d$hpvi_state
d$hpvidem_state[which(d$hpvi_state>0)] <- 0

## Partisan Change Index by State (2000-2004)
d$hpci_state <- (d$hreppr04_state - d$hreppr00_state) - 
  (cq004$RepVotesMajorPercentAll - cq000$RepVotesMajorPercentAll)
d$hpci_state[d$state=="Vermont"] <- NA
d$hpcirep_state <- d$hpci_state
d$hpcirep_state[which(d$hpci_state<0)] <- 0
d$hpcidem_state <- -d$hpci_state
d$hpcidem_state[which(d$hpci_state>0)] <- 0

## Partisan Voting Index by State (2004-2006)
d$hpvi2_state <- (d$hreppr04_state - cq004$RepVotesMajorPercentAll + 
                   d$hreppr06_state - cq006$RepVotesMajorPercentAll)/2
d$hpvi2_state[d$state=="Vermont"] <- NA
d$hpvi2rep_state <- d$hpvi2_state
d$hpvi2rep_state[which(d$hpvi2_state<0)] <- 0
d$hpvi2dem_state <- -d$hpvi2_state
d$hpvi2dem_state[which(d$hpvi2_state>0)] <- 0

## Partisan Change Index by State (2004-2006)
d$hpci2_state <- (d$hreppr06_state - d$hreppr04_state) - 
  (cq006$RepVotesMajorPercentAll - cq004$RepVotesMajorPercentAll)
d$hpci2_state[d$state=="Vermont"] <- NA
d$hpci2rep_state <- d$hpci2_state
d$hpci2rep_state[which(d$hpci2_state<0)] <- 0
d$hpci2dem_state <- -d$hpci2_state
d$hpci2dem_state[which(d$hpci2_state>0)] <- 0

## Two-Party Republican Proportion by District (2004)
d$hreppr04_district <- 
  cq204$RepVotesMajorPercent[match(d$district,cq204$district)]
summary(d$hreppr04_district)
dropped_cces <- sort(unique(d$district[is.na(d$hreppr04_district)]))
dropped_cces[!dropped_cces %in% dropped] 
# FL 24 no election in 2004
# FL 25 no election in 2004
#cq204$district[grep("",cq204$district)]

## Two-Party Republican Proportion by District (2006)
d$hreppr06_district <- 
  cq206$RepVotesMajorPercent[match(d$district,cq206$district)]
summary(d$hreppr06_district)
dropped_cces <- sort(unique(d$district[is.na(d$hreppr06_district)]))
dropped_cces[!dropped_cces %in% dropped] 
# FL 24 no election in 2004
# FL 25 no election in 2004
#cq206$district[grep("",cq206$district)]

## Partisan Voting Index by District (2004-2006)
d$hpvi2_district <- (d$hreppr04_district - d$hreppr04_state + 
                 d$hreppr06_district - d$hreppr06_state)/2
d$hpvi2rep_district <- d$hpvi2_district
d$hpvi2rep_district[which(d$hpvi2_district<0)] <- 0
d$hpvi2dem_district <- -d$hpvi2_district
d$hpvi2dem_district[which(d$hpvi2_district>0)] <- 0

## Partisan Change Index by District (2004-2006)
d$hpci2_district <- (d$hreppr06_district - d$hreppr04_district) - 
  (d$hreppr06_state - d$hreppr04_state) 
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

saveRDS(d,file="data/cces08s_cxt2.rds")