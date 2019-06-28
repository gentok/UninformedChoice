################################################################################# 
## File Name: CCES16_data2.R                                                   ##
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
dloc <- "data/cces16s.rds"
cqloc <- readRDS("data/cqloc_president.rds")
cq0loc <- cqloc[["nation"]]
cq1loc <- cqloc[["state"]]
cq2loc <- cqloc[["county"]]

###############
## Load Data ##
###############

d <- readRDS(dloc)
cq0 <- readRDS(cq0loc)
cq008 <- cq0[which(cq0$year %in% c(2008)),]
cq012 <- cq0[which(cq0$year %in% c(2012)),]
cq1 <- readRDS(cq1loc)
cq108 <- cq1[which(cq1$year %in% c(2008)),]
cq112 <- cq1[which(cq1$year %in% c(2012)),]
cq2 <- readRDS(cq2loc)
cq2$state_abb <- state.abb[match(cq2$State,state.name)]
cq2$state_abb[cq2$State=="District of Columbia"] <- "DC"
cq2$county <- paste(toupper(cq2$Area),
                    toupper(cq2$AreaType),
                    cq2$state_abb,
                    sep=" ")
cq2$county <- sub("COUNTY ","",cq2$county)
cq2$county <- sub("WARD DC","DC",cq2$county)
cq2$county <- sub("CITY CITY","CITY",cq2$county)
cq2$county <- sub("  "," ",cq2$county)
cq208 <- cq2[which(cq2$year %in% c(2008)),]
cq212 <- cq2[which(cq2$year %in% c(2012)),]

# Calculate State PVI
statePVI <- 
  ( (cq108$RepVotesMajorPercent - cq008$RepVotesMajorPercentAll) + 
      (cq112$RepVotesMajorPercent - cq012$RepVotesMajorPercentAll) )/2
statePVI <- statePVI[match(cq212$State,cq112$Area)]
summary(statePVI)

# Calculate County PVI

## Make Adjustment
## 1. SKAGWAY AK Incorporated into Hoonah-Angoon AK After 2007
cq208$RepVotes[cq208$county%in%c("SKAGWAY AK","HOONAH-ANGOON AK")] <-
  cq208$RepVotes[cq208$county%in%c("SKAGWAY AK")] + 
  cq208$RepVotes[cq208$county%in%c("HOONAH-ANGOON AK")]
cq208$DemVotes[cq208$county%in%c("SKAGWAY AK","HOONAH-ANGOON AK")] <-
  cq208$DemVotes[cq208$county%in%c("SKAGWAY AK")] + 
  cq208$DemVotes[cq208$county%in%c("HOONAH-ANGOON AK")]
cq208$RepVotesMajorPercent[cq208$county%in%c("SKAGWAY AK","HOONAH-ANGOON AK")] <-
  (cq208$RepVotes[cq208$county%in%c("SKAGWAY AK","HOONAH-ANGOON AK")] /
     (cq208$RepVotes[cq208$county%in%c("SKAGWAY AK","HOONAH-ANGOON AK")] + 
        cq208$DemVotes[cq208$county%in%c("SKAGWAY AK","HOONAH-ANGOON AK")]))*100
cq212$RepVotes[cq212$county%in%c("SKAGWAY AK","HOONAH-ANGOON AK")] <-
  cq212$RepVotes[cq212$county%in%c("SKAGWAY AK")] + 
  cq212$RepVotes[cq212$county%in%c("HOONAH-ANGOON AK")]
cq212$DemVotes[cq212$county%in%c("SKAGWAY AK","HOONAH-ANGOON AK")] <-
  cq212$DemVotes[cq212$county%in%c("SKAGWAY AK")] + 
  cq212$DemVotes[cq212$county%in%c("HOONAH-ANGOON AK")]
cq212$RepVotesMajorPercent[cq212$county%in%c("SKAGWAY AK","HOONAH-ANGOON AK")] <-
  (cq212$RepVotes[cq212$county%in%c("SKAGWAY AK","HOONAH-ANGOON AK")] /
     (cq212$RepVotes[cq212$county%in%c("SKAGWAY AK","HOONAH-ANGOON AK")] + 
        cq212$DemVotes[cq212$county%in%c("SKAGWAY AK","HOONAH-ANGOON AK")]))*100


## (2008 County PVI)
countyPVI08 <- cq208$RepVotesMajorPercent - cq108$RepVotesMajorPercent[match(cq208$State,cq108$Area)]
## Check if any county existing in 2012 missing in 2008
countyPVI08 <- countyPVI08[match(cq212$county,cq208$county)]
summary(countyPVI08)
cq212$county[is.na(countyPVI08)] # NAs are ignorable
## (2012 County PVI)
countyPVI12 <- cq212$RepVotesMajorPercent - cq112$RepVotesMajorPercent[match(cq212$State,cq112$Area)]
## Check if any county existing in 2008 missing in 2012
countyPVI12x <- countyPVI12[match(cq208$county,cq212$county)]
cq208$county[is.na(countyPVI12x)] # NAs are ignorable
## NO NAs
summary(countyPVI12)
# Combine
countyPVI <- (countyPVI08 + countyPVI12)/2
summary(countyPVI)

# Check Correlation (Very Low)
cor(countyPVI,statePVI, use="pairwise")

PVI16p <- data.frame(statePVI,countyPVI,
                     State = cq212$State,
                     County = cq212$county,
                     County_Original = cq212$Area)
saveRDS(PVI16p, "data/PVI16_president.rds")

## Fix County Values of Data
d$county <- sub("BOROUGH AK$","AK", d$county)
d$county <- sub("CENSUS AREA AK$","AK", d$county)
#d$county <- sub(" LA$"," PARISH LA", d$county)
d$county <- sub("DUPAGE IL","DU PAGE IL",d$county)
d$county <- sub("DE KALB IN","DEKALB IN",d$county)
d$county <- sub("LAPORTE IN","LA PORTE IN",d$county)
d$county <- sub("OBRIEN IA","O\'BRIEN IA",d$county)
d$county <- sub("EAST BATON ROUGE LA","EAST BATON ROUGE PARISH LA",d$county)
d$county <- sub("LIVINGSTON LA","LIVINGSTON PARISH LA",d$county)
d$county <- sub("QUEEN ANNE\'S MD","QUEEN ANNES MD",d$county)
d$county <- sub("DESOTO MS","DE SOTO MS",d$county)
d$county <- sub("MC KEAN PA","MCKEAN PA",d$county)
d$county <- sub("COVINGTON VA","COVINGTON CITY VA",d$county)
d$county <- sub("PETERSBURG VA","PETERSBURG CITY VA",d$county)
d$county <- sub("MANASSAS VA","MANASSAS CITY VA",d$county)
d$county <- sub("CHARLOTTESVILLE VA","CHARLOTTESVILLE CITY VA",d$county)
d$county <- sub("HARRISONBURG VA","HARRISONBURG CITY VA",d$county)
d$county <- sub("VIRGINIA BEACH VA","VIRGINIA BEACH CITY VA",d$county)
d$county <- sub("DEWITT TX","DE WITT TX",d$county)

###########################
## Partisan Voting Index ##
###########################

## Two-Party Republican Proportion by State (2008)
d$reppr08_state <- 
  cq108$RepVotesMajorPercent[match(d$state,cq108$Area)]
summary(d$reppr08_state)
unique(d$state[is.na(d$reppr08_state)])
#cq208$state[grep("",cq208$state)]

## Two-Party Republican Proportion by State (2012)
d$reppr12_state <- 
  cq112$RepVotesMajorPercent[match(d$state,cq112$Area)]
summary(d$reppr12_state)
unique(d$state[is.na(d$reppr12_state)])
#cq212$state[grep("",cq212$state)]

## Partisan Voting Index by State (2008-2012)
d$pvi_state <- (d$reppr08_state - cq008$RepVotesMajorPercentAll + 
                   d$reppr12_state - cq012$RepVotesMajorPercentAll)/2
d$pvirep_state <- d$pvi_state
d$pvirep_state[which(d$pvi_state<0)] <- 0
d$pvidem_state <- -d$pvi_state
d$pvidem_state[which(d$pvi_state>0)] <- 0
summary(d$pvi_state)

## Partisan Change Index by State (2008-2012)
d$pci_state <- (d$reppr12_state - d$reppr08_state) - 
                  (cq012$RepVotesMajorPercentAll - cq008$RepVotesMajorPercentAll)
d$pcirep_state <- d$pci_state
d$pcirep_state[which(d$pci_state<0)] <- 0
d$pcidem_state <- -d$pci_state
d$pcidem_state[which(d$pci_state>0)] <- 0
summary(d$pci_state)

## Two-Party Republican Proportion by County (2008)
d$reppr08_county <- 
  cq208$RepVotesMajorPercent[match(d$county,cq208$county)]
d$reppr08_county[which(d$county=="DISTRICT OF COLUMBIA DC")] <- 
  cq108$RepVotesMajorPercent[which(cq108$Area=="District of Columbia")]
# d$reppr08_county[which(d$state=="Alaska")] <- 
#   cq108$RepVotesMajorPercent[which(cq108$Area=="Alaska")]
summary(d$reppr08_county)
unique(d$county[is.na(d$reppr08_county)]) # 84 cases with missing county name

## Two-Party Republican Proportion by County (2012)
d$reppr12_county <- 
  cq212$RepVotesMajorPercent[match(d$county,cq212$county)]
d$reppr12_county[which(d$county=="DISTRICT OF COLUMBIA DC")] <- 
  cq112$RepVotesMajorPercent[which(cq112$Area=="District of Columbia")]
# d$reppr12_county[which(d$state=="Alaska")] <- 
#   cq112$RepVotesMajorPercent[which(cq112$Area=="Alaska")]
summary(d$reppr12_county)
unique(d$county[is.na(d$reppr12_county)]) # 84 cases with missing county name
#cq212$county[grep("",cq212$county)]

## Partisan Voting Index by County (2008-2012)
d$pvi_county <- (d$reppr08_county - d$reppr08_state + 
                 d$reppr12_county - d$reppr12_state)/2
d$pvirep_county <- d$pvi_county
d$pvirep_county[which(d$pvi_county<0)] <- 0
d$pvidem_county <- -d$pvi_county
d$pvidem_county[which(d$pvi_county>0)] <- 0
summary(d$pvi_county)

## Partisan Voting Index by County (2008-2012)
d$pci_county <- (d$reppr12_county - d$reppr08_county) - 
  (d$reppr12_state - d$reppr08_state)
d$pcirep_county <- d$pci_county
d$pcirep_county[which(d$pci_county<0)] <- 0
d$pcidem_county <- -d$pci_county
d$pcidem_county[which(d$pci_county>0)] <- 0
summary(d$pci_county)

## 3 Category PVI
quantile(abs(d$pvi_state),probs=c(seq(0,1,by=0.1)),na.rm=T)
d$pvi_state_3cat <- factor(NA,levels=c("0-5%","5-10%","10%-"))
d$pvi_state_3cat[which(abs(d$pvi_state)<5)] <- "0-5%"
d$pvi_state_3cat[which(abs(d$pvi_state)>=5)] <- "5-10%"
d$pvi_state_3cat[which(abs(d$pvi_state)>=10)] <- "10%-"
round(table(d$pvi_state_3cat,useNA="always")/
        sum(table(d$pvi_state_3cat, useNA="always")),3)
quantile(abs(d$pvi_county),probs=c(seq(0,1,by=0.1)),na.rm=T)
d$pvi_county_3cat <- factor(NA,levels=c("0-5%","5-10%","10%-"))
d$pvi_county_3cat[which(abs(d$pvi_county)<5)] <- "0-5%"
d$pvi_county_3cat[which(abs(d$pvi_county)>=5)] <- "5-10%"
d$pvi_county_3cat[which(abs(d$pvi_county)>=10)] <- "10%-"
round(table(d$pvi_county_3cat,useNA="always")/
        sum(table(d$pvi_county_3cat, useNA="always")),3)

## 3 Category PCI
quantile(abs(d$pci_state),probs=c(seq(0,1,by=0.1)),na.rm=T)
d$pci_state_3cat <- factor(NA,levels=c("0-1%","1-2%","2%-"))
d$pci_state_3cat[which(abs(d$pci_state)<1)] <- "0-1%"
d$pci_state_3cat[which(abs(d$pci_state)>=1)] <- "1-2%"
d$pci_state_3cat[which(abs(d$pci_state)>=2)] <- "2%-"
round(table(d$pci_state_3cat,useNA="always")/
        sum(table(d$pci_state_3cat, useNA="always")),3)
quantile(abs(d$pci_county),probs=c(seq(0,1,by=0.1)),na.rm=T)
d$pci_county_3cat <- factor(NA,levels=c("0-1%","1-2%","2%-"))
d$pci_county_3cat[which(abs(d$pci_county)<1)] <- "0-1%"
d$pci_county_3cat[which(abs(d$pci_county)>=1)] <- "1-2%"
d$pci_county_3cat[which(abs(d$pci_county)>=2)] <- "2%-"
round(table(d$pci_county_3cat,useNA="always")/
        sum(table(d$pci_county_3cat, useNA="always")),3)

###############
## Save Data ##
###############

saveRDS(d,file="data/cces16s_cxt.rds")
