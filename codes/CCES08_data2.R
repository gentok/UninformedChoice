################################################################################# 
## File Name: CCES08_data2.R                                                   ##
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
dloc <- "data/cces08s.rds"
cqloc <- readRDS("data/cqloc_president.rds")
cq0loc <- cqloc[["nation"]]
cq1loc <- cqloc[["state"]]
cq2loc <- cqloc[["county"]]

###############
## Load Data ##
###############

d <- readRDS(dloc)
cq0 <- readRDS(cq0loc)
cq000 <- cq0[which(cq0$year %in% c(2000)),]
cq004 <- cq0[which(cq0$year %in% c(2004)),]
cq1 <- readRDS(cq1loc)
cq100 <- cq1[which(cq1$year %in% c(2000)),]
cq104 <- cq1[which(cq1$year %in% c(2004)),]
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
#cq2$county <- sub("  "," ",cq2$county)
cq2$county <- str_squish(cq2$county)
cq200 <- cq2[which(cq2$year %in% c(2000)),]
cq204 <- cq2[which(cq2$year %in% c(2004)),]

# Calculate State PVI
statePVI <- 
  ( (cq100$RepVotesMajorPercent - cq000$RepVotesMajorPercentAll) + 
      (cq104$RepVotesMajorPercent - cq004$RepVotesMajorPercentAll) )/2
statePVI <- statePVI[match(cq204$State,cq104$Area)]
summary(statePVI)

# Calculate County PVI

## Make Adjustments
## 1. Clifton Forge City gave up city status and merged to Alleghany county after 2001
## Combine Clifton City and Alleghany County in 2000
cq200$RepVotes[cq200$county%in%c("CLIFTON FORGE CITY VA","ALLEGHANY VA")] <-
  cq200$RepVotes[cq200$county%in%c("CLIFTON FORGE CITY VA")] + 
  cq200$RepVotes[cq200$county%in%c("ALLEGHANY VA")]
cq200$DemVotes[cq200$county%in%c("CLIFTON FORGE CITY VA","ALLEGHANY VA")] <-
  cq200$DemVotes[cq200$county%in%c("CLIFTON FORGE CITY VA")] + 
  cq200$DemVotes[cq200$county%in%c("ALLEGHANY VA")]
cq200$RepVotesMajorPercent[cq200$county%in%c("CLIFTON FORGE CITY VA","ALLEGHANY VA")] <-
  (cq200$RepVotes[cq200$county%in%c("CLIFTON FORGE CITY VA","ALLEGHANY VA")] /
     (cq200$RepVotes[cq200$county%in%c("CLIFTON FORGE CITY VA","ALLEGHANY VA")] + 
        cq200$DemVotes[cq200$county%in%c("CLIFTON FORGE CITY VA","ALLEGHANY VA")]))*100
## 2. Kansas City MO was part of Jackson County in 2000
cq204$RepVotes[cq204$county%in%c("KANSAS CITY MO","JACKSON MO")] <-
  cq204$RepVotes[cq204$county%in%c("KANSAS CITY MO")] + 
  cq204$RepVotes[cq204$county%in%c("JACKSON MO")]
cq204$DemVotes[cq204$county%in%c("KANSAS CITY MO","JACKSON MO")] <-
  cq204$DemVotes[cq204$county%in%c("KANSAS CITY MO")] + 
  cq204$DemVotes[cq204$county%in%c("JACKSON MO")]
cq204$RepVotesMajorPercent[cq204$county%in%c("KANSAS CITY MO","JACKSON MO")] <-
  (cq204$RepVotes[cq204$county%in%c("KANSAS CITY MO","JACKSON MO")] /
     (cq204$RepVotes[cq204$county%in%c("KANSAS CITY MO","JACKSON MO")] + 
        cq204$DemVotes[cq204$county%in%c("KANSAS CITY MO","JACKSON MO")]))*100
## 3. Broomfield CO was part of Boulder County in 2000
cq204$RepVotes[cq204$county%in%c("BROOMFIELD CO","BOULDER CO")] <-
  cq204$RepVotes[cq204$county%in%c("BROOMFIELD CO")] + 
  cq204$RepVotes[cq204$county%in%c("BOULDER CO")]
cq204$DemVotes[cq204$county%in%c("BROOMFIELD CO","BOULDER CO")] <-
  cq204$DemVotes[cq204$county%in%c("BROOMFIELD CO")] + 
  cq204$DemVotes[cq204$county%in%c("BOULDER CO")]
cq204$RepVotesMajorPercent[cq204$county%in%c("BROOMFIELD CO","BOULDER CO")] <-
  (cq204$RepVotes[cq204$county%in%c("BROOMFIELD CO","BOULDER CO")] /
     (cq204$RepVotes[cq204$county%in%c("BROOMFIELD CO","BOULDER CO")] + 
        cq204$DemVotes[cq204$county%in%c("BROOMFIELD CO","BOULDER CO")]))*100
## 4. SKAGWAY AK Incorporated into Hoonah-Angoon AK After 2007
cq200$RepVotes[cq200$county%in%c("SKAGWAY AK","HOONAH-ANGOON AK")] <-
  cq200$RepVotes[cq200$county%in%c("SKAGWAY AK")] + 
  cq200$RepVotes[cq200$county%in%c("HOONAH-ANGOON AK")]
cq200$DemVotes[cq200$county%in%c("SKAGWAY AK","HOONAH-ANGOON AK")] <-
  cq200$DemVotes[cq200$county%in%c("SKAGWAY AK")] + 
  cq200$DemVotes[cq200$county%in%c("HOONAH-ANGOON AK")]
cq200$RepVotesMajorPercent[cq200$county%in%c("SKAGWAY AK","HOONAH-ANGOON AK")] <-
  (cq200$RepVotes[cq200$county%in%c("SKAGWAY AK","HOONAH-ANGOON AK")] /
     (cq200$RepVotes[cq200$county%in%c("SKAGWAY AK","HOONAH-ANGOON AK")] + 
        cq200$DemVotes[cq200$county%in%c("SKAGWAY AK","HOONAH-ANGOON AK")]))*100
cq204$RepVotes[cq204$county%in%c("SKAGWAY AK","HOONAH-ANGOON AK")] <-
  cq204$RepVotes[cq204$county%in%c("SKAGWAY AK")] + 
  cq204$RepVotes[cq204$county%in%c("HOONAH-ANGOON AK")]
cq204$DemVotes[cq204$county%in%c("SKAGWAY AK","HOONAH-ANGOON AK")] <-
  cq204$DemVotes[cq204$county%in%c("SKAGWAY AK")] + 
  cq204$DemVotes[cq204$county%in%c("HOONAH-ANGOON AK")]
cq204$RepVotesMajorPercent[cq204$county%in%c("SKAGWAY AK","HOONAH-ANGOON AK")] <-
  (cq204$RepVotes[cq204$county%in%c("SKAGWAY AK","HOONAH-ANGOON AK")] /
     (cq204$RepVotes[cq204$county%in%c("SKAGWAY AK","HOONAH-ANGOON AK")] + 
        cq204$DemVotes[cq204$county%in%c("SKAGWAY AK","HOONAH-ANGOON AK")]))*100


## (2000 County)
countyPVI00 <- cq200$RepVotesMajorPercent - cq100$RepVotesMajorPercent[match(cq200$State,cq100$Area)]
countyPVI00 <- countyPVI00[match(cq204$county,cq200$county)]
## Check if any county existing in 2004 missing in 2000
summary(countyPVI00)
cq204$county[is.na(countyPVI00)]
# Kansas City MO was part of Jackson County in 2000
countyPVI00[cq204$county=="KANSAS CITY MO"] <- 
  countyPVI00[cq204$county=="JACKSON MO"]
# Broomfield CO was part of Boulder County in 2000
countyPVI00[cq204$county=="BROOMFIELD CO"] <- 
  countyPVI00[cq204$county=="BOULDER CO"]
# Other NAs are Ignorable
summary(countyPVI00)
cq204$county[is.na(countyPVI00)] 

## (2004 County)
countyPVI04 <- cq204$RepVotesMajorPercent - cq104$RepVotesMajorPercent[match(cq204$State,cq104$Area)]
## Check if any county existing in 2000 missing in 2004
countyPVI04x <- countyPVI04[match(cq200$county,cq204$county)]
summary(countyPVI04x)
cq200$county[is.na(countyPVI04x)] # Already Adjusted
# No NAs
summary(countyPVI04)

# Combine
countyPVI <- (countyPVI00 + countyPVI04)/2
summary(countyPVI)
cq204$county[is.na(countyPVI)] 

# Check Correlation (Very Low)
cor(countyPVI,statePVI, use="pairwise")

PVI08p <- data.frame(statePVI,countyPVI,
                     State = cq204$State,
                     County = cq204$county,
                     County_Original = cq204$Area)
saveRDS(PVI08p, "data/PVI08_president.rds")

## Fix County Values of Data

d$county <- sub("DUPAGE IL","DU PAGE IL",d$county)
# d$county <- sub("DE KALB IN","DEKALB IN",d$county)
d$county <- sub("LAPORTE IN","LA PORTE IN",d$county)
# d$county <- sub("OBRIEN IA","O\'BRIEN IA",d$county)
# d$county <- sub("EAST BATON ROUGE LA","EAST BATON ROUGE PARISH LA",d$county)
d$county <- sub("LIVINGSTON LA","LIVINGSTON PARISH LA",d$county)
d$county <- sub("QUEEN ANNE\'S MD","QUEEN ANNES MD",d$county)
d$county <- sub("DESOTO MS","DE SOTO MS",d$county)
# d$county <- sub("MC KEAN PA","MCKEAN PA",d$county)
d$county <- sub("COVINGTON VA","COVINGTON CITY VA",d$county)
d$county <- sub("PETERSBURG VA","PETERSBURG CITY VA",d$county)
d$county <- sub("MANASSAS VA","MANASSAS CITY VA",d$county)
# d$county <- sub("CHARLOTTESVILLE VA","CHARLOTTESVILLE CITY VA",d$county)
d$county <- sub("HARRISONBURG VA","HARRISONBURG CITY VA",d$county)
# d$county <- sub("VIRGINIA BEACH VA","VIRGINIA BEACH CITY VA",d$county)
d$county <- sub("DEWITT TX","DE WITT TX",d$county)

d$county <- sub("GRAND TRAVERS MI","GRAND TRAVERSE MI",d$county)
d$county <- sub("SAN BERNARDIN CA","SAN BERNARDINO CA",d$county)
d$county <- sub("PRINCE GEORGE MD","PRINCE GEORGES MD",d$county)
d$county <- sub("LAFAYETTE LA","LAFAYETTE PARISH LA",d$county)
d$county <- sub("ALEXANDRIA VA","ALEXANDRIA CITY VA",d$county)
d$county <- sub("ALLEN LA","ALLEN PARISH LA",d$county)
d$county <- sub("LASALLE IL","LA SALLE IL",d$county)
d$county <- sub("SAN LUIS OBIS CA","SAN LUIS OBISPO CA",d$county)
d$county <- sub("ST. TAMMANY LA","ST. TAMMANY PARISH LA",d$county)
d$county <- sub("NEWPORT NEWS VA","NEWPORT NEWS CITY VA",d$county)
d$county <- sub("ST. MARY LA","ST. MARY PARISH LA",d$county)
d$county <- sub("EAST BATON RO LA","EAST BATON ROUGE PARISH LA",d$county)
d$county <- sub("BOSSIER LA","BOSSIER PARISH LA",d$county)
d$county <- sub("CHESAPEAKE VA","CHESAPEAKE CITY VA",d$county)
d$county <- sub("EVANGELINE LA","EVANGELINE PARISH LA",d$county)
d$county <- sub("CADDO LA","CADDO PARISH LA",d$county)
d$county <- sub("TANGIPAHOA LA","TANGIPAHOA PARISH LA",d$county)
d$county <- sub("HAMPTON VA","HAMPTON CITY VA",d$county)
d$county <- sub("VIRGINIA BEAC VA","VIRGINIA BEACH CITY VA",d$county)
d$county <- sub("CALCASIEU LA","CALCASIEU PARISH LA",d$county)
d$county <- sub("ORLEANS LA","ORLEANS PARISH LA",d$county)
d$county <- sub("LAFOURCHE LA","LAFOURCHE PARISH LA",d$county)
d$county <- sub("JEFFERSON LA","JEFFERSON PARISH LA",d$county)
d$county <- sub("PRINCE WILLIA VA","PRINCE WILLIAM VA",d$county)
d$county <- sub("IBERIA LA","IBERIA PARISH LA",d$county)
d$county <- sub("LEWIS AND CLA MT","LEWIS AND CLARK MT",d$county)
d$county <- sub("NORFOLK VA","NORFOLK CITY VA",d$county)
d$county <- sub("AVOYELLES LA","AVOYELLES PARISH LA",d$county)
d$county <- sub("LYNCHBURG VA","LYNCHBURG CITY VA",d$county)
d$county <- sub("ACADIA LA","ACADIA PARISH LA",d$county)
d$county <- sub("FALLS CHURCH VA","FALLS CHURCH CITY VA",d$county)
d$county <- sub("CHARLOTTESVIL VA","CHARLOTTESVILLE CITY VA",d$county)
d$county <- sub("TERREBONNE LA","TERREBONNE PARISH LA",d$county)
d$county <- sub("BRISTOL VA","BRISTOL CITY VA",d$county)
d$county <- sub("CAPE GIRARDEA MO","CAPE GIRARDEAU MO",d$county)
d$county <- sub("RAPIDES LA","RAPIDES PARISH LA",d$county)
d$county <- sub("STAUNTON VA","STAUNTON CITY VA",d$county)
d$county <- sub("NORTHUMBERLAN PA","NORTHUMBERLAND PA",d$county)
d$county <- sub("VERMILION LA","VERMILION PARISH LA",d$county)
d$county <- sub("LINCOLN LA","LINCOLN PARISH LA",d$county)
d$county <- sub("PORTSMOUTH VA","PORTSMOUTH CITY VA",d$county)
d$county <- sub("CLAIBORNE LA","CLAIBORNE PARISH LA",d$county)
d$county <- sub("ASCENSION LA","ASCENSION PARISH LA",d$county)
d$county <- sub("DANVILLE VA","DANVILLE CITY VA",d$county)
d$county <- sub("ST. MARTIN LA","ST. MARTIN PARISH LA",d$county)
d$county <- sub("ST. MARY'S MD","ST. MARYS MD",d$county)
d$county <- sub("LAMOURE ND","LA MOURE ND",d$county)
d$county <- sub("WEST BATON RO LA","WEST BATON ROUGE PARISH LA",d$county)
d$county <- sub("BEAUREGARD LA","BEAUREGARD PARISH LA",d$county)
d$county <- sub("PLAQUEMINES LA","PLAQUEMINES PARISH LA",d$county)
d$county <- sub("SUFFOLK VA","SUFFOLK CITY VA",d$county)
d$county <- sub("SALEM VA","SALEM CITY VA",d$county)
d$county <- sub("WAYNESBORO VA","WAYNESBORO CITY VA",d$county)
d$county <- sub("ST. LANDRY LA","ST. LANDRY PARISH LA",d$county)
d$county <- sub("WINN LA","WINN PARISH LA",d$county)
d$county <- sub("STE. GENEVIEV MO","STE. GENEVIEVE MO",d$county)
d$county <- sub("ST. BERNARD LA","ST. BERNARD PARISH LA",d$county)
d$county <- sub("VERNON LA","VERNON PARISH LA",d$county)
d$county <- sub("OUACHITA LA","OUACHITA PARISH LA",d$county)
d$county <- sub("WILLIAMSBURG VA","WILLIAMSBURG CITY VA",d$county)
d$county <- sub("GRANT LA","GRANT PARISH LA",d$county)
d$county <- sub("WINCHESTER VA","WINCHESTER CITY VA",d$county)
d$county <- sub("ST. JOHN THE LA","ST. JOHN THE BAPTIST PARISH LA",d$county)
d$county <- sub("DE SOTO LA","DE SOTO PARISH LA",d$county)
d$county <- sub("JEFFERSON DAV LA","JEFFERSON DAVIS PARISH LA",d$county)
d$county <- sub("BUENA VISTA VA","BUENA VISTA CITY VA",d$county)
d$county <- sub("EAST CARROLL LA","EAST CARROLL PARISH LA",d$county)
d$county <- sub("NATCHITOCHES LA","NATCHITOCHES PARISH LA",d$county)
d$county <- sub("LA SALLE LA","LA SALLE PARISH LA",d$county)
d$county <- sub("JEFFERSON DAV MS","JEFFERSON DAVIS MS",d$county)
d$county <- sub("MOREHOUSE LA","MOREHOUSE PARISH LA",d$county)
d$county <- sub("IBERVILLE LA","IBERVILLE PARISH LA",d$county)
d$county <- sub("COLONIAL HEIG VA","COLONIAL HEIGHTS CITY VA",d$county)
d$county <- sub("JACKSON LA","JACKSON PARISH LA",d$county)
d$county <- sub("POQUOSON VA","POQUOSON CITY VA",d$county)
d$county <- sub("WEBSTER LA","WEBSTER PARISH LA",d$county)
d$county <- sub("RADFORD VA","RADFORD CITY VA",d$county)
d$county <- sub("EAST FELICIAN LA","EAST FELICIANA PARISH LA",d$county)
d$county <- sub("CONCORDIA LA","CONCORDIA PARISH LA",d$county)
d$county <- sub("WASHINGTON LA","WASHINGTON PARISH LA",d$county)
d$county <- sub("UNION LA","UNION PARISH LA",d$county)
d$county <- sub("ST. JAMES LA","ST. JAMES PARISH LA",d$county)
d$county <- sub("ASSUMPTION LA","ASSUMPTION PARISH LA",d$county)
d$county <- sub("HOPEWELL VA","HOPEWELL CITY VA",d$county)
d$county <- sub("NORTON VA","NORTON CITY VA",d$county)
d$county <- sub("POINTE COUPEE LA","POINTE COUPEE PARISH LA",d$county)
d$county <- sub("FRANKLIN LA","FRANKLIN PARISH LA",d$county)
d$county <- sub("SABINE LA","SABINE PARISH LA",d$county)
d$county <- sub("FREDERICKSBUR VA","FREDERICKSBURG CITY VA",d$county)
d$county <- sub("ST. CHARLES LA","ST. CHARLES PARISH LA",d$county)
d$county <- sub("RED RIVER LA","RED RIVER PARISH LA",d$county)
d$county <- sub("BROOMFIELD CO","BOULDER CO",d$county) # Broomfield was part of Boulder County until 2001
d$county <- sub("NORTHAMPTON CITY VA","NORTHAMPTON VA",d$county)
# Alaska
d$county <- sub("FAIRBANKS NOR AK","FAIRBANKS NORTH STAR AK",d$county)
d$county <- sub("SKAGWAY-HOONA AK","HOONAH-ANGOON AK",d$county) # Incorporated after 2007
d$county <- sub("MATANUSKA-SUS AK","MATANUSKA-SUSITNA AK",d$county)
d$county <- sub("KENAI PENINSU AK","KENAI PENINSULA AK",d$county)
d$county <- sub("KETCHIKAN GAT AK","KETCHIKAN GATEWAY AK",d$county)
d$county <- sub("VALDEZ-CORDOV AK","VALDEZ-CORDOVA AK",d$county)

# unique(d$county[is.na(d$reppr00_county)])
# cq200$county[grep("FIELD",cq200$county)]

###########################
## Partisan Voting Index ##
###########################

## Two-Party Republican Proportion by State (2000)
d$reppr00_state <- 
  cq100$RepVotesMajorPercent[match(d$state,cq100$Area)]
summary(d$reppr00_state)
unique(d$state[is.na(d$reppr00_state)]) # No Value is Good
#cq200$state[grep("",cq200$state)]

## Two-Party Republican Proportion by State (2004)
d$reppr04_state <- 
  cq104$RepVotesMajorPercent[match(d$state,cq104$Area)]
summary(d$reppr04_state)
unique(d$state[is.na(d$reppr04_state)]) # No Value is Good
#cq204$state[grep("",cq204$state)]

## Partisan Voting Index by State (2000-2004)
d$pvi_state <- (d$reppr00_state - cq000$RepVotesMajorPercentAll + 
                   d$reppr04_state - cq004$RepVotesMajorPercentAll)/2
d$pvirep_state <- d$pvi_state
d$pvirep_state[which(d$pvi_state<0)] <- 0
d$pvidem_state <- -d$pvi_state
d$pvidem_state[which(d$pvi_state>0)] <- 0

## Partisan Change Index by State (2000-2004)
d$pci_state <- (d$reppr04_state - d$reppr00_state) - 
  (cq004$RepVotesMajorPercentAll - cq000$RepVotesMajorPercentAll)
d$pcirep_state <- d$pci_state
d$pcirep_state[which(d$pci_state<0)] <- 0
d$pcidem_state <- -d$pci_state
d$pcidem_state[which(d$pci_state>0)] <- 0

## Two-Party Republican Proportion by County (2000)
d$reppr00_county <- 
  cq200$RepVotesMajorPercent[match(d$county,cq200$county)]
d$reppr00_county[which(d$county=="DISTRICT OF C DC")] <- 
  cq100$RepVotesMajorPercent[which(cq100$Area=="District of Columbia")]
# d$reppr00_county[which(d$state=="Alaska")] <- 
#   cq100$RepVotesMajorPercent[which(cq100$Area=="Alaska")]
summary(d$reppr00_county)
unique(d$county[is.na(d$reppr00_county)])
#cq200$county[grep("",cq200$county)]

## Two-Party Republican Proportion by County (2004)
d$reppr04_county <- 
  cq204$RepVotesMajorPercent[match(d$county,cq204$county)]
d$reppr04_county[which(d$county=="DISTRICT OF C DC")] <- 
  cq104$RepVotesMajorPercent[which(cq104$Area=="District of Columbia")]
# d$reppr04_county[which(d$state=="Alaska")] <- 
#   cq104$RepVotesMajorPercent[which(cq104$Area=="Alaska")]
summary(d$reppr04_county)
unique(d$county[is.na(d$reppr04_county)])
#cq204$county[grep("",cq204$county)]

## Partisan Voting Index by County (2000-2004)
d$pvi_county <- (d$reppr00_county - d$reppr00_state + 
                 d$reppr04_county - d$reppr04_state)/2
d$pvirep_county <- d$pvi_county
d$pvirep_county[which(d$pvi_county<0)] <- 0
d$pvidem_county <- -d$pvi_county
d$pvidem_county[which(d$pvi_county>0)] <- 0

## Partisan Change Index by County (2000-2004)
d$pci_county <- (d$reppr04_county - d$reppr00_county) - 
  (d$reppr04_state - d$reppr00_state) 
d$pcirep_county <- d$pci_county
d$pcirep_county[which(d$pci_county<0)] <- 0
d$pcidem_county <- -d$pci_county
d$pcidem_county[which(d$pci_county>0)] <- 0

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

saveRDS(d,file="data/cces08s_cxt.rds")
