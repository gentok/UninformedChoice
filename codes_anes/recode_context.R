#' ---
#' title: "Adding Contextual Data to ANES Data"
#' author: "Gento Kato"
#' date: "November 7, 2018"
#' ---

#' 
#' # Preparation
#' 
#' ## Read in Data
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # (only in RStudio)
#+ echo=FALSE
# source("./anes_dataloc.R")
# source("./recode_cummulative.R")
#+
cqnaloc <- dataloc[dataid=="cqna"]
cqstloc <- dataloc[dataid=="cqst"]

#'
#' ## Import Aggregated voting results
#'
cqna <- readRDS(cqnaloc)
cqst <- readRDS(cqstloc)

#'
#' ## PVI Nation
#'

for(i in seq(1972,2016,4)) {
  d$pvi_nation[d$year==i] <- 
    (cqna$RepVotesMajorPercentAll[cqna$year==i-4] + 
       cqna$RepVotesMajorPercentAll[cqna$year==i-8])/2
  d$pvi1_nation[d$year==i] <- 
    cqna$RepVotesMajorPercentAll[cqna$year==i-4]
}

#'
#' ## PVI State
#'

for(i in seq(1972,2016,4)) {
  spvi1 <- 
    cqst[cqst$year==i-4,]$RepVotesMajorPercent - cqna[cqna$year==i-4,]$RepVotesMajorPercent
  spvi <- (spvi1 +  
             cqst[cqst$year==i-8,]$RepVotesMajorPercent - cqna[cqna$year==i-8,]$RepVotesMajorPercent)/2
  stn <- state.abb[match(cqst[cqst$year==i,]$Area,state.name)]
  stn[cqst[cqst$year==i,]$Area=="District of Columbia"] <- "DC"
  d$pvi_state[d$year==i] <- spvi[match(d$state[d$year==i],stn)]
  d$pvi1_state[d$year==i] <- spvi1[match(d$state[d$year==i],stn)]
}

#'
#' ## Republican Incumbent
#'

for (i in seq(1972,2016,4)) {
  d$repinc[d$year==i] <- 0
  d$deminc[d$year==i] <- 0
  
  repstatus <- cqst[cqst$year==i,]$RepStatus[1] 
  repsamecand <- cqst[cqst$year==i,]$RepCandidate[1] == cqst[cqst$year==i-4,]$RepCandidate[1]
  demstatus <- cqst[cqst$year==i,]$DemStatus[1] 
  demsamecand <- cqst[cqst$year==i,]$DemCandidate[1] == cqst[cqst$year==i-4,]$DemCandidate[1]
  
  if(repstatus=="Incumbent" & repsamecand) d$repinc[d$year==i] <- 1
  if(demstatus=="Incumbent" & demsamecand) d$deminc[d$year==i] <- 1
  d$repinc2[d$year==i] <- d$repinc[d$year==i]
  d$deminc2[d$year==i] <- d$deminc[d$year==i]
  if(i %in% c(1976,1988,2008)) d$repinc2[d$year==i] <- 1
  if(i %in% c(2000,2016)) d$deminc2[d$year==i] <- 1
  
}
table(d$repinc, d$deminc, useNA="always")
table(d$repinc, d$year)
table(d$deminc, d$year)
table(d$repinc2, d$deminc2)

# Remove Used Data
rm(cqna,cqst)