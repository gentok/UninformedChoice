################################################################################# 
## File Name: CCES08_data1.R                                                   ##
## Date: 05 May 2019                                                           ##
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
library(rprojroot); library(questionr); library(psych); 

## Set Working Directory (Automatically) ##
if (rstudioapi::isAvailable()==TRUE) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path)); 
} 
projdir <- find_root(has_file("thisishome.txt"))
cat(paste("Working Directory Set to:\n",projdir))
setwd(projdir)

## Import Functions
source("codes_cces/CCES_analysis0_functions.R")

## Initial Data Location
xloc <- "data/cces08x.rds"
yloc <- "data/cces08y.rds"

###############
## Load Data ##
###############

x <- readRDS(xloc)
y <- readRDS(yloc)

#####################################
## Preparing Datasets for Analysis ##
#####################################

# Subset Data Frame
d <- data.frame(year = 2008,
                id = x$V100,
                idyear = x$V100*1000+2008,
                wt = x$V201,
                # wt_post, # no post weight
                state = as.character(x$V206),
                county = toupper(x$V267),
                district = as.numeric(x$V250))

# State Abbreviation
d$state_abb <- state.abb[match(d$state,state.name)]
d$state_abb[which(d$state=="District of Columbia")] <- "DC"
table(d$state_abb, useNA="always")

# County Name + State Abbreviation
d$county <- paste(d$county,d$state_abb)
table(d$county)[1:10]

# Congressional District Number
d$district[d$district==0] <- 1
d$district <- paste(d$state_abb, d$district)
table(d$district)

#########################
## Knowledge Questions ##
#########################

## Institutional Knowledge ##

# 1. Majority Party in Federal House
table(x$CC308a)
d$knhmaj <- 0
d$knhmaj[which(x$CC308a=="Democrats")] <- 1
d$knhmaj[which(x$CC308a=="Not Asked")] <- NA
table(d$knhmaj, useNA="always")

# 2. Majority Party in Federal Senate
table(x$CC308b)
d$knsmaj <- 0
d$knsmaj[which(x$CC308b=="Democrats")] <- 1
d$knsmaj[which(x$CC308b=="Not Asked")] <- NA
table(d$knsmaj, useNA="always")

# 3. Majority Party in State Senate
# State Senate Majority @ January 2008 
d$ssmaj = NA
d$ssmaj[which(d$state=="Alabama")] <- "Democrats"
d$ssmaj[which(d$state=="Alaska")] <- "Republicans"
d$ssmaj[which(d$state=="Arizona")] <- "Republicans"
d$ssmaj[which(d$state=="Arkansas")] <- "Democrats"
d$ssmaj[which(d$state=="California")] <- "Democrats"
d$ssmaj[which(d$state=="Colorado")] <- "Democrats"
d$ssmaj[which(d$state=="Connecticut")] <- "Democrats"
d$ssmaj[which(d$state=="Delaware")] <- "Democrats"
d$ssmaj[which(d$state=="District of Columbia")] <- NA
d$ssmaj[which(d$state=="Florida")] <- "Republicans"
d$ssmaj[which(d$state=="Georgia")] <- "Republicans"
d$ssmaj[which(d$state=="Hawaii")] <- "Democrats"
d$ssmaj[which(d$state=="Idaho")] <- "Republicans"
d$ssmaj[which(d$state=="Illinois")] <- "Democrats"
d$ssmaj[which(d$state=="Indiana")] <- "Republicans"
d$ssmaj[which(d$state=="Iowa")] <- "Democrats"
d$ssmaj[which(d$state=="Kansas")] <- "Republicans"
d$ssmaj[which(d$state=="Kentucky")] <- "Republicans"
d$ssmaj[which(d$state=="Louisiana")] <- "Democrats"
d$ssmaj[which(d$state=="Maine")] <- "Democrats"
d$ssmaj[which(d$state=="Maryland")] <- "Democrats"
d$ssmaj[which(d$state=="Massachusetts")] <- "Democrats"
d$ssmaj[which(d$state=="Michigan")] <- "Republicans"
d$ssmaj[which(d$state=="Minnesota")] <- "Democrats"
d$ssmaj[which(d$state=="Mississippi")] <- "Democrats"
d$ssmaj[which(d$state=="Missouri")] <- "Republicans"
d$ssmaj[which(d$state=="Montana")] <- "Democrats"
d$ssmaj[which(d$state=="Nebraska")] <- "Neither"
d$ssmaj[which(d$state=="Nevada")] <- "Republicans"
d$ssmaj[which(d$state=="New Hampshire")] <- "Democrats"
d$ssmaj[which(d$state=="New Jersey")] <- "Democrats"
d$ssmaj[which(d$state=="New Mexico")] <- "Democrats"
d$ssmaj[which(d$state=="New York")] <- "Republicans"
d$ssmaj[which(d$state=="North Carolina")] <- "Democrats"
d$ssmaj[which(d$state=="North Dakota")] <- "Republicans"
d$ssmaj[which(d$state=="Ohio")] <- "Republicans"
d$ssmaj[which(d$state=="Oklahoma")] <- "Neither"
d$ssmaj[which(d$state=="Oregon")] <- "Democrats"
d$ssmaj[which(d$state=="Pennsylvania")] <- "Republicans"
d$ssmaj[which(d$state=="Rhode Island")] <- "Democrats"
d$ssmaj[which(d$state=="South Carolina")] <- "Republicans"
d$ssmaj[which(d$state=="South Dakota")] <- "Republicans"
d$ssmaj[which(d$state=="Tennessee")] <- "Neither"
d$ssmaj[which(d$state=="Texas")] <- "Republicans"
d$ssmaj[which(d$state=="Utah")] <- "Republicans"
d$ssmaj[which(d$state=="Vermont")] <- "Democrats"
d$ssmaj[which(d$state=="Virginia")] <- "Democrats"
d$ssmaj[which(d$state=="Washington")] <- "Democrats"
d$ssmaj[which(d$state=="West Virginia")] <- "Democrats"
d$ssmaj[which(d$state=="Wisconsin")] <- "Democrats"
d$ssmaj[which(d$state=="Wyoming")] <- "Republicans"
table(d$ssmaj,useNA="always")
# Knowledge
table(x$CC308c)
d$knssmaj <- 0
d$knssmaj[which(x$CC308c==d$ssmaj)] <- 1
d$knssmaj[which(x$CC308c=="Not Asked")] <- NA
table(d$knssmaj, useNA="always")

# 4. Majority Party in State House
# State House Majority @ January 2008 
d$shmaj = NA
d$shmaj[which(d$state=="Alabama")] <- "Democrats"
d$shmaj[which(d$state=="Alaska")] <- "Republicans"
d$shmaj[which(d$state=="Arizona")] <- "Republicans"
d$shmaj[which(d$state=="Arkansas")] <- "Democrats"
d$shmaj[which(d$state=="California")] <- "Democrats"
d$shmaj[which(d$state=="Colorado")] <- "Democrats"
d$shmaj[which(d$state=="Connecticut")] <- "Democrats"
d$shmaj[which(d$state=="Delaware")] <- "Republicans"
d$shmaj[which(d$state=="District of Columbia")] <- NA
d$shmaj[which(d$state=="Florida")] <- "Republicans"
d$shmaj[which(d$state=="Georgia")] <- "Republicans"
d$shmaj[which(d$state=="Hawaii")] <- "Democrats"
d$shmaj[which(d$state=="Idaho")] <- "Republicans"
d$shmaj[which(d$state=="Illinois")] <- "Democrats"
d$shmaj[which(d$state=="Indiana")] <- "Democrats"
d$shmaj[which(d$state=="Iowa")] <- "Democrats"
d$shmaj[which(d$state=="Kansas")] <- "Republicans"
d$shmaj[which(d$state=="Kentucky")] <- "Democrats"
d$shmaj[which(d$state=="Louisiana")] <- "Democrats"
d$shmaj[which(d$state=="Maine")] <- "Democrats"
d$shmaj[which(d$state=="Maryland")] <- "Democrats"
d$shmaj[which(d$state=="Massachusetts")] <- "Democrats"
d$shmaj[which(d$state=="Michigan")] <- "Democrats"
d$shmaj[which(d$state=="Minnesota")] <- "Democrats"
d$shmaj[which(d$state=="Mississippi")] <- "Democrats"
d$shmaj[which(d$state=="Missouri")] <- "Republicans"
d$shmaj[which(d$state=="Montana")] <- "Republicans"
d$shmaj[which(d$state=="Nebraska")] <- "Neither"
d$shmaj[which(d$state=="Nevada")] <- "Democrats"
d$shmaj[which(d$state=="New Hampshire")] <- "Democrats"
d$shmaj[which(d$state=="New Jersey")] <- "Democrats"
d$shmaj[which(d$state=="New Mexico")] <- "Democrats"
d$shmaj[which(d$state=="New York")] <- "Democrats"
d$shmaj[which(d$state=="North Carolina")] <- "Democrats"
d$shmaj[which(d$state=="North Dakota")] <- "Republicans"
d$shmaj[which(d$state=="Ohio")] <- "Republicans"
d$shmaj[which(d$state=="Oklahoma")] <- "Republicans"
d$shmaj[which(d$state=="Oregon")] <- "Democrats"
d$shmaj[which(d$state=="Pennsylvania")] <- "Democrats"
d$shmaj[which(d$state=="Rhode Island")] <- "Democrats"
d$shmaj[which(d$state=="South Carolina")] <- "Republicans"
d$shmaj[which(d$state=="South Dakota")] <- "Republicans"
d$shmaj[which(d$state=="Tennessee")] <- "Democrats"
d$shmaj[which(d$state=="Texas")] <- "Republicans"
d$shmaj[which(d$state=="Utah")] <- "Republicans"
d$shmaj[which(d$state=="Vermont")] <- "Democrats"
d$shmaj[which(d$state=="Virginia")] <- "Republicans"
d$shmaj[which(d$state=="Washington")] <- "Democrats"
d$shmaj[which(d$state=="West Virginia")] <- "Democrats"
d$shmaj[which(d$state=="Wisconsin")] <- "Republicans"
d$shmaj[which(d$state=="Wyoming")] <- "Republicans"
table(d$shmaj,useNA="always")
# Knowledge
table(x$CC308d)
d$knshmaj <- 0
d$knshmaj[which(x$CC308d==d$shmaj)] <- 1
d$knshmaj[which(x$CC308d=="Not Asked")] <- NA
table(d$knshmaj, useNA="always")

# 5. Institutional Knowledge Index
a <- psych::alpha(d[,c("knsmaj","knhmaj","knssmaj","knshmaj")])
d$kninst <- a$scores; summary(a)
summary(d$kninst)

## Local Incumbent Knowledge ##

# 1. Know Party of House Incumbent
table(x$CC309d); table(x$V535)
d$knhpty <- 0
d$knhpty[which(x$CC309d=="Republican" & x$V535=="Republican")] <- 1
d$knhpty[which(x$CC309d=="Democratic" & x$V535 %in% c("Democratic",
                                                      "Democratic-Farmer-Labor"))] <- 1
d$knhpty[which(x$CC309d=="Not Asked")] <- NA
table(x$V535,d$knhpty,useNA="always")

# 2a. Know Party of Senate Incumbent 1
table(x$CC309b); table(x$V544)
d$kns1pty <- 0
d$kns1pty[which(x$CC309b=="Republican" & x$V544=="Republican")] <- 1
d$kns1pty[which(x$CC309b=="Democratic" & x$V544 %in% c("Democratic","Democratic-Farmer-Labor"))] <- 1
#d$kns1pty[which(x$CC309b=="Other Party/Independent" & x$V544=="Independent")] <- 1
d$kns1pty[which(x$CC309b=="Not Asked")] <- NA
table(x$V544,d$kns1pty,useNA="always")

# 2b. Know Party of Senate Incumbent 2
table(x$CC309c); table(x$V548)
d$kns2pty <- 0
d$kns2pty[which(x$CC309c=="Republican" & x$V548=="Republican")] <- 1
d$kns2pty[which(x$CC309c=="Democratic" & x$V548 %in% c("Democratic","Democratic-Farmer-Labor"))] <- 1
d$kns2pty[which(x$CC309c=="Other Party/Independent" & x$V548=="Independent")] <- 1
d$kns2pty[which(x$CC309c=="Not Asked")] <- NA
table(x$V548,d$kns2pty,useNA="always")

# 3. Know Party of Governor Incumbent
table(x$CC309a); table(x$V513)
d$kngpty <- 0
d$kngpty[which(x$CC309a=="Republican" & x$V513=="Republican")] <- 1
d$kngpty[which(x$CC309a=="Democratic" & x$V513=="Democratic")] <- 1
d$kngpty[which(x$CC309a=="Other Party / Independent" & x$V513=="Independent")] <- 1
d$kngpty[which(x$CC309a=="Not Asked")] <- NA
table(x$V513,d$kngpty,useNA="always")

# 4. Knowledge of Incumbents
a <- psych::alpha(d[,c("knhpty","kns1pty","kns2pty","kngpty")])
d$knpty <- a$scores; summary(a)
summary(d$knpty)

## Factual Knowledge Index 
a <- psych::alpha(d[,c("knsmaj","knhmaj","knssmaj","knshmaj",
                       "knhpty","kns1pty","kns2pty","kngpty")])
d$knidx <- a$scores; summary(a)
summary(d$knidx)

## Ideological Distance Measures ##

# 0. Self Ideology
table(y$CC317a); hist(y$CC317a) # Yourself 
d$ide_self <- as.numeric(y$CC317a-50)/16.67 
d$ide_self[is.na(y$CC317a)] <- 0
d$ide_self[which(y$CC317a %in% c("Skipped","Not Asked"))] <- NA
table(d$ide_self); hist(d$ide_self)

# 1. Ideological Distance Difference with Presidential Candidates
table(y$CC317h); hist(y$CC317h) # Obama
table(y$CC317g); hist(y$CC317g) # McCain
# Republican Position
d$ide_prep <- (y$CC317g-50)/16.67
d$ide_prep[is.na(y$CC317g)] <- 0
table(d$ide_prep)
# Democrat Position
d$ide_pdem <- (y$CC317h-50)/16.67
d$ide_pdem[is.na(y$CC317h)] <- 0
table(d$ide_pdem)
# Positional Distance Difference 
d$idedist_prep <- abs(d$ide_prep - d$ide_self)
d$idedist_pdem <- abs(d$ide_pdem - d$ide_self)
table(d$idedist_prep, useNA="always"); hist(d$idedist_prep)
table(d$idedist_pdem, useNA="always"); hist(d$idedist_pdem)
d$idedist_prepadv <- -(d$idedist_prep - d$idedist_pdem)
# d$idedist_prepadv[which(y$CC317a=="Not sure")] <- 0
# d$idedist_prepadv[which(y$CC317h=="Not sure")] <- 0
# d$idedist_prepadv[which(y$CC317g=="Not sure")] <- 0
d$idedist_padvm <- abs(d$idedist_prepadv) # Advantage Magnitude
d$idedist_padvr <- (d$idedist_prepadv>0)*1 # Republican Has Advantage
d$idedist_padvmr <- d$idedist_padvm*d$idedist_padvr # Advantage Magnitude (Republican)
d$idedist_padvmd <- d$idedist_padvm*(1-d$idedist_padvr) # Advantage Magnitude (Republican)
table(d$idedist_prepadv, useNA="always"); hist(d$idedist_prepadv)
table(d$idedist_padvm, useNA="always"); hist(d$idedist_padvm)
table(d$idedist_padvr, useNA="always")
# Squared Distance
d$idedistsq_prep <- (d$ide_prep - d$ide_self)^2
d$idedistsq_pdem <- (d$ide_pdem - d$ide_self)^2
table(d$idedistsq_prep, useNA="always"); hist(d$idedistsq_prep)
table(d$idedistsq_pdem, useNA="always"); hist(d$idedistsq_pdem)
d$idedistsq_prepadv <- -(d$idedistsq_prep - d$idedistsq_pdem)
table(d$idedistsq_prepadv, useNA="always"); hist(d$idedistsq_prepadv)

# 2. Ideological Distance Difference with Senate Candidates
table(y$CC317i) # Senate Candidate 1 (Democrat)
table(y$CC317j) # Senate Candidate 2 (Mostly Republican)
# Republican Position
d$ide_srep <- (y$CC317j-50)/16.66
d$ide_srep[is.na(y$CC317j)] <- 0
d$ide_srep[is.na(x$CC317H_SC1)] <- NA
d$ide_srep[is.na(x$CC317H_SC2)] <- NA
table(d$ide_srep); hist(d$ide_srep)
# Democrat Position
d$ide_sdem <- (y$CC317i-50)/16.66
d$ide_sdem[is.na(y$CC317i)] <- 0
d$ide_sdem[is.na(x$CC317H_SC1)] <- NA
d$ide_sdem[is.na(x$CC317H_SC2)] <- NA
table(d$ide_sdem); hist(d$ide_sdem)
# Positional Distance Difference 
d$idedist_srep <- abs(d$ide_srep - d$ide_self)
d$idedist_sdem <- abs(d$ide_sdem - d$ide_self)
table(d$idedist_srep); table(d$idedist_sdem)
d$idedist_srepadv <- -(d$idedist_srep - d$idedist_sdem)
# d$idedist_srepadv[which(y$CC317a=="Not sure")] <- 0
# d$idedist_srepadv[which(y$CC317i=="Not sure")] <- 0
# d$idedist_srepadv[which(y$CC317j=="Not sure")] <- 0
d$idedist_sadvm <- abs(d$idedist_srepadv) # Advantage Magnitude
d$idedist_sadvr <- (d$idedist_srepadv>0)*1 # Republican Has Advantage
d$idedist_sadvmr <- d$idedist_sadvm*d$idedist_sadvr # Advantage Magnitude (Republican)
d$idedist_sadvmd <- d$idedist_sadvm*(1-d$idedist_sadvr) # Advantage Magnitude (Republican)
table(d$idedist_srepadv); hist(d$idedist_srepadv)
table(d$idedist_sadvm); hist(d$idedist_sadvm)
table(d$idedist_sadvr)
# Squared Distance
d$idedistsq_srep <- (d$ide_srep - d$ide_self)^2
d$idedistsq_sdem <- (d$ide_sdem - d$ide_self)^2
table(d$idedistsq_srep, useNA="always"); hist(d$idedistsq_srep)
table(d$idedistsq_sdem, useNA="always"); hist(d$idedistsq_sdem)
d$idedistsq_srepadv <- -(d$idedistsq_srep - d$idedistsq_sdem)
table(d$idedistsq_srepadv, useNA="always"); hist(d$idedistsq_srepadv)

# 3. Ideological Distance Difference with House Candidates
table(y$CC317k) # House Candidate 1 (Democrat)
table(y$CC317l) # House Candidate 2 (Mostly Republican)
# Republican Position
d$ide_hrep <- (y$CC317l-50)/16.66
d$ide_hrep[is.na(y$CC317l)] <- 0
d$ide_hrep[is.na(y$CC317H_HC1)] <- NA
d$ide_hrep[is.na(y$CC317H_HC2)] <- NA
table(d$ide_hrep); hist(d$ide_hrep)
# Democrat Position
d$ide_hdem <- (y$CC317k-50)/16.66
d$ide_hdem[is.na(y$CC317k)] <- 0
d$ide_hdem[is.na(y$CC317H_HC1)] <- NA
d$ide_hdem[is.na(y$CC317H_HC2)] <- NA
table(d$ide_hdem); hist(d$ide_hdem)
# Positional Distance Difference 
d$idedist_hrep <- abs(d$ide_hrep - d$ide_self)
d$idedist_hdem <- abs(d$ide_hdem - d$ide_self)
table(d$idedist_hrep); table(d$idedist_hdem)
d$idedist_hrepadv <- -(d$idedist_hrep - d$idedist_hdem)
# d$idedist_hrepadv[which(y$CC317a=="Not sure")] <- 0
# d$idedist_hrepadv[which(y$CC317k=="Not sure")] <- 0
# d$idedist_hrepadv[which(y$CC317l=="Not sure")] <- 0
d$idedist_hadvm <- abs(d$idedist_hrepadv) # Advantage Magnitude
d$idedist_hadvr <- (d$idedist_hrepadv>0)*1 # Republican Has Advantage
d$idedist_hadvmr <- d$idedist_hadvm*d$idedist_hadvr # Advantage Magnitude (Republican)
d$idedist_hadvmd <- d$idedist_hadvm*(1-d$idedist_hadvr) # Advantage Magnitude (Republican)
table(d$idedist_hrepadv); hist(d$idedist_hrepadv)
table(d$idedist_hadvm); hist(d$idedist_hadvm)
table(d$idedist_hadvr)
# Squared Distance
d$idedistsq_hrep <- (d$ide_hrep - d$ide_self)^2
d$idedistsq_hdem <- (d$ide_hdem - d$ide_self)^2
table(d$idedistsq_hrep, useNA="always"); hist(d$idedistsq_hrep)
table(d$idedistsq_hdem, useNA="always"); hist(d$idedistsq_hdem)
d$idedistsq_hrepadv <- -(d$idedistsq_hrep - d$idedistsq_hdem)
table(d$idedistsq_hrepadv, useNA="always"); hist(d$idedistsq_hrepadv)

################
## DV: Voting ##
################

# 0. Voted in 2008 Election
table(x$CC403)
d$voted <- (x$CC403=="I definitely voted in the November General Election")*1
table(d$voted)

# 1a. Presidential Voting Intention (Pre-Election)
table(x$CC326)
table(x$CC326b)
table(x$CC327)
d$vpint <- NA
d$vpint[which(x$CC326=="No")] <- "Abstain"
d$vpint[which(x$CC326b=="I didn't vote in this election")] <- "Abstain"
d$vpint[which(x$CC326b=="John McCain (Republican)")] <- "Republican"
d$vpint[which(x$CC326b=="Barack Obama (Democratic)")] <- "Democrat"
d$vpint[which(x$CC326b %in% c("Ron Paul","Ralph Nader",
                              "Bob Barr (Libertarian)",
                              "Cynthia McKinney (Green)",
                              "Other"))] <- "Other"
d$vpint[which(x$CC326b %in% c("I'm not sure","Skipped"))] <- NA
d$vpint[which(x$CC327=="I won't vote in this election")] <- "Abstain"
d$vpint[which(x$CC327=="John McCain (Republican)")] <- "Republican"
d$vpint[which(x$CC327=="Barack Obama (Democratic)")] <- "Democrat"
d$vpint[which(x$CC327 %in% c("Ron Paul","Ralph Nader",
                             "Bob Barr (Libertarian)",
                             "Cynthia McKinney (Green)",
                             "Other"))] <- "Other"
d$vpint[which(x$CC327 %in% c("I'm not sure","Skipped"))] <- "Undecided"
d$vpint <- factor(d$vpint,levels=c("Abstain","Republican","Democrat","Other","Undecided"))
table(d$vpint, useNA="always")
d$vpintx <- as.numeric(d$vpint)-1
table(d$vpintx,d$vpint, useNA="always")

# 1b. Presidential Voting Result (Post-Election)
table(x$CC403)
table(x$CC410)
d$vpres <- NA
d$vpres[which(x$CC410=="I did not vote in this race")] <- "Abstain"
d$vpres[which(x$CC410=="John McCain (Republican)")] <- "Republican"
d$vpres[which(x$CC410=="Barack Obama (Democrat)")] <- "Democrat"
d$vpres[which(x$CC410 %in% c("Ron Paul","Ralph Nader (Independent)",
                             "Robert Barr (Libertarian)",
                             "Cynthia McKinney (Green Party)",
                             "Other candidate or party:"))] <- "Other"
d$vpres[which(x$CC410 %in% c("I'm not sure","Skipped","Not Asked"))] <- NA
d$vpres[which(x$CC403 %in% c("I did not vote in the election this November",
                                "I thought about voting this time - but didn't",
                                "I usually vote, but didn't this time",
                                "I attempted to vote but did not or could not"))] <- "Abstain"
d$vpres <- factor(d$vpres,levels=c("Abstain","Republican","Democrat","Other"))
table(d$vpres, useNA="always")
d$vpresx <- as.numeric(d$vpres)-1
table(d$vpresx, useNA="always")

# 2a. Senate Voting Intention
table(x$CC326)
table(x$CC335)
table(x$CC337)
table(x$V565)
d$vsint <- NA
d$vsint[which(x$CC326=="No")] <- "Abstain" 
d$vsint[is.na(x$SenCand1Party)] <- NA # No Election
d$vsint[which(x$CC335=="Democratic candidate")] <- "Democrat"
d$vsint[which(x$CC335=="Republican candidate")] <- "Republican"
d$vsint[which(x$CC335=="Other party candidate")] <- "Other" 
d$vsint[which(x$CC335=="Other")] <- "Other"
d$vsint[which(x$CC335 %in% c("Haven't decided","Skipped"))] <- "Undecided"
d$vsint[which(x$CC335 %in% c("I won't vote in this election"))] <- "Abstain"
d$vsint[which(x$CC337=="Democratic candidate")] <- "Democrat"
d$vsint[which(x$CC337=="Republican candidate")] <- "Republican"
d$vsint[which(x$CC337=="Other party candidate")] <- "Other"
d$vsint[which(x$CC337=="Other")] <- "Other"
d$vsint[which(x$CC337 %in% c("I'm not sure","Skipped"))] <- NA
d$vsint[which(x$CC337 %in% c("I didn't vote in this election"))] <- "Abstain"
d$vsint[which(x$V565=="No election")] <- NA
d$vsint <- factor(d$vsint,levels=c("Abstain","Republican","Democrat","Other","Undecided"))
table(d$vsint, useNA="always")
d$vsintx <- as.numeric(d$vsint)-1
table(d$vsintx, useNA="always")

# 2b. Senate Voting Result 
table(x$CC403)
table(x$CC411)
table(x$V565)
d$vsres <- NA
d$vsres[which(x$CC411=="Democratic candidate")] <- "Democrat"
d$vsres[which(x$CC411=="Republican candidate")] <- "Republican" 
d$vsres[which(x$CC411=="Other party candidate")] <- "Other"
d$vsres[which(x$CC411 %in% c("Not sure","Skipped"))] <- NA
d$vsres[which(x$CC411 %in% c("I did not vote in this race"))] <- "Abstain"
d$vsres[which(x$CC403 %in% c("I did not vote in the election this November",
                                "I thought about voting this time - but didn't",
                                "I usually vote, but didn't this time",
                                "I attempted to vote but did not or could not"))] <- "Abstain"
d$vsres[which(x$V565=="No election")] <- NA
d$vsres <- factor(d$vsres,levels=c("Abstain","Republican","Democrat","Other"))
table(d$vsres, useNA="always")
d$vsresx <- as.numeric(d$vsres)-1
table(d$vsresx, useNA="always")

# 3a. House Voting Intention
table(x$CC326)
table(x$CC339, useNA="always")
table(x$CC340)
d$vhint <- NA
d$vhint[which(x$CC326=="No")] <- "Abstain" 
d$vhint[which(x$CC339=="Democratic candidate")] <- "Democrat"
d$vhint[which(x$CC339=="Republican candidate")] <- "Republican"
d$vhint[which(x$CC339=="Other party candidate")] <- "Other"
d$vhint[which(x$CC339=="Other party candidate 2")] <- "Other"
d$vhint[which(x$CC339=="Other")] <- "Other" 
d$vhint[which(x$CC339 %in% c("Haven't decided","Skipped"))] <- "Undecided"
d$vhint[which(x$CC339 %in% c("I won't vote in this election"))] <- "Abstain"
d$vhint[which(x$CC340=="Democratic candidate")] <- "Democrat"
d$vhint[which(x$CC340=="Republican candidate")] <- "Republican"
d$vhint[which(x$CC340=="Other party candidate")] <- "Other"
d$vhint[which(x$CC340=="Other party candidate 2")] <- "Other"
d$vhint[which(x$CC340=="Other")] <- "Other"
d$vhint[which(x$CC340 %in% c("I'm not sure","Skipped"))] <- NA
d$vhint[which(x$CC340 %in% c("I didn't vote in this election"))] <- "Abstain"
d$vhint <- factor(d$vhint,levels=c("Abstain","Republican","Democrat","Other","Undecided"))
table(d$vhint, useNA="always")
d$vhintx <- as.numeric(d$vhint)-1
table(d$vhintx, useNA="always")

# 3b. House Voting Result 
table(x$CC403)
table(x$CC412)
d$vhres <- NA
d$vhres[which(x$CC412=="Democratic candidate")] <- "Democrat"
d$vhres[which(x$CC412=="Republican candidate")] <- "Republican"
d$vhres[which(x$CC412=="Other party candidate")] <- "Other"
d$vhres[which(x$CC412 %in% c("Not sure","Skipped"))] <- NA
d$vhres[which(x$CC412 %in% c("I did not vote in this race"))] <- "Abstain"
d$vhres[which(x$CC403 %in% c("I did not vote in the election this November",
                                "I thought about voting this time - but didn't",
                                "I usually vote, but didn't this time",
                                "I attempted to vote but did not or could not"))] <- "Abstain"
d$vhres <- factor(d$vhres,levels=c("Abstain","Republican","Democrat","Other"))
table(d$vhres, useNA="always")
d$vhresx <- as.numeric(d$vhres)-1
table(d$vhresx, useNA="always")

# 4a. Governor Voting Intention
table(x$CC326)
table(x$CC336, useNA="always")
table(x$CC338)
table(x$V506)
d$vgint <- NA
d$vgint[which(x$CC326=="No")] <- "Abstain" 
d$vgint[which(x$CC336=="Democratic candidate")] <- "Democrat"
d$vgint[which(x$CC336=="Republican candidate")] <- "Republican"
d$vgint[which(x$CC336=="Other party candidate")] <- "Other"
d$vgint[which(x$CC336=="Other")] <- "Other"
d$vgint[which(x$CC336 %in% c("Haven't decided","Skipped"))] <- "Undecided"
d$vgint[which(x$CC336 %in% c("I won't vote in this election"))] <- "Abstain"
d$vgint[which(x$CC338=="Democratic candidate")] <- "Democrat"
d$vgint[which(x$CC338=="Republican candidate")] <- "Republican"
d$vgint[which(x$CC338=="Other party candidate")] <- "Other"
d$vgint[which(x$CC338=="Other")] <- "Other"
d$vgint[which(x$CC338 %in% c("I'm not sure","Skipped"))] <- NA
d$vgint[which(x$CC338 %in% c("I didn't vote in this election"))] <- "Abstain"
d$vgint[which(x$V506=="No election")] <- NA
d$vgint <- factor(d$vgint,levels=c("Abstain","Republican","Democrat","Other","Undecided"))
table(d$vgint, useNA="always")
d$vgintx <- as.numeric(d$vgint)-1
table(d$vgintx, useNA="always")

# 4b. Governor Voting Result 
table(x$CC403)
table(x$CC413)
table(x$V506)
d$vgres <- NA
d$vgres[which(x$CC413=="Democratic candidate")] <- "Democrat"
d$vgres[which(x$CC413=="Republican candidate")] <- "Republican"
d$vgres[which(x$CC413=="Other party candidate")] <- "Other"
d$vgres[which(x$CC413 %in% c("Not sure","Skipped"))] <- NA
d$vgres[which(x$CC413 %in% c("I did not vote in this race"))] <- "Abstain"
d$vgres[which(x$CC403 %in% c("I did not vote in the election this November",
                                "I thought about voting this time - but didn't",
                                "I usually vote, but didn't this time",
                                "I attempted to vote but did not or could not"))] <- "Abstain"
d$vgres[which(x$V506=="No election")] <- NA
d$vgres <- factor(d$vgres,levels=c("Abstain","Republican","Democrat","Other"))
table(d$vgres, useNA="always")
d$vgres[-which(paste(x$GovCand1Party_post,x$GovCand2Party_post) %in% 
                    c("Democratic Republican"))] <- NA
d$vgresx <- as.numeric(d$vgres)-1
table(d$vgresx, useNA="always")

# 5. Voting Result for State Senate Election
table(x$CC414_2, useNA="always")
d$vssres <- NA
d$vssres[which(x$CC414_2=="Democrat")] <- "Democrat"
d$vssres[which(x$CC414_2=="Republican")] <- "Republican"
d$vssres[which(x$CC414_2=="Someone Else")] <- "Other"
d$vssres[which(x$CC414_2=="I did not vote in this race")] <- "Abstain"
d$vssres[which(x$CC414_2 %in% c("Skipped","Not Asked"))] <- NA
d$vssres <- factor(d$vssres,levels=c("Abstain","Republican","Democrat","Other"))
table(d$vssres, useNA="always")
d$vssresx <- as.numeric(d$vssres)-1
table(d$vssresx, useNA="always")

# 6. Voting Result for State House (Lower Chamber) Election
table(x$CC414_1, useNA="always")
d$vshres <- NA
d$vshres[which(x$CC414_1=="Democrat")] <- "Democrat"
d$vshres[which(x$CC414_1=="Republican")] <- "Republican"
d$vshres[which(x$CC414_1=="Someone Else")] <- "Other"
d$vshres[which(x$CC414_1=="I did not vote in this race")] <- "Abstain"
d$vshres[which(x$CC414_1 %in% c("Skipped","Not Asked"))] <- NA
d$vshres <- factor(d$vshres,levels=c("Abstain","Republican","Democrat","Other"))
table(d$vshres, useNA="always")
d$vshresx <- as.numeric(d$vshres)-1
table(d$vshresx, useNA="always")

##############
## Controls ##
##############

## Demographics ##

# 1. Gender (female)
table(x$V208)
d$female <- as.numeric(x$V208)-1
table(d$female)

# 2. Age
table(y$V207)
d$age <- 2008-y$V207
table(d$age)

# 3. Race
table(x$V211)
d$white <- (x$V211=="White")*1
#d$white[which(x$V211=="Mixed" & x$multrace_1=="Yes")] <- 1
d$black <- (x$V211=="Black")*1
#d$black[which(x$V211=="Mixed" & x$multrace_2=="Yes")] <- 1
d$latino <- (x$V211=="Hispanic")*1
#d$latino[which(x$V211=="Mixed" & x$multrace_3=="Yes")] <- 1
d$asian <- (x$V211=="Asian")*1
#d$asian <- (x$V211=="Asian" & x$multrace_4=="Yes")*1
d$other <- (x$V211 %in% c("Native American","Other","Middle Eastern","Mixed"))*1
# d$other[which(x$V211=="Mixed" & (x$multrace_5=="Yes" |
#                                  x$multrace_8=="Yes" |
#                                  x$multrace_97=="Yes"))] <- 1
table(d$white,x$V211)
table(d$black,x$V211)
table(d$latino,x$V211)
table(d$asian,x$V211)
table(d$other,x$V211)
# Summary Variable
d$race <- NA
d$race[d$white==1] <- "White"
d$race[d$black==1] <- "Black"
d$race[d$latino==1] <- "Hispanic"
d$race[d$asian==1] <- "Asian"
d$race[d$other==1] <- "Other"
d$race <- factor(d$race, levels=c("White","Black","Hispanic",
                                  "Asian","Other"))


# 4. Marital Status
table(x$V214)
d$married <- NA
d$married[which(x$V214 %in% c("Married","Separated",
                                 "Domestic partnership"))] <- 1
d$married[which(x$V214 %in% c("Single","Divorced","Widowed"))] <- 0
table(d$married)

# 4. Full-Time Working Status
table(x$V209)
d$fulltime <- (x$V209=="Full-time")*1

# 5. Family Income
table(x$V246)
d$income <- as.numeric(x$V246)
d$income[which(x$V246 %in% c("Prefer not to say","Skipped"))] <- NA
table(d$income)

## Societal ##

# 1. Education
table(x$V213)
d$edu <- as.numeric(x$V213)-1 # 6 cats
table(d$edu)

# 2a. Church Attendance 
table(x$V217)
d$chatd <- 6-as.numeric(x$V217)
d$chatd[which(x$V217=="Don't know")] <- 0
d$chatd[which(x$V217=="Skipped")] <- NA
table(d$chatd)

# 2b. Born Again Christian
table(x$V215)
d$bornagain <- 2-as.numeric(x$V215)
d$bornagain[which(x$V215=="Don't know")] <- 0
d$bornagain[which(x$V215=="Skipped")] <- NA
table(d$bornagain)

# 3. Residence Length
# table(x$CC08_361)
# d$resid <- as.numeric(x$CC08_361)
# d$resid[which(x$CC08_361 %in% c("Skipped","Not Asked"))] <- NA

## Attitudinal ##

# 1. Public Affair Interest (Political Interest)
table(x$V244)
d$polint <- 4 - as.numeric(x$V244)
d$polint[which(x$V244=="Don't know")] <- 0
d$polint[which(x$V244=="Skipped")] <- NA
table(d$polint)

# 2. Party ID Strength
table(x$CC307a)
d$pidstr <- abs(as.numeric(x$CC307a) - 4)
table(d$pidstr)
d$pidstr[which(x$CC307a=="Not sure")] <- 0
d$pidstr[which(x$CC307a %in% c("Skipped","Not Asked"))] <- NA
table(d$pidstr)
d$repstr <- d$pidstr
d$repstr[which(as.numeric(x$CC307a) %in% c(1,2,3))] <- 0
table(d$repstr)
d$demstr <- d$pidstr
d$demstr[which(as.numeric(x$CC307a) %in% c(5,6,7))] <- 0
table(d$demstr)

# 2.5 Party ID
d$pid <- d$repstr - d$demstr

# 3. National Economy Evaluation
table(x$CC302)
d$evecon <- 3 - as.numeric(x$CC302)
d$evecon[which(x$CC302 %in% c("Not sure"))] <- 0
d$evecon[which(x$CC302 %in% c("Skipped","Not Asked"))] <- NA
table(d$evecon)

# 4. Income Increase for the Past Four Years
# table(x$CC08_303)
# d$retinc <- 3 - as.numeric(x$CC08_303)
# d$retinc[which(x$CC08_303 %in% c("Skipped","Not Asked"))] <- NA
# table(d$retinc)

# attr(x, "var.labels")[which(names(x)=="newsint")]

###############
## Save Data ##
###############

saveRDS(d,file="data/cces08s.rds")
