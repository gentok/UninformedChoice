################################################################################# 
## File Name: CCES16_data1.R                                                   ##
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
xloc <- "data/cces16x.rds"
yloc <- "data/cces16y.rds"

###############
## Load Data ##
###############

x <- readRDS(xloc)
y <- readRDS(yloc)

#####################################
## Preparing Datasets for Analysis ##
#####################################

# Subset Data Frame
d <- data.frame(year = 2016,
                id = x$V101,
                idyear = x$V101*1000+2016,
                wt = x$commonweight,
                wt_post = x$commonweight_post,
                state = x$inputstate,
                county = toupper(x$countyname),
                district = as.numeric(x$cdid113),
                district115 = as.numeric(x$cdid115))

# State Abbreviation
d$state_abb <- state.abb[match(d$state,state.name)]
d$state_abb[which(d$state=="District of Columbia")] <- "DC"
table(d$state_abb, useNA="always")

# Congressional District Number
d$district <- paste(d$state_abb, d$district)
table(d$district)[1:10]
d$district115 <- paste(d$state_abb, d$district115)
table(d$district115)[1:10]

#########################
## Knowledge Questions ##
#########################

## Institutional Knowledge ##

# 1. Majority Party in Federal House
table(x$CC16_321a)
d$knhmaj <- 0
d$knhmaj[which(x$CC16_321a=="Republicans")] <- 1
d$knhmaj[which(x$CC16_321a=="Not Asked")] <- NA
table(d$knhmaj, useNA="always")

# 2. Majority Party in Federal Senate
table(x$CC16_321b)
d$knsmaj <- 0
d$knsmaj[which(x$CC16_321b=="Republicans")] <- 1
d$knsmaj[which(x$CC16_321b=="Not Asked")] <- NA
table(d$knsmaj, useNA="always")

# 3. Majority Party in State Senate
# State Senate Majority @ January 2016 
d$ssmaj = NA
d$ssmaj[which(d$state=="Alabama")] <- "Republicans"
d$ssmaj[which(d$state=="Alaska")] <- "Republicans"
d$ssmaj[which(d$state=="Arizona")] <- "Republicans"
d$ssmaj[which(d$state=="Arkansas")] <- "Republicans"
d$ssmaj[which(d$state=="California")] <- "Democrats"
d$ssmaj[which(d$state=="Colorado")] <- "Republicans"
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
d$ssmaj[which(d$state=="Louisiana")] <- "Republicans"
d$ssmaj[which(d$state=="Maine")] <- "Republicans"
d$ssmaj[which(d$state=="Maryland")] <- "Democrats"
d$ssmaj[which(d$state=="Massachusetts")] <- "Democrats"
d$ssmaj[which(d$state=="Michigan")] <- "Republicans"
d$ssmaj[which(d$state=="Minnesota")] <- "Democrats"
d$ssmaj[which(d$state=="Mississippi")] <- "Republicans"
d$ssmaj[which(d$state=="Missouri")] <- "Republicans"
d$ssmaj[which(d$state=="Montana")] <- "Republicans"
d$ssmaj[which(d$state=="Nebraska")] <- NA
d$ssmaj[which(d$state=="Nevada")] <- "Republicans"
d$ssmaj[which(d$state=="New Hampshire")] <- "Republicans"
d$ssmaj[which(d$state=="New Jersey")] <- "Democrats"
d$ssmaj[which(d$state=="New Mexico")] <- "Democrats"
d$ssmaj[which(d$state=="New York")] <- "Republicans"
d$ssmaj[which(d$state=="North Carolina")] <- "Republicans"
d$ssmaj[which(d$state=="North Dakota")] <- "Republicans"
d$ssmaj[which(d$state=="Ohio")] <- "Republicans"
d$ssmaj[which(d$state=="Oklahoma")] <- "Republicans"
d$ssmaj[which(d$state=="Oregon")] <- "Democrats"
d$ssmaj[which(d$state=="Pennsylvania")] <- "Republicans"
d$ssmaj[which(d$state=="Rhode Island")] <- "Democrats"
d$ssmaj[which(d$state=="South Carolina")] <- "Democrats"
d$ssmaj[which(d$state=="South Dakota")] <- "Republicans"
d$ssmaj[which(d$state=="Tennessee")] <- "Republicans"
d$ssmaj[which(d$state=="Texas")] <- "Republicans"
d$ssmaj[which(d$state=="Utah")] <- "Republicans"
d$ssmaj[which(d$state=="Vermont")] <- "Democrats"
d$ssmaj[which(d$state=="Virginia")] <- "Republicans"
d$ssmaj[which(d$state=="Washington")] <- "Republicans"
d$ssmaj[which(d$state=="West Virginia")] <- "Republicans"
d$ssmaj[which(d$state=="Wisconsin")] <- "Republicans"
d$ssmaj[which(d$state=="Wyoming")] <- "Republicans"
table(d$ssmaj,useNA="always")
# Knowledge
table(x$CC16_321c)
d$knssmaj <- 0
d$knssmaj[which(x$CC16_321c==d$ssmaj)] <- 1
d$knssmaj[which(x$CC16_321c=="Not Asked")] <- NA
table(d$knssmaj, useNA="always")

# 4. Majority Party in State House
# State House Majority @ January 2016 
d$shmaj = NA
d$shmaj[which(d$state=="Alabama")] <- "Republicans"
d$shmaj[which(d$state=="Alaska")] <- "Republicans"
d$shmaj[which(d$state=="Arizona")] <- "Republicans"
d$shmaj[which(d$state=="Arkansas")] <- "Republicans"
d$shmaj[which(d$state=="California")] <- "Democrats"
d$shmaj[which(d$state=="Colorado")] <- "Democrats"
d$shmaj[which(d$state=="Connecticut")] <- "Democrats"
d$shmaj[which(d$state=="Delaware")] <- "Democrats"
d$shmaj[which(d$state=="District of Columbia")] <- NA
d$shmaj[which(d$state=="Florida")] <- "Republicans"
d$shmaj[which(d$state=="Georgia")] <- "Republicans"
d$shmaj[which(d$state=="Hawaii")] <- "Democrats"
d$shmaj[which(d$state=="Idaho")] <- "Republicans"
d$shmaj[which(d$state=="Illinois")] <- "Democrats"
d$shmaj[which(d$state=="Indiana")] <- "Republicans"
d$shmaj[which(d$state=="Iowa")] <- "Republicans"
d$shmaj[which(d$state=="Kansas")] <- "Republicans"
d$shmaj[which(d$state=="Kentucky")] <- "Democrats"
d$shmaj[which(d$state=="Louisiana")] <- "Republicans"
d$shmaj[which(d$state=="Maine")] <- "Democrats"
d$shmaj[which(d$state=="Maryland")] <- "Democrats"
d$shmaj[which(d$state=="Massachusetts")] <- "Democrats"
d$shmaj[which(d$state=="Michigan")] <- "Republicans"
d$shmaj[which(d$state=="Minnesota")] <- "Republicans"
d$shmaj[which(d$state=="Mississippi")] <- "Republicans"
d$shmaj[which(d$state=="Missouri")] <- "Republicans"
d$shmaj[which(d$state=="Montana")] <- "Republicans"
d$shmaj[which(d$state=="Nebraska")] <- "Neither"
d$shmaj[which(d$state=="Nevada")] <- "Republicans"
d$shmaj[which(d$state=="New Hampshire")] <- "Republicans"
d$shmaj[which(d$state=="New Jersey")] <- "Democrats"
d$shmaj[which(d$state=="New Mexico")] <- "Republicans"
d$shmaj[which(d$state=="New York")] <- "Democrats"
d$shmaj[which(d$state=="North Carolina")] <- "Republicans"
d$shmaj[which(d$state=="North Dakota")] <- "Republicans"
d$shmaj[which(d$state=="Ohio")] <- "Republicans"
d$shmaj[which(d$state=="Oklahoma")] <- "Republicans"
d$shmaj[which(d$state=="Oregon")] <- "Democrats"
d$shmaj[which(d$state=="Pennsylvania")] <- "Republicans"
d$shmaj[which(d$state=="Rhode Island")] <- "Democrats"
d$shmaj[which(d$state=="South Carolina")] <- "Democrats"
d$shmaj[which(d$state=="South Dakota")] <- "Republicans"
d$shmaj[which(d$state=="Tennessee")] <- "Republicans"
d$shmaj[which(d$state=="Texas")] <- "Republicans"
d$shmaj[which(d$state=="Utah")] <- "Republicans"
d$shmaj[which(d$state=="Vermont")] <- "Democrats"
d$shmaj[which(d$state=="Virginia")] <- "Republicans"
d$shmaj[which(d$state=="Washington")] <- "Democrats"
d$shmaj[which(d$state=="West Virginia")] <- "Republicans"
d$shmaj[which(d$state=="Wisconsin")] <- "Republicans"
d$shmaj[which(d$state=="Wyoming")] <- "Republicans"
table(d$shmaj,useNA="always")
# Knowledge
table(x$CC16_321d)
d$knshmaj <- 0
d$knshmaj[which(x$CC16_321d==d$shmaj)] <- 1
d$knshmaj[which(x$CC16_321d=="Not Asked")] <- NA
table(d$knshmaj, useNA="always")

# 5. Institutional Knowledge Index
a <- psych::alpha(d[,c("knsmaj","knhmaj","knssmaj","knshmaj")])
d$kninst <- a$scores; summary(a)
summary(d$kninst)

## Local Incumbent Knowledge ##

# 1. Know Party of House Incumbent
table(x$CC16_322d); table(x$CurrentHouseParty)
d$knhpty <- 0
d$knhpty[which(x$CC16_322d=="Republican" & x$CurrentHouseParty=="Republican")] <- 1
d$knhpty[which(x$CC16_322d=="Democrat" & x$CurrentHouseParty=="Democratic")] <- 1
d$knhpty[which(x$CC16_322d=="Not Asked")] <- NA
table(x$CurrentHouseParty,d$knhpty,useNA="always")

# 2a. Know Party of Senate Incumbent 1
table(x$CC16_322b); table(x$CurrentSen1Party)
d$kns1pty <- 0
d$kns1pty[which(x$CC16_322b=="Republican" & x$CurrentSen1Party=="Republican")] <- 1
d$kns1pty[which(x$CC16_322b=="Democrat" & x$CurrentSen1Party %in% c("Democratic","Democratic-Farmer-Labor"))] <- 1
d$kns1pty[which(x$CC16_322b=="Not Asked")] <- NA
table(x$CurrentSen1Party,d$kns1pty,useNA="always")

# 2a. Know Party of Senate Incumbent 1
table(x$CC16_322c); table(x$CurrentSen2Party)
d$kns2pty <- 0
d$kns2pty[which(x$CC16_322c=="Republican" & x$CurrentSen2Party=="Republican")] <- 1
d$kns2pty[which(x$CC16_322c=="Democrat" & x$CurrentSen2Party %in% c("Democratic","Democratic-Farmer-Labor"))] <- 1
d$kns2pty[which(x$CC16_322c=="Other Party / Independent" & x$CurrentSen2Party=="Independent")] <- 1
d$kns2pty[which(x$CC16_322c=="Not Asked")] <- NA
table(x$CurrentSen2Party,d$kns2pty,useNA="always")

# 3. Know Party of Governor Incumbent
table(x$CC16_322a); table(x$CurrentGovParty)
d$kngpty <- 0
d$kngpty[which(x$CC16_322a=="Republican" & x$CurrentGovParty=="Republican")] <- 1
d$kngpty[which(x$CC16_322a=="Democrat" & x$CurrentGovParty=="Democratic")] <- 1
d$kngpty[which(x$CC16_322a=="Other Party / Independent" & x$CurrentGovParty=="Independent")] <- 1
d$kngpty[which(x$CC16_322a=="Not Asked")] <- NA
table(x$CurrentGovParty,d$kngpty,useNA="always")

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
table(x$CC16_340a) # Yourself 
d$ide_self <- as.numeric(x$CC16_340a) - 4 
d$ide_self[which(x$CC16_340a %in% c("Not sure"))] <- 0
d$ide_self[which(x$CC16_340a %in% c("Skipped","Not Asked"))] <- NA
table(d$ide_self)

# 1. Ideological Distance Difference with Presidential Candidates
table(x$CC16_340d) # Clinton
table(x$CC16_340e) # Trump
# Republican Position
d$ide_prep <- as.numeric(x$CC16_340e) - 4
d$ide_prep[which(x$CC16_340e %in% c("Not sure"))] <- 0
d$ide_prep[which(x$CC16_340e %in% c("Skipped","Not Asked"))] <- NA
table(d$ide_prep)
# Democrat Position
d$ide_pdem <- as.numeric(x$CC16_340d) - 4
d$ide_pdem[which(x$CC16_340d %in% c("Not sure"))] <- 0
d$ide_pdem[which(x$CC16_340d %in% c("Skipped","Not Asked"))] <- NA
table(d$ide_pdem)
# Positional Distance Difference 
d$idedist_prep <- abs(d$ide_prep - d$ide_self)
d$idedist_pdem <- abs(d$ide_pdem - d$ide_self)
table(d$idedist_prep, useNA="always")
table(d$idedist_pdem, useNA="always")
d$idedist_prepadv <- -(d$idedist_prep - d$idedist_pdem)
# d$idedist_prepadv[which(x$CC16_340a=="Not sure")] <- 0
# d$idedist_prepadv[which(x$CC16_340d=="Not sure")] <- 0
# d$idedist_prepadv[which(x$CC16_340e=="Not sure")] <- 0
d$idedist_padvm <- abs(d$idedist_prepadv) # Advantage Magnitude
d$idedist_padvr <- (d$idedist_prepadv>0)*1 # Republican Has Advantage
d$idedist_padvmr <- d$idedist_padvm*d$idedist_padvr # Advantage Magnitude (Republican)
d$idedist_padvmd <- d$idedist_padvm*(1-d$idedist_padvr) # Advantage Magnitude (Republican)
table(d$idedist_prepadv, useNA="always")
table(d$idedist_padvm, useNA="always")
table(d$idedist_padvr, useNA="always")
# Squared Distance
d$idedistsq_prep <- (d$ide_prep - d$ide_self)^2
d$idedistsq_pdem <- (d$ide_pdem - d$ide_self)^2
table(d$idedistsq_prep, useNA="always"); hist(d$idedistsq_prep)
table(d$idedistsq_pdem, useNA="always"); hist(d$idedistsq_pdem)
d$idedistsq_prepadv <- -(d$idedistsq_prep - d$idedistsq_pdem)
table(d$idedistsq_prepadv, useNA="always"); hist(d$idedistsq_prepadv)

# 2. Ideological Distance Difference with Senate Candidates
table(x$CC16_340l) # Senate Candidate 1 (Democrat)
table(x$CC16_340m) # Senate Candidate 2 (Mostly Republican)
# Republican Position
d$ide_srep <- as.numeric(x$CC16_340m) - 4
d$ide_srep[which(x$CC16_340m %in% c("Not sure"))] <- 0
d$ide_srep[which(x$CC16_340m %in% c("Skipped","Not Asked"))] <- NA
d$ide_srep[-which(paste(x$SenCand1Party,x$SenCand2Party) %in% 
                    c("Democratic Republican"))] <- NA
table(d$ide_srep)
# Democrat Position
d$ide_sdem <- as.numeric(x$CC16_340l) - 4
d$ide_sdem[which(x$CC16_340l %in% c("Not sure"))] <- 0
d$ide_sdem[which(x$CC16_340l %in% c("Skipped","Not Asked"))] <- NA
d$ide_sdem[-which(paste(x$SenCand1Party,x$SenCand2Party) %in% 
                    c("Democratic Republican"))] <- NA
table(d$ide_sdem)
# Positional Distance Difference 
d$idedist_srep <- abs(d$ide_srep - d$ide_self)
d$idedist_sdem <- abs(d$ide_sdem - d$ide_self)
table(d$idedist_srep); table(d$idedist_sdem)
d$idedist_srepadv <- -(d$idedist_srep - d$idedist_sdem)
# d$idedist_srepadv[which(x$CC16_340a=="Not sure")] <- 0
# d$idedist_srepadv[which(x$CC16_340l=="Not sure")] <- 0
# d$idedist_srepadv[which(x$CC16_340m=="Not sure")] <- 0
d$idedist_sadvm <- abs(d$idedist_srepadv) # Advantage Magnitude
d$idedist_sadvr <- (d$idedist_srepadv>0)*1 # Republican Has Advantage
d$idedist_sadvmr <- d$idedist_sadvm*d$idedist_sadvr # Advantage Magnitude (Republican)
d$idedist_sadvmd <- d$idedist_sadvm*(1-d$idedist_sadvr) # Advantage Magnitude (Republican)
table(d$idedist_srepadv)
table(d$idedist_sadvm)
table(d$idedist_sadvr)
# Squared Distance
d$idedistsq_srep <- (d$ide_srep - d$ide_self)^2
d$idedistsq_sdem <- (d$ide_sdem - d$ide_self)^2
table(d$idedistsq_srep, useNA="always"); hist(d$idedistsq_srep)
table(d$idedistsq_sdem, useNA="always"); hist(d$idedistsq_sdem)
d$idedistsq_srepadv <- -(d$idedistsq_srep - d$idedistsq_sdem)
table(d$idedistsq_srepadv, useNA="always"); hist(d$idedistsq_srepadv)

# 3. Ideological Distance Difference with House Candidates
table(x$CC16_340n) # House Candidate 1 (Democrat)
table(x$CC16_340o) # House Candidate 2 (Mostly Republican)
# Republican Position
d$ide_hrep <- as.numeric(x$CC16_340o) - 4
d$ide_hrep[which(x$CC16_340o %in% c("Not sure"))] <- 0
d$ide_hrep[which(x$CC16_340o %in% c("Skipped","Not Asked"))] <- NA
d$ide_hrep[-which(paste(x$HouseCand1Party,x$HouseCand2Party) %in% 
                    c("Democratic Republican"))] <- NA
table(d$ide_hrep)
# Democrat Position
d$ide_hdem <- as.numeric(x$CC16_340n) - 4
d$ide_hdem[which(x$CC16_340n %in% c("Not sure"))] <- 0
d$ide_hdem[which(x$CC16_340n %in% c("Skipped","Not Asked"))] <- NA
d$ide_hdem[-which(paste(x$HouseCand1Party,x$HouseCand2Party) %in% 
                    c("Democratic Republican"))] <- NA
table(d$ide_hdem)
# Positional Distance Difference 
d$idedist_hrep <- abs(d$ide_hrep - d$ide_self)
d$idedist_hdem <- abs(d$ide_hdem - d$ide_self)
table(d$idedist_hrep); table(d$idedist_hdem)
d$idedist_hrepadv <- -(d$idedist_hrep - d$idedist_hdem)
# d$idedist_hrepadv[which(x$CC16_340a=="Not sure")] <- 0
# d$idedist_hrepadv[which(x$CC16_340n=="Not sure")] <- 0
# d$idedist_hrepadv[which(x$CC16_340o=="Not sure")] <- 0
d$idedist_hadvm <- abs(d$idedist_hrepadv) # Advantage Magnitude
d$idedist_hadvr <- (d$idedist_hrepadv>0)*1 # Republican Has Advantage
d$idedist_hadvmr <- d$idedist_hadvm*d$idedist_hadvr # Advantage Magnitude (Republican)
d$idedist_hadvmd <- d$idedist_hadvm*(1-d$idedist_hadvr) # Advantage Magnitude (Republican)
table(d$idedist_hrepadv)
table(d$idedist_hadvm)
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

# 0. Voted in 2016 Election
table(x$CC16_401)
d$voted <- (x$CC16_401=="I definitely voted in the General Election.")*1
table(d$voted)

# 1a. Presidential Voting Intention (Pre-Election)
table(x$CC16_364)
table(x$CC16_364b)
table(x$CC16_364c)
d$vpint <- NA
d$vpint[which(x$CC16_364=="No")] <- "Abstain"
d$vpint[which(x$CC16_364b=="I didn't vote in this election")] <- "Abstain"
d$vpint[which(x$CC16_364b=="Donald Trump (Republican)")] <- "Republican"
d$vpint[which(x$CC16_364b=="Hillary Clinton (Democrat)")] <- "Democrat"
d$vpint[which(x$CC16_364b %in% c("Gary Johnson (Libertarian)",
                                 "Jill Stein (Green)","Other"))] <- "Other"
d$vpint[which(x$CC16_364b %in% c("I'm not sure","Skipped"))] <- NA
d$vpint[which(x$CC16_364c=="I won't vote in this election")] <- "Abstain"
d$vpint[which(x$CC16_364c=="Donald Trump (Republican)")] <- "Republican"
d$vpint[which(x$CC16_364c=="Hillary Clinton (Democrat)")] <- "Democrat"
d$vpint[which(x$CC16_364c %in% c("Gary Johnson (Libertarian)",
                                   "Jill Stein (Green)","Other"))] <- "Other"
d$vpint[which(x$CC16_364c %in% c("I'm not sure","Skipped"))] <- "Undecided"
d$vpint <- factor(d$vpint,levels=c("Abstain","Republican","Democrat","Other","Undecided"))
table(d$vpint, useNA="always")
d$vpintx <- as.numeric(d$vpint)-1
table(d$vpintx,d$vpint, useNA="always")

# 1b. Presidential Voting Result (Post-Election)
table(x$votereg_post)
table(x$CC16_401)
table(x$CC16_410a)
d$vpres <- NA
d$vpres[which(x$CC16_410a=="I didn't vote in this election")] <- "Abstain"
d$vpres[which(x$CC16_410a=="Donald Trump (Republican)")] <- "Republican"
d$vpres[which(x$CC16_410a=="Hillary Clinton (Democrat)")] <- "Democrat"
d$vpres[which(x$CC16_410a %in% c("Gary Johnson (Libertarian)",
                                 "Jill Stein (Green)",
                                 "Evan McMullin (Independent)","Other"))] <- "Other"
d$vpres[which(x$CC16_410a %in% c("I'm not sure","Skipped","Not Asked"))] <- NA
d$vpres[which(x$CC16_401 %in% c("I did not vote in the election this November.",
                                "I thought about voting this time - but didn't",
                                "I usually vote, but didn't this time.",
                                "I attempted to vote but did not or could not."))] <- "Abstain"
d$vpres[which(x$votereg_post=="No")] <- "Abstain"
d$vpres <- factor(d$vpres,levels=c("Abstain","Republican","Democrat","Other"))
table(d$vpres, useNA="always")
d$vpresx <- as.numeric(d$vpres)-1
table(d$vpresx, useNA="always")

# 2a. Senate Voting Intention
table(x$CC16_364)
table(x$CC16_365)
table(x$CC16_365x)
table(x$SenCand1Party,useNA="always")
d$vsint <- NA
d$vsint[which(x$CC16_364=="No")] <- "Abstain" 
d$vsint[is.na(x$SenCand1Party)] <- NA # No Election
d$vsint[which(x$CC16_365=="$SenCand1Name ($SenCand1Party)")] <- 
  x$SenCand1Party[which(x$CC16_365=="$SenCand1Name ($SenCand1Party)")]
d$vsint[which(x$CC16_365=="$SenCand2Name ($SenCand2Party)")] <- 
  x$SenCand2Party[which(x$CC16_365=="$SenCand2Name ($SenCand2Party)")]
d$vsint[which(x$CC16_365=="$SenCand3Name ($SenCand3Party)")] <- 
  x$SenCand3Party[which(x$CC16_365=="$SenCand3Name ($SenCand3Party)")]
d$vsint[which(x$CC16_365=="$SenCand4Name ($SenCand4Party)")] <- 
  x$SenCand4Party[which(x$CC16_365=="$SenCand4Name ($SenCand4Party)")]
d$vsint[which(x$CC16_365=="Other")] <- "Other"
d$vsint[which(x$CC16_365 %in% c("I'm not sure","Skipped"))] <- "Undecided"
d$vsint[which(x$CC16_365 %in% c("No one"))] <- "Abstain"
d$vsint[which(x$CC16_365x=="$SenCand1Name ($SenCand1Party)")] <- 
  x$SenCand1Party[which(x$CC16_365x=="$SenCand1Name ($SenCand1Party)")]
d$vsint[which(x$CC16_365x=="$SenCand2Name ($SenCand2Party)")] <- 
  x$SenCand2Party[which(x$CC16_365x=="$SenCand2Name ($SenCand2Party)")]
d$vsint[which(x$CC16_365x=="$SenCand3Name ($SenCand3Party)")] <- 
  x$SenCand3Party[which(x$CC16_365x=="$SenCand3Name ($SenCand3Party)")]
d$vsint[which(x$CC16_365x=="$SenCand4Name ($SenCand4Party)")] <- 
  x$SenCand4Party[which(x$CC16_365x=="$SenCand4Name ($SenCand4Party)")]
d$vsint[which(x$CC16_365x=="Other")] <- "Other"
d$vsint[which(x$CC16_365x %in% c("Not sure","Skipped"))] <- NA
d$vsint[which(x$CC16_365x %in% c("I did not vote",
                                 "I did not vote in this race"))] <- "Abstain"
d$vsint[which(d$vsint %in% c("Libertarian","Constitution"))] <- "Other"
d$vsint[which(d$vsint %in% c("Democratic"))] <- "Democrat"
d$vsint <- factor(d$vsint,levels=c("Abstain","Republican","Democrat","Other","Undecided"))
d$vsint[-which(paste(x$SenCand1Party,x$SenCand2Party) %in% 
                    c("Democratic Republican"))] <- NA
table(d$vsint, useNA="always")
d$vsintx <- as.numeric(d$vsint)-1
table(d$vsintx, useNA="always")

# 2b. Senate Voting Result 
table(x$votereg_post)
table(x$CC16_401)
table(x$CC16_410b)
table(x$SenCand1Party_post, useNA="always")
d$vsres <- NA
d$vsres[which(x$CC16_410b=="$SenCand1Name ($SenCand1Party)")] <- 
  x$SenCand1Party[which(x$CC16_410b=="$SenCand1Name ($SenCand1Party)")]
d$vsres[which(x$CC16_410b=="$SenCand2Name ($SenCand2Party)")] <- 
  x$SenCand2Party[which(x$CC16_410b=="$SenCand2Name ($SenCand2Party)")]
d$vsres[which(x$CC16_410b=="$SenCand3Name ($SenCand3Party)")] <- 
  x$SenCand3Party[which(x$CC16_410b=="$SenCand3Name ($SenCand3Party)")]
d$vsres[which(x$CC16_410b=="$SenCand4Name ($SenCand4Party)")] <- 
  x$SenCand4Party[which(x$CC16_410b=="$SenCand4Name ($SenCand4Party)")]
d$vsres[which(x$CC16_410b=="Other")] <- "Other"
d$vsres[which(x$CC16_410b %in% c("Not sure","Skipped"))] <- NA
d$vsres[which(x$CC16_410b %in% c("I did not vote in this race"))] <- "Abstain"
d$vsres[which(x$CC16_401 %in% c("I did not vote in the election this November.",
                                "I thought about voting this time - but didn't",
                                "I usually vote, but didn't this time.",
                                "I attempted to vote but did not or could not."))] <- "Abstain"
d$vsres[which(x$votereg_post=="No")] <- "Abstain"
d$vsres[is.na(x$SenCand1Party_post)] <- NA # No Election
d$vsres[which(d$vsres %in% c("Libertarian","Constitution"))] <- "Other"
d$vsres[which(d$vsres %in% c("Democratic"))] <- "Democrat"
d$vsres <- factor(d$vsres,levels=c("Abstain","Republican","Democrat","Other"))
d$vsres[-which(paste(x$SenCand1Party_post,x$SenCand2Party_post) %in% 
                    c("Democratic Republican"))] <- NA
table(d$vsres, useNA="always")
d$vsresx <- as.numeric(d$vsres)-1
table(d$vsresx, useNA="always")

# 3a. House Voting Intention
table(x$CC16_364)
table(x$CC16_367, useNA="always")
table(x$CC16_367x)
table(x$HouseCand1Party, useNA="always")
d$vhint <- NA
d$vhint[which(x$CC16_364=="No")] <- "Abstain" 
d$vhint[is.na(x$HouseCand1Party)] <- NA # No Election
d$vhint[which(x$CC16_367=="$HouseCand1Name ($HouseCand1Party)")] <- 
  x$HouseCand1Party[which(x$CC16_367=="$HouseCand1Name ($HouseCand1Party)")]
d$vhint[which(x$CC16_367=="$HouseCand2Name ($HouseCand2Party)")] <- 
  x$HouseCand2Party[which(x$CC16_367=="$HouseCand2Name ($HouseCand2Party)")]
d$vhint[which(x$CC16_367=="$HouseCand3Name ($HouseCand3Party)")] <- 
  x$HouseCand3Party[which(x$CC16_367=="$HouseCand3Name ($HouseCand3Party)")]
d$vhint[which(x$CC16_367=="$HouseCand4Name ($HouseCand4Party)")] <- 
  x$HouseCand4Party[which(x$CC16_367=="$HouseCand4Name ($HouseCand4Party)")]
d$vhint[which(x$CC16_367=="$HouseCand5Name ($HouseCand5Party)")] <- 
  x$HouseCand5Party[which(x$CC16_367=="$HouseCand5Name ($HouseCand5Party)")]
d$vhint[which(x$CC16_367=="$HouseCand6Name ($HouseCand6Party)")] <- 
  x$HouseCand6Party[which(x$CC16_367=="$HouseCand6Name ($HouseCand6Party)")]
d$vhint[which(x$CC16_367=="$HouseCand7Name ($HouseCand1Party)")] <- 
  x$HouseCand7Party[which(x$CC16_367=="$HouseCand7Name ($HouseCand1Party)")]
d$vhint[which(x$CC16_367=="$HouseCand8Name ($HouseCand8Party)")] <- 
  x$HouseCand8Party[which(x$CC16_367=="$HouseCand8Name ($HouseCand8Party)")]
d$vhint[which(x$CC16_367=="$HouseCand9Name ($HouseCand9Party)")] <- 
  x$HouseCand9Party[which(x$CC16_367=="$HouseCand9Name ($HouseCand9Party)")]
d$vhint[which(x$CC16_367=="$HouseCand10Name ($HouseCand10Party)")] <- 
  x$HouseCand10Party[which(x$CC16_367=="$HouseCand10Name ($HouseCand10Party)")]
d$vhint[which(x$CC16_367=="$HouseCand11Name ($HouseCand11Party)")] <- 
  x$HouseCand11Party[which(x$CC16_367=="$HouseCand11Name ($HouseCand11Party)")]
d$vhint[which(x$CC16_367=="Other")] <- "Other"
d$vhint[which(x$CC16_367 %in% c("I'm not sure","Skipped"))] <- "Undecided"
d$vhint[which(x$CC16_367 %in% c("No one"))] <- "Abstain"
d$vhint[which(x$CC16_367x=="$HouseCand1Name ($HouseCand1Party)")] <- 
  x$HouseCand1Party[which(x$CC16_367x=="$HouseCand1Name ($HouseCand1Party)")]
d$vhint[which(x$CC16_367x=="$HouseCand2Name ($HouseCand2Party)")] <- 
  x$HouseCand2Party[which(x$CC16_367x=="$HouseCand2Name ($HouseCand2Party)")]
d$vhint[which(x$CC16_367x=="$HouseCand3Name ($HouseCand3Party)")] <- 
  x$HouseCand3Party[which(x$CC16_367x=="$HouseCand3Name ($HouseCand3Party)")]
d$vhint[which(x$CC16_367x=="$HouseCand4Name ($HouseCand4Party)")] <- 
  x$HouseCand4Party[which(x$CC16_367x=="$HouseCand4Name ($HouseCand4Party)")]
d$vhint[which(x$CC16_367x=="$HouseCand5Name ($HouseCand5Party)")] <- 
  x$HouseCand5Party[which(x$CC16_367x=="$HouseCand5Name ($HouseCand5Party)")]
d$vhint[which(x$CC16_367x=="$HouseCand6Name ($HouseCand6Party)")] <- 
  x$HouseCand6Party[which(x$CC16_367x=="$HouseCand6Name ($HouseCand6Party)")]
d$vhint[which(x$CC16_367x=="$HouseCand7Name ($HouseCand1Party)")] <- 
  x$HouseCand7Party[which(x$CC16_367x=="$HouseCand7Name ($HouseCand1Party)")]
d$vhint[which(x$CC16_367x=="$HouseCand8Name ($HouseCand8Party)")] <- 
  x$HouseCand8Party[which(x$CC16_367x=="$HouseCand8Name ($HouseCand8Party)")]
d$vhint[which(x$CC16_367x=="$HouseCand9Name ($HouseCand9Party)")] <- 
  x$HouseCand9Party[which(x$CC16_367x=="$HouseCand9Name ($HouseCand9Party)")]
d$vhint[which(x$CC16_367x=="$HouseCand10Name ($HouseCand10Party)")] <- 
  x$HouseCand10Party[which(x$CC16_367x=="$HouseCand10Name ($HouseCand10Party)")]
d$vhint[which(x$CC16_367x=="$HouseCand11Name ($HouseCand11Party)")] <- 
  x$HouseCand11Party[which(x$CC16_367x=="$HouseCand11Name ($HouseCand11Party)")]
d$vhint[which(x$CC16_367x=="Other")] <- "Other"
d$vhint[which(x$CC16_367x %in% c("Not sure","Skipped"))] <- NA
d$vhint[which(x$CC16_367x %in% c("I did not vote",
                                 "I did not vote in this race"))] <- "Abstain"
d$vhint[which(d$vhint %in% c("Libertarian","Conservative Party",
                             "Independent","Green","Liberty Union"))] <- "Other"
d$vhint[which(d$vhint %in% c("Democratic"))] <- "Democrat"
d$vhint <- factor(d$vhint,levels=c("Abstain","Republican","Democrat","Other","Undecided"))
d$vhint[-which(paste(x$HouseCand1Party,x$HouseCand2Party) %in% 
                    c("Democratic Republican"))] <- NA
table(d$vhint, useNA="always")
d$vhintx <- as.numeric(d$vhint)-1
table(d$vhintx, useNA="always")

# 3b. House Voting Result 
table(x$votereg_post)
table(x$CC16_401)
table(x$CC16_412)
table(x$HouseCand1Party_post, useNA="always")
d$vhres <- NA
d$vhres[which(x$CC16_412=="$HouseCand1Name ($HouseCand1Party)")] <- 
  x$HouseCand1Party_post[which(x$CC16_412=="$HouseCand1Name ($HouseCand1Party)")]
d$vhres[which(x$CC16_412=="$HouseCand2Name ($HouseCand2Party)")] <- 
  x$HouseCand2Party_post[which(x$CC16_412=="$HouseCand2Name ($HouseCand2Party)")]
d$vhres[which(x$CC16_412=="$HouseCand3Name ($HouseCand3Party)")] <- 
  x$HouseCand3Party_post[which(x$CC16_412=="$HouseCand3Name ($HouseCand3Party)")]
d$vhres[which(x$CC16_412=="$HouseCand4Name ($HouseCand4Party)")] <- 
  x$HouseCand4Party_post[which(x$CC16_412=="$HouseCand4Name ($HouseCand4Party)")]
d$vhres[which(x$CC16_412=="$HouseCand5Name ($HouseCand5Party)")] <- 
  x$HouseCand5Party_post[which(x$CC16_412=="$HouseCand5Name ($HouseCand5Party)")]
d$vhres[which(x$CC16_412=="$HouseCand6Name ($HouseCand6Party)")] <- 
  x$HouseCand6Party_post[which(x$CC16_412=="$HouseCand6Name ($HouseCand6Party)")]
d$vhres[which(x$CC16_412=="$HouseCand7Name ($HouseCand1Party)")] <- 
  x$HouseCand7Party_post[which(x$CC16_412=="$HouseCand7Name ($HouseCand1Party)")]
d$vhres[which(x$CC16_412=="$HouseCand8Name ($HouseCand8Party)")] <- 
  x$HouseCand8Party_post[which(x$CC16_412=="$HouseCand8Name ($HouseCand8Party)")]
d$vhres[which(x$CC16_412=="$HouseCand9Name ($HouseCand9Party)")] <- 
  x$HouseCand9Party_post[which(x$CC16_412=="$HouseCand9Name ($HouseCand9Party)")]
d$vhres[which(x$CC16_412=="$HouseCand10Name ($HouseCand10Party)")] <- 
  x$HouseCand10Party_post[which(x$CC16_412=="$HouseCand10Name ($HouseCand10Party)")]
d$vhres[which(x$CC16_412=="$HouseCand11Name ($HouseCand11Party)")] <- 
  x$HouseCand11Party_post[which(x$CC16_412=="$HouseCand11Name ($HouseCand11Party)")]
d$vhres[which(x$CC16_412=="Other")] <- "Other"
d$vhres[which(x$CC16_412 %in% c("Not sure","Skipped"))] <- NA
d$vhres[which(x$CC16_412 %in% c("I did not vote in this race"))] <- "Abstain"
d$vhres[which(x$CC16_401 %in% c("I did not vote in the election this November.",
                                "I thought about voting this time - but didn't",
                                "I usually vote, but didn't this time.",
                                "I attempted to vote but did not or could not."))] <- "Abstain"
d$vhres[which(x$votereg_post=="No")] <- "Abstain"
d$vhres[is.na(x$HouseCand1Party_post)] <- NA # No Election
d$vhres[which(d$vhres %in% c("Libertarian","Conservative Party",
                             "Independent","Green","Liberty Union"))] <- "Other"
d$vhres[which(d$vhres %in% c("Democratic"))] <- "Democrat"
d$vhres <- factor(d$vhres,levels=c("Abstain","Republican","Democrat","Other"))
d$vhres[-which(paste(x$HouseCand1Party_post,x$HouseCand2Party_post) %in% 
                    c("Democratic Republican"))] <- NA
table(d$vhres, useNA="always")
d$vhresx <- as.numeric(d$vhres)-1
table(d$vhresx, useNA="always")

# 4a. Governor Voting Intention
table(x$CC16_364)
table(x$CC16_366, useNA="always")
table(x$CC16_366x)
table(x$GovCand1Name, useNA="always")
d$vgint <- NA
d$vgint[which(x$CC16_364=="No")] <- "Abstain" 
d$vgint[is.na(x$GovCand1Name)] <- NA # No Election 
d$vgint[which(x$CC16_366=="$GovCand1Name ($GovCand1Party)")] <- 
  x$GovCand1Party[which(x$CC16_366=="$GovCand1Name ($GovCand1Party)")]
d$vgint[which(x$CC16_366=="$GovCand2Name ($GovCand2Party)")] <- 
  x$GovCand2Party[which(x$CC16_366=="$GovCand2Name ($GovCand2Party)")]
d$vgint[which(x$CC16_366=="$GovCand3Name ($GovCand3Party)")] <- 
  x$GovCand3Party[which(x$CC16_366=="$GovCand3Name ($GovCand3Party)")]
d$vgint[which(x$CC16_366=="Other")] <- "Other"
d$vgint[which(x$CC16_366 %in% c("I'm not sure","Skipped"))] <- "Undecided"
d$vgint[which(x$CC16_366 %in% c("No one"))] <- "Abstain"
d$vgint[which(x$CC16_366x=="$GovCand1Name ($GovCand1Party)")] <- 
  x$GovCand1Party[which(x$CC16_366x=="$GovCand1Name ($GovCand1Party)")]
d$vgint[which(x$CC16_366x=="$GovCand2Name ($GovCand2Party)")] <- 
  x$GovCand2Party[which(x$CC16_366x=="$GovCand2Name ($GovCand2Party)")]
d$vgint[which(x$CC16_366x=="$GovCand3Name ($GovCand3Party)")] <- 
  x$GovCand3Party[which(x$CC16_366x=="$GovCand3Name ($GovCand3Party)")]
d$vgint[which(x$CC16_366x=="Other")] <- "Other"
d$vgint[which(x$CC16_366x %in% c("Not sure","Skipped"))] <- NA
d$vgint[which(x$CC16_366x %in% c("I did not vote",
                                 "I did not vote in this race"))] <- "Abstain"
d$vgint[which(d$vgint %in% c("Libertarian",
                             "Independent Party of Oregon"))] <- "Other"
d$vgint[which(d$vgint %in% c("Democratic"))] <- "Democrat"
d$vgint <- factor(d$vgint,levels=c("Abstain","Republican","Democrat","Other","Undecided"))
d$vgint[-which(paste(x$GovCand1Party,x$GovCand2Party) %in% 
                    c("Democratic Republican"))] <- NA
table(d$vgint, useNA="always")
d$vgintx <- as.numeric(d$vgint)-1
table(d$vgintx, useNA="always")

# 4b. Governor Voting Result 
table(x$votereg_post)
table(x$CC16_401)
table(x$CC16_411)
table(x$GovCand1Name_post, useNA="always")
d$vgres <- NA
d$vgres[which(x$CC16_411=="$GovCand1Name ($GovCand1Party)")] <- 
  x$GovCand1Party_post[which(x$CC16_411=="$GovCand1Name ($GovCand1Party)")]
d$vgres[which(x$CC16_411=="$GovCand2Name ($GovCand2Party)")] <- 
  x$GovCand2Party_post[which(x$CC16_411=="$GovCand2Name ($GovCand2Party)")]
d$vgres[which(x$CC16_411=="$GovCand3Name ($GovCand3Party)")] <- 
  x$GovCand3Party_post[which(x$CC16_411=="$GovCand3Name ($GovCand3Party)")]
d$vgres[which(x$CC16_411=="Other")] <- "Other"
d$vgres[which(x$CC16_411 %in% c("Not sure","Skipped"))] <- NA
d$vgres[which(x$CC16_411 %in% c("I did not vote in this race"))] <- "Abstain"
d$vgres[which(x$CC16_401 %in% c("I did not vote in the election this November.",
                                "I thought about voting this time - but didn't",
                                "I usually vote, but didn't this time.",
                                "I attempted to vote but did not or could not."))] <- "Abstain"
d$vgres[which(x$votereg_post=="No")] <- "Abstain"
d$vgres[is.na(x$GovCand1Name_post)] <- NA # No Election 
d$vgres[which(d$vgres %in% c("Libertarian",
                             "Independent Party of Oregon"))] <- "Other"
d$vgres[which(d$vgres %in% c("Democratic"))] <- "Democrat"
d$vgres <- factor(d$vgres,levels=c("Abstain","Republican","Democrat","Other"))
table(d$vgres, useNA="always")
d$vgres[-which(paste(x$GovCand1Party_post,x$GovCand2Party_post) %in% 
                    c("Democratic Republican"))] <- NA
d$vgresx <- as.numeric(d$vgres)-1
table(d$vgresx, useNA="always")

# 5. Voting Result for State Senate Election
table(x$CC16_413c)
d$vssres <- NA
d$vssres[which(x$CC16_413c=="Democratic candidate")] <- "Democrat"
d$vssres[which(x$CC16_413c=="Republican candidate")] <- "Republican"
d$vssres[which(x$CC16_413c=="Other candidate")] <- "Other"
d$vssres[which(x$CC16_413c=="Did not vote in this race")] <- "Abstain"
d$vssres[which(x$CC16_413c %in% c("There was no race for this office",
                                  "Skipped","Not Asked"))] <- NA
d$vssres <- factor(d$vssres,levels=c("Abstain","Republican","Democrat","Other"))
table(d$vssres, useNA="always")
d$vssresx <- as.numeric(d$vssres)-1
table(d$vssresx, useNA="always")

# 6. Voting Result for State House (Lower Chamber) Election
table(x$CC16_413d)
d$vshres <- NA
d$vshres[which(x$CC16_413d=="Democratic candidate")] <- "Democrat"
d$vshres[which(x$CC16_413d=="Republican candidate")] <- "Republican"
d$vshres[which(x$CC16_413d=="Other candidate")] <- "Other"
d$vshres[which(x$CC16_413d=="Did not vote in this race")] <- "Abstain"
d$vshres[which(x$CC16_413d %in% c("There was no race for this office",
                                  "Skipped","Not Asked"))] <- NA
d$vshres <- factor(d$vshres,levels=c("Abstain","Republican","Democrat","Other"))
table(d$vshres, useNA="always")
d$vshresx <- as.numeric(d$vshres)-1
table(d$vshresx, useNA="always")

# 7. Voting Result for State Attorney Election
table(x$CC16_413a)
d$vsares <- NA
d$vsares[which(x$CC16_413a=="Democratic candidate")] <- "Democrat"
d$vsares[which(x$CC16_413a=="Republican candidate")] <- "Republican"
d$vsares[which(x$CC16_413a=="Other candidate")] <- "Other"
d$vsares[which(x$CC16_413a=="Did not vote in this race")] <- "Abstain"
d$vsares[which(x$CC16_413a %in% c("There was no race for this office",
                                  "Skipped","Not Asked"))] <- NA
d$vsares <- factor(d$vsares,levels=c("Abstain","Republican","Democrat","Other"))
table(d$vsares, useNA="always")
d$vsaresx <- as.numeric(d$vsares)-1
table(d$vsaresx, useNA="always")

# 8. Voting Result for State Secretary Election
table(x$CC16_413b)
d$vssecres <- NA
d$vssecres[which(x$CC16_413b=="Democratic candidate")] <- "Democrat"
d$vssecres[which(x$CC16_413b=="Republican candidate")] <- "Republican"
d$vssecres[which(x$CC16_413b=="Other candidate")] <- "Other"
d$vssecres[which(x$CC16_413b=="Did not vote in this race")] <- "Abstain"
d$vssecres[which(x$CC16_413b %in% c("There was no race for this office",
                                  "Skipped","Not Asked"))] <- NA
d$vssecres <- factor(d$vssecres,levels=c("Abstain","Republican","Democrat","Other"))
table(d$vssecres, useNA="always")
d$vssecresx <- as.numeric(d$vssecres)-1
table(d$vssecresx, useNA="always")

##############
## Controls ##
##############

## Demographics ##

# 1. Gender (female)
table(x$gender)
d$female <- as.numeric(x$gender)-1
table(d$female)

# 2. Age
table(y$birthyr)
d$age <- 2016-y$birthyr
table(d$age)

# 3. Race
table(x$race)
d$white <- (x$race=="White")*1
#d$white[which(x$race=="Mixed" & x$multrace_1=="Yes")] <- 1
d$black <- (x$race=="Black")*1
#d$black[which(x$race=="Mixed" & x$multrace_2=="Yes")] <- 1
d$latino <- (x$race=="Hispanic")*1
#d$latino[which(x$race=="Mixed" & x$multrace_3=="Yes")] <- 1
d$asian <- (x$race=="Asian")*1
#d$asian <- (x$race=="Asian" & x$multrace_4=="Yes")*1
d$other <- (x$race %in% c("Native American","Other","Middle Eastern"))*1
d$other[x$race=="Mixed"] <- 1
# d$other[which(x$race=="Mixed" & (x$multrace_5=="Yes" |
#                                  x$multrace_8=="Yes" |
#                                  x$multrace_97=="Yes"))] <- 1
table(d$white,x$race)
table(d$black,x$race)
table(d$latino,x$race)
table(d$asian,x$race)
table(d$other,x$race)
table(d$white,d$black)
# Or Mutually exclusive races
d$race <- x$race
d$race[d$race %in% c("Native American","Mixed","Other","Middle Eastern")] <- "Other"
table(d$race)
d$race <- factor(d$race, levels=c("White","Black","Hispanic","Asian","Other"))
table(d$race, useNA="always")

# 4. Marital Status
table(x$marstat)
d$married <- NA
d$married[which(x$marstat %in% c("Married","Separated",
                                 "Domestic partnership"))] <- 1
d$married[which(x$marstat %in% c("Single","Divorced","Widowed"))] <- 0
table(d$married)

# 4. Full-Time Working Status
table(x$employ)
d$fulltime <- (x$employ=="Full-time")*1

# 5. Family Income
table(x$faminc)
d$income <- as.numeric(x$faminc)
d$income[which(x$faminc %in% c("Prefer not to say","Skipped"))] <- NA
table(d$income)

## Societal ##

# 1. Education
table(x$educ)
d$edu <- as.numeric(x$educ)-1 # 6 cats
table(d$edu)

# 2a. Church Attendance 
table(x$pew_churatd)
d$chatd <- 6-as.numeric(x$pew_churatd)
d$chatd[which(x$pew_churatd=="Don't know")] <- 0
d$chatd[which(x$pew_churatd=="Skipped")] <- NA
table(d$chatd)

# 2b. Born Again Christian
table(x$pew_bornagain)
d$bornagain <- 2-as.numeric(x$pew_bornagain)
d$bornagain[which(x$pew_bornagain=="Don't know")] <- 0
d$bornagain[which(x$pew_bornagain=="Skipped")] <- NA
table(d$bornagain)

# 3. Residence Length
table(x$CC16_361)
d$resid <- as.numeric(x$CC16_361)
d$resid[which(x$CC16_361 %in% c("Skipped","Not Asked"))] <- NA

## Attitudinal ##

# 1. Public Affair Interest (Political Interest)
table(x$newsint)
d$polint <- 4 - as.numeric(x$newsint)
d$polint[which(x$newsint=="Don't know")] <- 0
d$polint[which(x$newsint=="Skipped")] <- NA
table(d$polint)

# 2. Party ID Strength
table(x$pid7)
d$pidstr <- abs(as.numeric(x$pid7) - 4)
table(d$pidstr)
d$pidstr[which(x$pid7=="Not sure")] <- 0
d$pidstr[which(x$pid7 %in% c("Skipped","Not Asked"))] <- NA
table(d$pidstr)
d$repstr <- d$pidstr
d$repstr[which(as.numeric(x$pid7) %in% c(1,2,3))] <- 0
table(d$repstr)
d$demstr <- d$pidstr
d$demstr[which(as.numeric(x$pid7) %in% c(5,6,7))] <- 0
table(d$demstr)

# 2.5 Party ID
d$pid <- d$repstr - d$demstr

# 3. National Economy Evaluation
table(x$CC16_302)
d$evecon <- 3 - as.numeric(x$CC16_302)
d$evecon[which(x$CC16_302 %in% c("Not sure"))] <- 0
d$evecon[which(x$CC16_302 %in% c("Skipped","Not Asked"))] <- NA
table(d$evecon)

# 4. Income Increase for the Past Four Years
table(x$CC16_303)
d$retinc <- 3 - as.numeric(x$CC16_303)
d$retinc[which(x$CC16_303 %in% c("Skipped","Not Asked"))] <- NA
table(d$retinc)

# attr(x, "var.labels")[which(names(x)=="newsint")]

###############
## Save Data ##
###############

saveRDS(d,file="data/cces16s.rds")
