#' ---
#' title: "Creating Objective Knowledge Scales"
#' author: "Gento Kato"
#' date: "June 26, 2019"
#' ---

#' 
#' # Preparation
#'
#' ## Clear Workspace
#' 
rm(list=ls())
#' 
#' ## Set Working Directory (The directory of this .R file)
#' 
library(rprojroot);library(rstudioapi)
if (rstudioapi::isAvailable()==TRUE) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path)); 
} 
projdir <- find_root(has_file("thisishome.txt"))
cat(paste("Working Directory Set to:\n",projdir))
setwd(projdir)
#' 
#' ## Require Packages Necessary
#' 
require(readstata13)
require(foreign)
require(MCMCpack)
require(mcmcplots)
#' 
#' ## Read in Data
#' 
source(paste0(projdir,"/codes_anes/anes_dataloc.R"))
cumloc <- dataloc[dataid=="cum"]
years <- c(seq(1972,2016,4)) #, 1990
eyloc <- dataloc[dataid %in% years]
hn72loc <- dataloc[dataid=="hn72"]
sc72loc <- dataloc[dataid=="sc72"]
or08loc <- dataloc[dataid=="or08"]

#'
#' ## Import Cummulative Data
#'
cum <- read.dta13(cumloc, convert.factors = FALSE)
names(cum) <- sapply(names(cum), tolower)

#' 
#' ## Import Each Year Data
#' 
for (i in 1:length(years)){
  obj <- read.dta13(eyloc[i], convert.factors = FALSE)
  names(obj) <- sapply(names(obj), tolower)
  assign(paste("t",years[i],sep=""), obj)
}
#' 
#' ## If 2016 Data has 4271 rows:
#' 
conv2016 <- function(t2016){
  if (nrow(t2016)==4271) t2016 <- t2016[-936,]
  return(t2016)
}
t2016 <- conv2016(t2016)
#' 
#' ## Import Othe Data
#' 
hn72 <- read.csv(hn72loc, stringsAsFactors = FALSE, encoding="CP932")
sc72 <- read.csv(sc72loc, stringsAsFactors = FALSE, encoding="CP932")
or08 <- read.spss(or08loc, use.value.labels = FALSE,
                 to.data.frame=TRUE)
#' 
#' ## Create a List to Store Knowledge Index
#' 
objknlist <- vector("list", length(years)) 
names(objknlist) <- paste("yr", years, sep="")
#+ echo=FALSE
# objknlist <- readRDS(paste0(projdir,"/data/objkn_irt.rds"))
#'
#' # Function to Export IRT Score
#'
trainIRT <- function(yrdt) {
  
  # Prepare Data for IRT
  kncol <- grep("objkn_",names(yrdt))
  cat("\n")
  cat("\n")
  cat(paste("N of Knowledge Variables:", length(kncol)))
  
  select_row <- which(complete.cases(yrdt[,kncol]))
  knowdtx <- yrdt[select_row,kncol]
  objkn_add <- (rowSums(knowdtx) - mean(rowSums(knowdtx)))/sd(rowSums(knowdtx))
  rownames(knowdtx)[which.max(objkn_add)] <- "MAX_ROW"
  rownames(knowdtx)[which.min(objkn_add)] <- "MIN_ROW"
  
  start <- Sys.time(); start
  knowx.param.samples1 <- 
    MCMCirt1d(knowdtx, theta.constraints=list(MAX_ROW="+",MIN_ROW="-"), burnin = 25000,
              mcmc = 25000, thin=5, verbose = 500, seed = 123, theta.start = NA,
              alpha.start = 1, beta.start = 1, t0 = 0, T0 = 1, ab0=0, AB0=.5,
              store.item = FALSE, store.ability = TRUE,drop.constant.items=TRUE)
  knowx.param.samples2 <- 
    MCMCirt1d(knowdtx, theta.constraints=list(MAX_ROW="+",MIN_ROW="-"), burnin = 25000,
              mcmc = 25000, thin=5, verbose = 500, seed = 124, theta.start = NA,
              alpha.start = 1, beta.start = 1, t0 = 0, T0 = 1, ab0=0, AB0=.5,
              store.item = FALSE, store.ability = TRUE,drop.constant.items=TRUE)
  knowx.param.samples <- mcmc.list(knowx.param.samples1,knowx.param.samples2)
  end <- Sys.time()
  knowx.IRTtime <- end-start
  
  cat("\n")
  cat("\n")
  print(knowx.IRTtime)
  
  ## Factual Test Measure
  knowx.ability.summary <- summary(knowx.param.samples)
  
  ## Mean Tendency
  meanKnowx <- knowx.ability.summary$statistics[,1]
  #plot(density(meanKnowx))
  
  ## Check the Relationship with simple additive knowledge Variable
  cat("\n")
  cat("\n")
  cat(paste("Correlation with Additive Index:", cor(meanKnowx,objkn_add)))
  #plot(meanKnowx,objkn_add)
  
  ## Check Convergence ##
  #plot(knowx.param.samples[,1:206])
  #autocorr.plot(knowx.param.samples[,1:206])
  #geweke.diag(knowx.param.samples[,1:206])
  #gelman.plot(knowx.param.samples[,1:206])
  knowx.geweke <- geweke.diag(knowx.param.samples)
  #plot(density(knowx.geweke[[1]]$z))
  
  # Back to Data
  yrdt$objkn_irt <- NA
  yrdt$objkn_irt[select_row] <- meanKnowx
  yrdt$objkn_add <- NA
  yrdt$objkn_add[select_row] <- objkn_add
  
  # Exporting List
  out <- list(yrdt[,c("objkn_irt","objkn_add")], knowx.geweke)
  names(out) <- c("vars","geweke")
  
  # Store Data in the Result list
  return(out)
}

#'
#' # Create Knowledge Index 
#' 
#' ## 1972
#' 

# Number of Candidate in One district
n <- 0
dist <- rep(NA,nrow(hn72))
for(i in 1:nrow(hn72)){
  if (i %in% which((hn72$CD!=""| hn72$STATE!=""))){
    n <- n+1
  }
  dist[i] <- n
}
hn72$ncand <- table(dist)[match(dist,names(table(dist)))]
head(hn72)
# table(t1972$v720006) # District Code (using old state code)
sc72$conv_add <- sc72$new_id*100 - sc72$state_id*100
head(sc72)
t1972$conv_add <- sc72$conv_add[match(floor(t1972$v720006/100), sc72$state_id)]
t1972$new_distcode <- t1972$v720006 + t1972$conv_add
t1972$ncand <- hn72$ncand[match(t1972$new_distcode, floor(hn72$CODE/10))]
table(t1972$ncand, useNA="always") # NAs are those who live in DC

# 1 President Term Limit
table(t1972$v720943, useNA="always")
t1972$objkn_1 <- (t1972$v720943==2)*1
t1972$objkn_1[t1972$v720943==0] <- NA
table(t1972$objkn_1, useNA="always")
# 2 Senate Term
table(t1972$v720944, useNA="always")
t1972$objkn_2 <- (t1972$v720944==6)*1
t1972$objkn_2[t1972$v720944==0] <- NA
table(t1972$objkn_2, useNA="always")
# 3 Know House Candidate & Party 1
table(t1972$v720945, useNA="always")
table(t1972$v720946a, useNA="always")
t1972$objkn_3 <- (t1972$v720946a<99960)*1
t1972$objkn_3[t1972$v720946a==0 & t1972$v720945 %in% c(1,5,9)] <- 0
t1972$objkn_3[t1972$v720946a==0 & t1972$v720945 == 0] <- NA
table(t1972$v720947a, useNA="always") # 1=Dem, 5=Rep, 7=Other
t1972$correct_party1 <- hn72$PARTY[match(t1972$v720946a, hn72$CODE)]
table(t1972$correct_party1, useNA="always")
t1972$objkn_3[t1972$v720947a==1 & t1972$correct_party1!="D"] <- 0 
t1972$objkn_3[t1972$v720947a==5 & t1972$correct_party1!="R"] <- 0
t1972$objkn_3[t1972$v720947a==7 & t1972$correct_party1 %in% c("D","R","")] <- 0 
table(t1972$objkn_3, useNA="always")
# 4 Know House Candidate & Party 2
table(t1972$v720946b, useNA="always")
t1972$objkn_4 <- (t1972$v720946b<99960)*1
t1972$objkn_4[t1972$v720946b==0 & t1972$v720945 %in% c(1,5,9)] <- 0
t1972$objkn_4[t1972$v720946b==0 & t1972$v720945 == 0] <- NA
table(t1972$v720947b, useNA="always") # 1=Dem, 5=Rep, 7=Other
t1972$correct_party2 <- hn72$PARTY[match(t1972$v720946b, hn72$CODE)]
table(t1972$correct_party2, useNA="always")
t1972$objkn_4[t1972$v720947b==1 & t1972$correct_party2!="D"] <- 0 
t1972$objkn_4[t1972$v720947b==5 & t1972$correct_party2!="R"] <- 0
t1972$objkn_4[t1972$v720947b==7 & t1972$correct_party2 %in% c("D","R","")] <- 0 
# If there is no second candidate in the district and R know that
table(t1972$v720948, useNA="always")
t1972$objkn_4[t1972$v720948 %in% c(1,3) & t1972$ncand==1] <- 1 
table(t1972$objkn_4, useNA="always")
# 5 House Term
table(t1972$v720949, useNA="always")
t1972$objkn_5 <- (t1972$v720949==2)*1
t1972$objkn_5[t1972$v720949==0] <- NA
table(t1972$objkn_5, useNA="always")
# 6 House Majority Before Election
table(t1972$v720950, useNA="always")
t1972$objkn_6 <- (t1972$v720950==5)*1
t1972$objkn_6[t1972$v720950==0] <- NA
table(t1972$objkn_6, useNA="always")
# 7 House Majority After Election
table(t1972$v720951, useNA="always")
t1972$objkn_7 <- (t1972$v720951==5)*1
t1972$objkn_7[t1972$v720951==0] <- NA
table(t1972$objkn_7, useNA="always")

# Generate IRT Knowledge Index
#+ eval=FALSE
objknlist[["yr1972"]] <- trainIRT(t1972)
#+
# Summarize
summary(objknlist[["yr1972"]]$vars$objkn_irt)
#+results = 'asis', echo=FALSE, fig.width=6, fig.height=4
plot(density(objknlist[["yr1972"]]$vars$objkn_irt, na.rm=TRUE))

#'
#' ## 1976 (Not Enough Factual Items)
#' 

# House Majority Party Before Election
table(t1976$v763683, useNA="always")
t1976$objkn_1 <- (t1976$v763683==5)*1
t1976$objkn_1[t1976$v763683==0] <- NA
# House Majority Party After Election
table(t1976$v763684, useNA="always")
t1976$objkn_2 <- (t1976$v763684==5)*1
t1976$objkn_2[t1976$v763684==0] <- NA

# Generate IRT Knowledge Index
#+ eval=FALSE
objknlist[["yr1976"]] <- trainIRT(t1976)
#+
# Summarize
summary(objknlist[["yr1976"]]$vars$objkn_irt)
#+results = 'asis', echo=FALSE, fig.width=6, fig.height=4
plot(density(objknlist[["yr1976"]]$vars$objkn_irt, na.rm=TRUE))

#'
#' ## 1980
#' 

# House Majoirity Party Before Election
table(t1980$v801028, useNA="always") # 5 Dem
t1980$objkn_1 <- (t1980$v801028==5)*1
t1980$objkn_1[t1980$v801028==9] <- NA
table(t1980$objkn_1, useNA="always")
# House Majoirity Party After Election
table(t1980$v801029, useNA="always") # 1 Rep
t1980$objkn_2 <- (t1980$v801029==1)*1
t1980$objkn_2[t1980$v801029==9] <- NA
table(t1980$objkn_2, useNA="always")
# Recall House Candidate 1
table(t1980$v800823, useNA="always") # 1 know 5 DK
table(t1980$v800826, useNA="always") # 1 correct
t1980$objkn_3 <- (t1980$v800826==1)*1
t1980$objkn_3[t1980$v800823==9] <- NA
table(t1980$objkn_3, useNA="always")
# Recall House Candidate 2
table(t1980$v800823, useNA="always") # 1 know 5/9 DK
table(t1980$v800829, useNA="always") # 1 correct
t1980$objkn_4 <- (t1980$v800829==1)*1
t1980$objkn_4[t1980$v800823==9] <- NA
table(t1980$objkn_4, useNA="always")
# Know Incumbency Status (Extract Correct Answers from Cummulative Data)
table(cum[cum$vcf0004==1980,]$vcf0977) # 5 is don't know
table(cum[cum$vcf0004==1980,]$vcf0978, t1980$v800913)
t1980$objkn_5 <- (cum[cum$vcf0004==1980,]$vcf0978==1)*1
t1980$objkn_5[cum[cum$vcf0004==1980,]$vcf0977==5] <- 0
t1980$objkn_5[cum[cum$vcf0004==1980,]$vcf0977==9] <- NA
table(t1980$objkn_5, useNA="always")

# Generate IRT Knowledge Index
#+ eval=FALSE
objknlist[["yr1980"]] <- trainIRT(t1980)
#+
# Summarize
summary(objknlist[["yr1980"]]$vars$objkn_irt)
#+results = 'asis', echo=FALSE, fig.width=6, fig.height=4
plot(density(objknlist[["yr1980"]]$vars$objkn_irt, na.rm=TRUE))

#'
#' ## 1984
#' 

# House Majoirity Party Before Election
table(t1984$v841006, useNA="always") # 5 Dem
t1984$objkn_1 <- (t1984$v841006==5)*1
t1984$objkn_1[t1984$v841006==9] <- NA
table(t1984$objkn_1, useNA="always")
# House Majoirity Party After Election
table(t1984$v841007, useNA="always") # 5 Dem
t1984$objkn_2 <- (t1984$v841007==5)*1
t1984$objkn_2[t1984$v841007==9] <- NA
table(t1984$objkn_2, useNA="always")
# Recall House Candidate 1
table(t1984$v840737, useNA="always") # 1 know 5 DK
table(t1984$v840741, useNA="always") # 1 correct
t1984$objkn_3 <- (t1984$v840741==1)*1
t1984$objkn_3[t1984$v840737==9] <- NA
table(t1984$objkn_3, useNA="always")
# Recall House Candidate 2
table(t1984$v840737, useNA="always") # 1 know 5/9 DK
table(t1984$v840745, useNA="always") # 1 correct
t1984$objkn_4 <- (t1984$v840745==1)*1
t1984$objkn_4[t1984$v840737==9] <- NA
table(t1984$objkn_4, useNA="always")
# Know Incumbency Status (Extract Correct Answers from Cummulative Data)
table(cum[cum$vcf0004==1984,]$vcf0977) # 5 is don't know
table(cum[cum$vcf0004==1984,]$vcf0978) # v840741
t1984$objkn_5 <- (cum[cum$vcf0004==1984,]$vcf0978==1)*1
t1984$objkn_5[cum[cum$vcf0004==1984,]$vcf0977==5] <- 0
t1984$objkn_5[cum[cum$vcf0004==1984,]$vcf0977==9] <- NA
table(t1984$objkn_5, useNA="always")

# Generate IRT Knowledge Index
#+ eval=FALSE
objknlist[["yr1984"]] <- trainIRT(t1984)
#+
# Summarize
summary(objknlist[["yr1984"]]$vars$objkn_irt)
#+results = 'asis', echo=FALSE, fig.width=6, fig.height=4
plot(density(objknlist[["yr1984"]]$vars$objkn_irt, na.rm=TRUE))


#'
#' ## 1988
#' 

## Party with House Majority Before the Election (Post)
# 1988: 880878 
table(t1988$v880878, useNA="always") # 5 Dem
t1988$objkn_1 <- (t1988$v880878==5)*1
t1988$objkn_1[t1988$v880878==0] <- NA
table(t1988$objkn_1, useNA="always")
## Recall House Candidate Names
# 1988: 880565 
## Recall 1st House Candidate Names Correctly 
# 1988: 880569 
table(t1988$v880565, useNA="always") # 1 know 5 DK
table(t1988$v880569, useNA="always") # 1 correct
t1988$objkn_2 <- (t1988$v880569==1)*1
t1988$objkn_2[t1988$v880565==0] <- NA
table(t1988$objkn_2, useNA="always")
## Recall 2nd House Candidate Names Correctly 
# 1988: 880573 
table(t1988$v880565, useNA="always") # 1 know 5 DK
table(t1988$v880573, useNA="always") # 1 correct
t1988$objkn_3 <- (t1988$v880573==1)*1
t1988$objkn_3[t1988$v880565==0] <- NA
table(t1988$objkn_3, useNA="always")
## Party with Senate Majority Before the Election (Post)
table(t1988$v880879, useNA="always") # 5 Dem
t1988$objkn_4 <- (t1988$v880879==5)*1
t1988$objkn_4[t1988$v880879==0] <- NA
table(t1988$objkn_4, useNA="always")
# Know Incumbency Status (Extract Correct Answers from Cummulative Data)
# 1988: 880709,880712 
table(cum[cum$vcf0004==1988,]$vcf0977) # 5 is don't know
table(cum[cum$vcf0004==1988,]$vcf0978) 
t1988$objkn_5 <- (cum[cum$vcf0004==1988,]$vcf0978==1)*1
t1988$objkn_5[cum[cum$vcf0004==1988,]$vcf0977==5] <- 0
t1988$objkn_5[cum[cum$vcf0004==1988,]$vcf0977==9] <- NA
table(t1988$objkn_5, useNA="always")
# Office Recognition 1 (Ted Kennedy)
table(t1988$v880871, useNA="always")
t1988$objkn_o1 <- (t1988$v880871==1)*1
t1988$objkn_o1[t1988$v880871==0] <- NA
# Office Recognition 2 (GEORGE SCHULTZ)
table(t1988$v880872, useNA="always")
t1988$objkn_o2 <- (t1988$v880872==1)*1
t1988$objkn_o2[t1988$v880872==0] <- NA
# Office Recognition 3 (WILLIAM REHNQUIST)
table(t1988$v880873, useNA="always")
t1988$objkn_o3 <- (t1988$v880873==1)*1
t1988$objkn_o3[t1988$v880873==0] <- NA
# Office Recognition 4 (MIKHAIL GORBACHEV)
table(t1988$v880874, useNA="always")
t1988$objkn_o4 <- (t1988$v880874==1)*1
t1988$objkn_o4[t1988$v880874==0] <- NA
# Office Recognition 5 (MARGARET THATCHER)
table(t1988$v880875, useNA="always")
t1988$objkn_o5 <- (t1988$v880875==1)*1
t1988$objkn_o5[t1988$v880875==0] <- NA
# Office Recognition 6 (YASSER ARAFAT)
table(t1988$v880876, useNA="always")
t1988$objkn_o6 <- (t1988$v880876==1)*1
t1988$objkn_o6[t1988$v880876==0] <- NA
# Office Recognition 7 (JIM WRIGHT)
table(t1988$v880877, useNA="always")
t1988$objkn_o7 <- (t1988$v880877==1)*1
t1988$objkn_o7[t1988$v880877==0] <- NA

# Generate IRT Knowledge Index
#+ eval=FALSE
objknlist[["yr1988"]] <- trainIRT(t1988)
#+
# Summarize
summary(objknlist[["yr1988"]]$vars$objkn_irt)
#+results = 'asis', echo=FALSE, fig.width=6, fig.height=4
plot(density(objknlist[["yr1988"]]$vars$objkn_irt, na.rm=TRUE))

#'
#' ## 1992
#' 

## Party with House Majority Before the Election (Post)
table(t1992$v925951, useNA="always") # 5 Dem
t1992$objkn_1 <- (t1992$v925951==5)*1
t1992$objkn_1[t1992$v925951==0] <- NA
table(t1992$objkn_1, useNA="always")
## Recall 1st House Candidate Names Correctly 
table(t1992$v925109, useNA="always") # 1 know 5 DK
table(t1992$v925113, useNA="always") # 1 correct
t1992$objkn_2 <- (t1992$v925113==1)*1
t1992$objkn_2[t1992$v925109==0] <- NA
table(t1992$objkn_2, useNA="always")
## Recall 2nd House Candidate Names Correctly 
table(t1992$v925109, useNA="always") # 1 know 5 DK
table(t1992$v925117, useNA="always") # 1 correct
t1992$objkn_3 <- (t1992$v925117==1)*1
t1992$objkn_3[t1992$v925109==0] <- NA
table(t1992$objkn_3, useNA="always")
## Party with Senate Majority Before the Election (Post)
table(t1992$v925952, useNA="always") # 5 Dem
t1992$objkn_4 <- (t1992$v925952==5)*1
t1992$objkn_4[t1992$v925952==0] <- NA
table(t1992$objkn_4, useNA="always")
# Know Incumbency Status (Extract Correct Answers from Cummulative Data)
table(cum[cum$vcf0004==1992,]$vcf0977) # 5 is don't know
table(cum[cum$vcf0004==1992,]$vcf0978) 
t1992$objkn_5 <- (cum[cum$vcf0004==1992,]$vcf0978==1)*1
t1992$objkn_5[cum[cum$vcf0004==1992,]$vcf0977==5] <- 0
t1992$objkn_5[cum[cum$vcf0004==1992,]$vcf0977==9] <- NA
table(t1992$objkn_5, useNA="always")
# Office Recognition 1 (DAN QUAYLE) 
table(t1992$v925916, useNA="always")
t1992$objkn_o1 <- (t1992$v925916==1)*1
t1992$objkn_o1[t1992$v925916==0] <- NA
# Office Recognition 2 (William Rehnquist) 
table(t1992$v925917, useNA="always")
t1992$objkn_o2 <- (t1992$v925917==1)*1
t1992$objkn_o2[t1992$v925917==0] <- NA
# Office Recognition 3 (Boris Yeltsin) 
table(t1992$v925918, useNA="always")
t1992$objkn_o3 <- (t1992$v925918==1)*1
t1992$objkn_o3[t1992$v925918==0] <- NA
# Office Recognition 4 (Tom Foley) 
table(t1992$v925919, useNA="always")
t1992$objkn_o4 <- (t1992$v925919==1)*1
t1992$objkn_o4[t1992$v925919==0] <- NA
# Constitutionality Responsibility 
table(t1992$v925920, useNA="always") # 3 Supreme Court
t1992$objkn_co <- (t1992$v925920==3)*1
t1992$objkn_co[t1992$v925920==0] <- NA
# NOminate Judge Responsibility
table(t1992$v925921, useNA="always") # 1 President
t1992$objkn_no <- (t1992$v925921==1)*1
t1992$objkn_no[t1992$v925921==0] <- NA

# Generate IRT Knowledge Index
#+ eval=FALSE
objknlist[["yr1992"]] <- trainIRT(t1992)
#+
# Summarize
summary(objknlist[["yr1992"]]$vars$objkn_irt)
#+results = 'asis', echo=FALSE, fig.width=6, fig.height=4
plot(density(objknlist[["yr1992"]]$vars$objkn_irt, na.rm=TRUE))

#'
#' ## 1996
#' 

## Party with House Majority Before the Election (Post)
table(t1996$v961072, useNA="always") # 5 Rep
t1996$objkn_1 <- (t1996$v961072==5)*1
t1996$objkn_1[t1996$v961072==0] <- NA
table(t1996$objkn_1, useNA="always")
## Recall 1st House Candidate Names Correctly 
table(t1996$v961006, useNA="always") # 1 know 5 DK
table(t1996$v961010, useNA="always") # 1 correct
t1996$objkn_2 <- (t1996$v961010==1)*1
t1996$objkn_2[t1996$v961006==0] <- NA
table(t1996$objkn_2, useNA="always")
## Recall 2nd House Candidate Names Correctly 
table(t1996$v961006, useNA="always") # 1 know 5 DK
table(t1996$v961014, useNA="always") # 1 correct
t1996$objkn_3 <- (t1996$v961014==1)*1
t1996$objkn_3[t1996$v961006==0] <- NA
table(t1996$objkn_3, useNA="always")
## Party with Senate Majority Before the Election (Post)
table(t1996$v961073, useNA="always") # 5 Rep
t1996$objkn_4 <- (t1996$v961073==5)*1
t1996$objkn_4[t1996$v961073==0] <- NA
table(t1996$objkn_4, useNA="always")
# Know Incumbency Status (Extract Correct Answers from Cummulative Data)
table(cum[cum$vcf0004==1996,]$vcf0977) # 5 is don't know
table(cum[cum$vcf0004==1996,]$vcf0978) 
t1996$objkn_5 <- (cum[cum$vcf0004==1996,]$vcf0978==1)*1
t1996$objkn_5[cum[cum$vcf0004==1996,]$vcf0977==5] <- 0
t1996$objkn_5[cum[cum$vcf0004==1996,]$vcf0977==9] <- NA
table(t1996$objkn_5, useNA="always")
# Office Recognition 1 (Al Gore) 
table(t1996$v961189, useNA="always")
t1996$objkn_o1 <- (t1996$v961189==1)*1
t1996$objkn_o1[t1996$v961189==0] <- NA
# Office Recognition 2 (William Rehnquist) 
table(t1996$v961190, useNA="always")
t1996$objkn_o2 <- (t1996$v961190==1)*1
t1996$objkn_o2[t1996$v961190==0] <- NA
# Office Recognition 3 (Boris Yeltsin) 
table(t1996$v961191, useNA="always")
t1996$objkn_o3 <- (t1996$v961191==1)*1
t1996$objkn_o3[t1996$v961191==0] <- NA
# Office Recognition 4 (Newt Gingrich) 
table(t1996$v961192, useNA="always")
t1996$objkn_o4 <- (t1996$v961192==1)*1
t1996$objkn_o4[t1996$v961192==0] <- NA

# Generate IRT Knowledge Index
#+ eval=FALSE
objknlist[["yr1996"]] <- trainIRT(t1996)
#+
# Summarize
summary(objknlist[["yr1996"]]$vars$objkn_irt)
#+results = 'asis', echo=FALSE, fig.width=6, fig.height=4
plot(density(objknlist[["yr1996"]]$vars$objkn_irt, na.rm=TRUE))

#'
#' ## 2000
#' 

## Party with House Majority Before the Election (Post)
table(t2000$v001356, useNA="always") # 5 Rep
t2000$objkn_1 <- (t2000$v001356==5)*1
t2000$objkn_1[t2000$v001356==0] <- NA
table(t2000$objkn_1, useNA="always")
## Recall 1st House Candidate Names Correctly 
table(t2000$v001206, useNA="always") # 1 know 5 DK
table(t2000$v001210, useNA="always") # 1 correct
t2000$objkn_2 <- (t2000$v001210==1)*1
t2000$objkn_2[t2000$v001206==0] <- NA
table(t2000$objkn_2, useNA="always")
## Recall 2nd House Candidate Names Correctly 
table(t2000$v001206, useNA="always") # 1 know 5 DK
table(t2000$v001214, useNA="always") # 1 correct
t2000$objkn_3 <- (t2000$v001214==1)*1
t2000$objkn_3[t2000$v001206==0] <- NA
table(t2000$objkn_3, useNA="always")
## Party with Senate Majority Before the Election (Post)
table(t2000$v001357, useNA="always") # 5 Rep
t2000$objkn_4 <- (t2000$v001357==5)*1
t2000$objkn_4[t2000$v001357==0] <- NA
table(t2000$objkn_4, useNA="always")
# Know Incumbency Status (Extract Correct Answers from Cummulative Data)
table(cum[cum$vcf0004==2000,]$vcf0977) # 5 is don't know
table(cum[cum$vcf0004==2000,]$vcf0978) 
t2000$objkn_5 <- (cum[cum$vcf0004==2000,]$vcf0978==1)*1
t2000$objkn_5[cum[cum$vcf0004==2000,]$vcf0977==5] <- 0
t2000$objkn_5[cum[cum$vcf0004==2000,]$vcf0977==9] <- NA
table(t2000$objkn_5, useNA="always")
# Office Recognition 1 (Trent Lott) 
table(t2000$v001447, useNA="always")
t2000$objkn_o1 <- (t2000$v001447==1)*1
t2000$objkn_o1[t2000$v001447==0] <- NA
# Office Recognition 2 (William Rehnquist) 
table(t2000$v001450, useNA="always")
t2000$objkn_o2 <- (t2000$v001450==1)*1
t2000$objkn_o2[t2000$v001450==0] <- NA
# Office Recognition 3 (Boris Yeltsin) 
table(t2000$v001453, useNA="always")
t2000$objkn_o3 <- (t2000$v001453==1)*1
t2000$objkn_o3[t2000$v001453==0] <- NA
# Office Recognition 4 (Newt Gingrich) 
table(t2000$v001456, useNA="always")
t2000$objkn_o4 <- (t2000$v001456==1)*1
t2000$objkn_o4[t2000$v001456==0] <- NA

# Generate IRT Knowledge Index
#+ eval=FALSE
objknlist[["yr2000"]] <- trainIRT(t2000)
#+
# Summarize
summary(objknlist[["yr2000"]]$vars$objkn_irt)
#+results = 'asis', echo=FALSE, fig.width=6, fig.height=4
plot(density(objknlist[["yr2000"]]$vars$objkn_irt, na.rm=TRUE))

#'
#' ## 2004
#' 

## Party with House Majority Before the Election (Post)
table(t2004$v045089, useNA="always") # 5 Rep
t2004$objkn_1 <- (t2004$v045089==5)*1
t2004$objkn_1[t2004$v045089==0] <- NA
table(t2004$objkn_1, useNA="always")
## Party with Senate Majority Before the Election (Post)
table(t2004$v045090, useNA="always") # 5 Rep
t2004$objkn_4 <- (t2004$v045090==5)*1
t2004$objkn_4[t2004$v045090==0] <- NA
table(t2004$objkn_4, useNA="always")
# Office Recognition 1 (Dennis Hastert) 
table(t2004$v045162, useNA="always")
t2004$objkn_o1 <- (t2004$v045162==1)*1
t2004$objkn_o1[t2004$v045162==0] <- NA
# Office Recognition 2 (Dick Cheney)
table(t2004$v045163, useNA="always")
t2004$objkn_o2 <- (t2004$v045163==1)*1
t2004$objkn_o2[t2004$v045163==0] <- NA
# Office Recognition 3 (Tony Blair)
table(t2004$v045164, useNA="always")
t2004$objkn_o3 <- (t2004$v045164==1)*1
t2004$objkn_o3[t2004$v045164==0] <- NA
# Office Recognition 4 (William Rehnquist) 
table(t2004$v045165, useNA="always")
t2004$objkn_o4 <- (t2004$v045165==1)*1
t2004$objkn_o4[t2004$v045165==0] <- NA


# Generate IRT Knowledge Index
#+ eval=FALSE
objknlist[["yr2004"]] <- trainIRT(t2004)
#+
# Summarize
summary(objknlist[["yr2004"]]$vars$objkn_irt)
#+results = 'asis', echo=FALSE, fig.width=6, fig.height=4
plot(density(objknlist[["yr2004"]]$vars$objkn_irt, na.rm=TRUE))

#'
#' ## 2008
#' 

## Party with House Majority Before the Election (Post)
table(t2008$v085066, useNA="always") # 1 Dem
t2008$objkn_1 <- (t2008$v085066==1)*1
t2008$objkn_1[t2008$v085066==-2] <- NA
table(t2008$objkn_1, useNA="always")
## Party with Senate Majority Before the Election (Post)
table(t2008$v085067, useNA="always") # 1 Dem
t2008$objkn_4 <- (t2008$v085067==1)*1
t2008$objkn_4[t2008$v085067==-2] <- NA
table(t2008$objkn_4, useNA="always")
# Office Recognition 1 (NANCY PELOSI) 
table(t2008$v085120, useNA="always")
table(or08$PELOSI_Level1, useNA="always")
t2008$objkn_o1 <- or08$PELOSI_Level1[match(t2008$v080001, or08$ID)]
table(t2008$objkn_o1, useNA="always")
# Office Recognition 2 (DICK CHENEY)
table(t2008$v085121, useNA="always")
table(or08$CHENEY_Level1, useNA="always")
t2008$objkn_o2 <- or08$CHENEY_Level1[match(t2008$v080001, or08$ID)]
table(t2008$objkn_o2, useNA="always")
# Office Recognition 3 (GORDON BROWN)
table(t2008$v085122, useNA="always")
table(or08$BROWN_Level1, useNA="always")
t2008$objkn_o3 <- or08$BROWN_Level1[match(t2008$v080001, or08$ID)]
table(t2008$objkn_o3, useNA="always")
# Office Recognition 4 (JOHN ROBERTS) 
table(t2008$v085123, useNA="always")
table(or08$ROBERTS_Level1, useNA="always")
t2008$objkn_o4 <- or08$ROBERTS_Level1[match(t2008$v080001, or08$ID)]
table(t2008$objkn_o4, useNA="always")

# Generate IRT Knowledge Index
#+ eval=FALSE
objknlist[["yr2008"]] <- trainIRT(t2008)
#+
# Summarize
summary(objknlist[["yr2008"]]$vars$objkn_irt)
#+results = 'asis', echo=FALSE, fig.width=6, fig.height=4
plot(density(objknlist[["yr2008"]]$vars$objkn_irt, na.rm=TRUE))

#'
#' ## 2012
#' 

## President Term Limit
table(t2012$preknow_prestimes, useNA="always")
t2012$objkn_0 <- (t2012$preknow_prestimes==2)*1
table(t2012$objkn_0, useNA="always")
## Federal Deficit Bigger than 1990s
table(t2012$preknow_sizedef, useNA="always")
t2012$objkn_01 <- (t2012$preknow_sizedef==1)*1
table(t2012$objkn_01, useNA="always")
## Senate Term
table(t2012$preknow_senterm, useNA="always")
t2012$objkn_02 <- (t2012$preknow_senterm==6)*1
table(t2012$objkn_02, useNA="always")
## What is medicare
table(t2012$preknow_medicare, useNA="always")
t2012$objkn_03 <- (t2012$preknow_medicare==1)*1
table(t2012$objkn_03, useNA="always")
## Least Spending (Foreign Aid)
table(t2012$preknow_leastsp, useNA="always")
t2012$objkn_04 <- (t2012$preknow_leastsp==1)*1
table(t2012$objkn_04, useNA="always")
## Party with House Majority Before the Election (Post)
table(t2012$knowl_housemaj, useNA="always") # 2 Rep
t2012$objkn_1 <- (t2012$knowl_housemaj==2)*1
t2012$objkn_1[t2012$knowl_housemaj %in% c(-6,-7)] <- NA
table(t2012$objkn_1, useNA="always")
## Party with Senate Majority Before the Election (Post)
table(t2012$knowl_senmaj, useNA="always") # 1 Dem
t2012$objkn_4 <- (t2012$knowl_senmaj==1)*1
t2012$objkn_4[t2012$knowl_senmaj %in% c(-6,-7)] <- NA
table(t2012$objkn_4, useNA="always")
# Office Recognition 1 (John Boehner) 
table(t2012$ofcrec_speaker_correct, useNA="always")
t2012$objkn_o1 <- (t2012$ofcrec_speaker_correct==1)*1
t2012$objkn_o1[t2012$ofcrec_speaker_correct %in% c(-6,-7)] <- NA
# Office Recognition 2 (Joe Biden)
table(t2012$ofcrec_vp_correct, useNA="always")
t2012$objkn_o2 <- (t2012$ofcrec_vp_correct==1)*1
t2012$objkn_o2[t2012$ofcrec_vp_correct %in% c(-6,-7)] <- NA
# Office Recognition 3 (David Cameron)
table(t2012$ofcrec_pmuk_correct, useNA="always")
t2012$objkn_o3 <- (t2012$ofcrec_pmuk_correct==1)*1
t2012$objkn_o3[t2012$ofcrec_pmuk_correct %in% c(-6,-7)] <- NA
# Office Recognition 4 (JOHN ROBERTS)
table(t2012$ofcrec_cj_correct, useNA="always")
t2012$objkn_o4 <- (t2012$ofcrec_cj_correct>=0.5)*1
t2012$objkn_o4[t2012$ofcrec_cj_correct %in% c(-6,-7)] <- NA
table(t2012$objkn_o4, useNA="always")

# Generate IRT Knowledge Index
#+ eval=FALSE
objknlist[["yr2012"]] <- trainIRT(t2012)
#+
# Summarize
summary(objknlist[["yr2012"]]$vars$objkn_irt)
#+results = 'asis', echo=FALSE, fig.width=6, fig.height=4
plot(density(objknlist[["yr2012"]]$vars$objkn_irt, na.rm=TRUE))

#'
#' ## 2016
#' 

## Party with House Majority Before the Election (Post)
table(t2016$v161515, useNA="always") # 2 Rep
t2016$objkn_1 <- (t2016$v161515==2)*1
t2016$objkn_1[t2016$v161515 %in% c(-5)] <- NA
table(t2016$objkn_1, useNA="always")
## Party with Senate Majority Before the Election (Post)
table(t2016$v161516, useNA="always") # 2 Rep
t2016$objkn_4 <- (t2016$v161516==2)*1
t2016$objkn_4[t2016$v161516 %in% c(-5)] <- NA
table(t2016$objkn_4, useNA="always")
# Office Recognition 1 (Joe Biden) 
table(t2016$v162072, useNA="always")
t2016$objkn_o1 <- (t2016$v162072==1)*1
t2016$objkn_o1[t2016$v162072 %in% c(-6,-7)] <- NA
# Office Recognition 2 (Paul Ryan)
table(t2016$v162073a, useNA="always")
t2016$objkn_o2 <- (t2016$v162073a==1)*1
t2016$objkn_o2[t2016$v162073a %in% c(-6,-7)] <- NA
# Office Recognition 3 (Angela Merkel)
table(t2016$v162074a, useNA="always")
t2016$objkn_o3 <- (t2016$v162074a==1)*1
t2016$objkn_o3[t2016$v162074a %in% c(-6,-7)] <- NA
# Office Recognition 4 (Vladimir Putin)
table(t2016$v162075a, useNA="always")
t2016$objkn_o4 <- (t2016$v162075a==1)*1
t2016$objkn_o4[t2016$v162075a %in% c(-6,-7)] <- NA
# Office Recognition 5 (John Roberts) 
table(t2016$v162076a, useNA="always")
t2016$objkn_o5 <- (t2016$v162076a>=0.5)*1
t2016$objkn_o5[t2016$v162076a %in% c(-6,-7)] <- NA
table(t2016$objkn_o5, useNA="always")

# Generate IRT Knowledge Index
#+ eval=FALSE
objknlist[["yr2016"]] <- trainIRT(t2016)
#+
# Summarize
summary(objknlist[["yr2016"]]$vars$objkn_irt)
#+results = 'asis', echo=FALSE, fig.width=6, fig.height=4
plot(density(objknlist[["yr2016"]]$vars$objkn_irt, na.rm=TRUE))

#'
#' # Save Data 
#' 
#+ eval=FALSE
saveRDS(objknlist, paste0(projdir,"/data/objkn_irt.rds"))

#+ eval=FALSE, echo=FALSE
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# rmarkdown::render("create_objkn_irt.R",
#                   "pdf_document")
