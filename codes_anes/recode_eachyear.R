#' ---
#' title: "Recoding ANES Yearly Data"
#' author: "Gento Kato"
#' date: "November 7, 2018"
#' ---

#' 
#' # Preparation
#' 
#' ## Require Packages Necessary
#' 
require(readstata13)
require(foreign)
#' 
#' ## Read in Data
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # (only in RStudio)
#+ echo=FALSE
# source("./anes_dataloc.R")
# source("./recode_cummulative.R")
#+
years <- c(seq(1972,2016,4),1990)
eyloc <- dataloc[dataid %in% years]
occ08loc <- dataloc[dataid=="occ08"]
occ12loc <- dataloc[dataid=="occ12"]
objknloc <- dataloc[dataid=="objkn"]
#' 
#' ## Import Each Year Data
#' 
for (i in 1:length(years)){
  obj <- read.dta13(eyloc[i], convert.factors = FALSE)
  names(obj) <- sapply(names(obj), tolower)
  assign(paste("t",years[i],sep=""), obj)
}
#' 
#' ## Import Occupation data for 2008
#' 
occ08 <- read.spss(occ08loc, use.value.labels = FALSE,
                   to.data.frame=TRUE)
#' 
#' ## Import Predicted Occupation Data for 2012 
#' 
occ12 <- read.csv(occ12loc, stringsAsFactors = FALSE)
#' 
#' ## Import IRT Objective Knowledge Scales
#' 
objknlist <- readRDS(objknloc)
#' 
#' ## 2016 Data Adjustment
#' 
#' The Cases of 2016 is one fewer in the cummulative dataset (Nov 2/2018 Version)
#' 
conv2016 <- function(t2016){
  if (nrow(t2016)-nrow(d[d$year==2016,])==1){
    print(length(d$age[d$year==2016]))
    print(length(t2016$v161267))
    # Find missing case by matching age
    print(cbind(c(d$age[d$year==2016],NA),t2016$v161267,
                c(d$age[d$year==2016],NA)-t2016$v161267)[931:945,1:3])
    # Drop the suspected row
    t2016 <- t2016[-936,]
    # Check again
    print(cbind(d$age[d$year==2016],t2016$v161267,
          d$age[d$year==2016]-t2016$v161267)[931:945,1:3])
  } 
  return(t2016)
}
t2016 <- conv2016(t2016)
#' 
#' # Adjust Religion Variables (only 2016)
#' 

# Variable Missing in 2016
table(d[d$year==2016,]$prot, useNA="always")
table(d[d$year==2016,]$cath, useNA="always")
table(d[d$year==2016,]$jew, useNA="always")
# Substitute in 2016
table(t2016$v161247a, useNA="always") # Major Religious Group (Attending Church)
table(t2016$v161247b, useNA="always") # Major Religious Group (Not Attending Church)
t2016$relig <- t2016$v161247a
t2016$relig[t2016$v161247a==-1] <- t2016$v161247b[t2016$v161247a==-1]
table(t2016$relig, useNA="always")
t2016$relig[t2016$relig %in% c(-8,-9)] <- NA
# Assign Variable in Cummulative Dataset
d[d$year==2016,]$prot <- (t2016$relig==1)*1
table(d[d$year==2016,]$prot, useNA="always")
d[d$year==2016,]$cath <- (t2016$relig==2)*1
table(d[d$year==2016,]$cath, useNA="always")
d[d$year==2016,]$jew <- (t2016$relig==3)*1
table(d[d$year==2016,]$jew, useNA="always")

#'
#' ## Mode Data 
#'

# for 1972
d$mode_1972 <- NA
d[d$year==1972,]$mode_1972 <- t1972$v720438
table(d[d$year==1972,]$mode_1972)
# Mode 3 & 4 do not have ideology question
# Mode 2 & 3 & 4 do not have objective knowlede question 

# for 1992 (post survey)
d$mode_1992 <- NA
d[d$year==1992,]$mode_1992 <- t1992$v925002
table(d[d$year==1992,]$mode_1992)
# Mode 3&4 (Short Form) do not have objective knowledge question

# Define Dropping Variable
d$dropalways <- 0
d$dropalways[d$mode==4|d$mode_1972%in%c(3,4)] <- 1
d$dropifobjkn <- d$dropalways
d$dropifobjkn[d$mode_1972%in%c(2,3,4)|d$mode_1992%in%c(3,4)] <- 1

#'
#' ## Ideology 
#'

# 1972, 1976, 1980, 1984
# Those who haven't though much about or DK in self ideology (0 or 8) are not 
# asked to answer candidates' answers. Recode to zero
table(t1972$v720652, d[d$year==1972,]$idedem, useNA="always")
table(t1972$v720652, d[d$year==1972,]$iderep, useNA="always")
d[d$year==1972,]$idedem[t1972$v720652%in%c(0,8)] <- 0
d[d$year==1972,]$iderep[t1972$v720652%in%c(0,8)] <- 0
table(t1976$v763286, d[d$year==1976,]$idedem, useNA="always")
table(t1976$v763286, d[d$year==1976,]$iderep, useNA="always")
d[d$year==1976,]$idedem[t1976$v763286%in%c(0,8)] <- 0
d[d$year==1976,]$iderep[t1976$v763286%in%c(0,8)] <- 0
table(t1980$v800267, d[d$year==1980,]$idedem, useNA="always")
table(t1980$v800267, d[d$year==1980,]$iderep, useNA="always")
d[d$year==1980,]$idedem[t1980$v800267%in%c(0,8)] <- 0
d[d$year==1980,]$iderep[t1980$v800267%in%c(0,8)] <- 0
table(t1984$v840370, d[d$year==1984,]$idedem, useNA="always")
table(t1984$v840370, d[d$year==1984,]$iderep, useNA="always")
d[d$year==1984,]$idedem[t1984$v840370%in%c(0,8)] <- 0
d[d$year==1984,]$iderep[t1984$v840370%in%c(0,8)] <- 0


# 2000
table(t2000$v000440,d[d$year==2000,]$ideself, useNA="always")
table(t2000$v000446,d[d$year==2000,]$ideself, useNA="always")
d[d$year==2000,]$ideself <- ifelse(t2000$v000446==0,NA,
                                   ifelse(t2000$v000446%in%c(8,9),0,
                                          t2000$v000446-4))
table(d[d$year==2000,]$ideself, useNA="always") 

# 2016 (The cummulative data is messed up)
table(t2016$v161126, d[d$year==2016,]$ideself, useNA="always") # Pre question does not work
table(t2016$v161127, t2016$v161126, useNA="always")
# d[d$year==2016,]$ideself <- ifelse(t2016$v161126==-9,NA,
#                                    ifelse(t2016$v161126%in%c(99,-8),0,
#                                           t2016$v161126-4))
# d[d$year==2016,]$ideself[t2016$v161127==1] <- -1
# d[d$year==2016,]$ideself[t2016$v161127==2] <- 0
# d[d$year==2016,]$ideself[t2016$v161127==3] <- 1
# d[d$year==2016,]$ideself[t2016$v161127%in%c(-8,-9)] <- -0
table(t2016$v162171, d[d$year==2016,]$ideself, useNA="always") # Post Question is used in cummulative
table(t2016$v162171a, d[d$year==2016,]$ideself, useNA="always")
d[d$year==2016,]$ideself[t2016$v162171a==1] <- -1
d[d$year==2016,]$ideself[t2016$v162171a==2] <- 0
d[d$year==2016,]$ideself[t2016$v162171a==3] <- 1
d[d$year==2016,]$ideself[t2016$v162171a%in%c(-8,-9)] <- -0
table(d[d$year==2016,]$ideself, useNA="always") 

### Ideological Alignment (Republican Advantage)
d$iderepadv <- -(abs(d$ideself-d$iderep) - abs(d$ideself-d$idedem))
table(d$iderepadv, useNA="always")
d$iderepadvsq <- -(abs(d$ideself-d$iderep)^2 - abs(d$ideself-d$idedem)^2)
table(d$iderepadvsq, useNA="always")

#' 
#' ## Retrospective Economy (1972 & 76)
#' 
# 1972 (Two Questionaires)
table(t1972$v720780, t1972$v720507, useNA="always")
d[d$year==1972,]$evecon <- ifelse(t1972$v720780==0,
                                  ifelse(t1972$v720507==0,NA,
                                         ifelse(t1972$v720507%in%c(8,9),0,
                                                (3-t1972$v720507)*0.75)),
                                  ifelse(t1972$v720780%in%c(8,9),
                                         0,(4-t1972$v720780)))
d[d$year==1972,]$evecon[t1972$v720780==7] <- -2
d[d$year==1972,]$evecon[t1972$v720780==1] <- 2
table(d[d$year==1972,]$evecon, useNA="always")
# 1976
table(t1976$v763139, useNA="always")
d[d$year==1976,]$evecon <- ifelse(t1976$v763139==9,NA,
                                  ifelse(t1976$v763139==8,0,
                                         (3-t1976$v763139)*0.75))
table(d[d$year==1976,]$evecon, useNA="always")

#' 
#' # Adjust Income
#' 

table(d$inc, useNA="always")

# List of Relevant Variables
# 1972: 720420 
# 1976: 763507 
# 1980: 800686 
# 1984: 840680 
# 1988: 880520 
# 1992: 924104 
# 1996: 960701
# 2000: 000994 
# 2004: 043293x 
# 2008: 083248x
# 2012: incgroup_prepost_x 
# 2016: V161361x V162309x 

# Percentile Conversion Function
convper <- function(old.var,missing.val){
  r <- old.var
  r[r %in% missing.val] <- NA
  rt <- cumsum(table(r)/sum(table(r))) # Cummulative Percentile
  rt <- rt - diff(c(0,rt))/2 # Take Midpoints 
  r <- rt[match(r, names(rt))]
  return(r)
}

# 1972
table(t1972$v720420, useNA="always")
d$inc[d$year==1972] <- convper(t1972$v720420, c(99))
table(d$inc[d$year==1972], useNA="always")
# 1976
table(t1976$v763507, useNA="always")
d$inc[d$year==1976] <- convper(t1976$v763507, c(98,99))
table(d$inc[d$year==1976], useNA="always")
# 1980
table(t1980$v800686, useNA="always")
d$inc[d$year==1980] <- convper(t1980$v800686, c(98,99))
table(d$inc[d$year==1980], useNA="always")
# 1984
table(t1984$v840680, useNA="always")
d$inc[d$year==1984] <- convper(t1984$v840680, c(88,98,99))
table(d$inc[d$year==1984], useNA="always")
# 1988
table(t1988$v880520, useNA="always")
d$inc[d$year==1988] <- convper(t1988$v880520, c(88,98,99))
table(d$inc[d$year==1988], useNA="always")
# 1992
table(t1992$v924104, useNA="always")
d$inc[d$year==1992] <- convper(t1992$v924104, c(66,77,88,98,99))
table(d$inc[d$year==1992], useNA="always")
# 1996
table(t1996$v960701, useNA="always")
d$inc[d$year==1996] <- convper(t1996$v960701, c(88,98,99))
table(d$inc[d$year==1996], useNA="always")
# 2000
table(t2000$v000994, useNA="always")
d$inc[d$year==2000] <- convper(t2000$v000994, c(98,99))
table(d$inc[d$year==2000], useNA="always")
# 2004
table(t2004$v043293x, useNA="always")
d$inc[d$year==2004] <- convper(t2004$v043293x, c(88,89))
table(d$inc[d$year==2004], useNA="always")
# 2008
table(t2008$v083248x, useNA="always")
d$inc[d$year==2008] <- convper(t2008$v083248x, c(-9,-8))
table(d$inc[d$year==2008], useNA="always")
# 2012
table(t2012$incgroup_prepost_x, useNA="always")
d$inc[d$year==2012] <- convper(t2012$incgroup_prepost_x, c(-9,-8))
table(d$inc[d$year==2012], useNA="always")
# 2016
table(t2016$v161361x, useNA="always")
table(t2016$v162309x, useNA="always")
t2016$income_all <- t2016$v161361x
t2016$income_all[t2016$v162309x %in% seq(1,28,1)] <- 
  t2016$v162309x[t2016$v162309x %in% seq(1,28,1)]  
d$inc[d$year==2016] <- convper(t2016$income_all, c(-9,-5))
table(d$inc[d$year==2016], useNA="always")

#' 
#' # Adjust Urban OR Suburban Residence
#' 

table(d$urban, useNA="always")
# No missing case before 2000
table(d$urban[d$year<2000], useNA="always")
# A lot of missing cases 2000 or after
table(d$urban[d$year>=2000], useNA="always")
# 2000 # Respondents self-answer
t2000$urban <- t2000$v001019
t2000$urban[t2000$v001019 %in% c(0,9)] <- NA
t2000$urban[t2000$v001019 %in% c(1,2,3,4)] <- 1
t2000$urban[t2000$v001019 %in% c(5,6,7,8)] <- 0
d$urban[d$year==2000] <- t2000$urban
table(d$urban[d$year==2000])
# 2004
#041213 # Census
table(t2004$v041213, useNA="always")
#042043 # Interviewer Rating
table(t2004$v042043, useNA="always")
table(t2004$v041213, t2004$v042043)
# Use Interviewer Rating >= Suburb(3)
# If missing, apply census category
t2004$urban <- (t2004$v042043>=3)*1
t2004$urban[t2004$v042043==0] <- (t2004$v041213[t2004$v042043==0]==5)*1
d$urban[d$year==2004] <- t2004$urban
table(d$urban[d$year==2004], useNA="always")
# 2008
# Census (Restricted)
table(t2008$v081215, useNA="always")
# Interviewer's rating
table(t2008$v082025, useNA="always") # Pre
table(t2008$v084025, useNA="always") # Post
t2008$urban <- (t2008$v082025>=3)*1 # Start with pre >=3 (Suburb)
table(t2008$v082025[t2008$v082025 %in% c(-4,7)],
      t2008$v084025[t2008$v082025 %in% c(-4,7)])
t2008$urban[t2008$v082025 %in% c(-4,7)] <- 
  (t2008$v084025[t2008$v082025 %in% c(-4,7)] >= 3)*1 # Add Post
t2008$urban[t2008$v082025 %in% c(-4,7) & t2008$v084025 %in% c(-4,7)] <- NA # Just in case
d$urban[d$year==2008] <- t2008$urban
table(d$urban[d$year==2008], useNA="always")
# 2012
# Interviewer's Rating (FTF Only)
table(t2012$dwell_block_urban, useNA="always")
t2012$urban <- (t2012$dwell_block_urban>=3)*1
t2012$urban[t2012$dwell_block_urban==-1] <- NA # Web Samples to NA
d$urban[d$year==2012] <- t2012$urban
table(d$urban[d$year==2012], useNA="always")
# 2016
# Interviewer's Rating (FTF Only)
table(t2016$v167534, useNA="always")
t2016$urban <- (t2016$v167534>=3)*1
t2016$urban[t2016$v167534 %in% c(-1,-8)] <- NA # Web & Inapplicables to NA
d$urban[d$year==2016] <- t2016$urban
table(d$urban[d$year==2016], useNA="always")

# Check missing cases 2000 or after
table(d$urban[d$year>=2000], useNA="always")

#'
#' # Adjust Education
#' 

table(d$educ, useNA="always")

# List of Relevant Variables
# 1972: 720300 
# 1976: 763389 
# 1980: 800436 
# 1984: 840438 
# 1988: 880422 
# 1992: 923905,923908 
# 1996: 960607,960610 
# 2000: 000913 
# 2004: 043254 
# 2008: 083218x 
# 2012: dem_edu 
# 2016: V161270 

# Conversion Function by Index
convindex <- function(old.var,index,missing.val){
  r <- old.var
  r[r %in% missing.val] <- NA
  if (is.null(index)) {
    return(r)
  }
  rt <- table(r) # Just a table
  if (length(rt)==length(index)){
    r <- index[match(r, names(rt))]
    return(r)
  } else {
    stop("incorrect index length")
  }
  
}

# 1972
table(t1972$v720300, useNA="always")
index <- c(1, # 0 or 1
           2,3,4,5,6,7,
           8, # COMPLETED 7 GRADES OR LESS PLUS NON-COLLEGE TRAINING
           8,9, # COMPLETED 8 GRADES PLUS NON-COLLEGE TRAINING
           9,10,11,
           10, # 9 GRADES PLUS NON-COLLEGE TRAINING
           11, # 10 GRADES PLUS NON-COLLEGE TRAINING
           12, # 11 GRADES PLUS NON-COLLEGE TRAINING; "HIGH SCHOOL" PLUS NON-COLLEGE TRAINING
           12, # EQUIVALENCY <PRIORITY OVER CODES 11-43>
           12, 13, # 12 GRADES PLUS NON-COLLEGE TRAINING
           14, # SOME COLLEGE; AA; JUNIOR COLLEGE; "NORMAL SCHOOL"
           16, # BACHELOR'S DEGREE
           17, 17, 17, 17, 17) # ABOVE BACHELOR'S DEGREE    
d$educ[d$year==1972] <- convindex(t1972$v720300, index, c(98,99))
table(d$educ[d$year==1972], useNA="always")
# 1976 (Use 763384. 763389 is a summary)
table(t1976$v763384, useNA="always")
table(t1976$v763389, useNA="always")
table(t1976$v763384, t1976$v763389, useNA="always")
t1976$educ <- t1976$v763384
t1976$educ[t1976$v763384==99 & t1976$v763389==9] <- 16 # Convert missing
d$educ[d$year==1976] <- convindex(t1976$educ, NULL, c(98,99)) 
table(d$educ[d$year==1976], useNA="always")
# 1980 (Use 800429. 800436 is a summary)
table(t1980$v800429, useNA="always")
table(t1980$v800436, useNA="always")
table(t1980$v800429, t1980$v800436, useNA="always")
d$educ[d$year==1980] <- convindex(t1980$v800429, NULL, c(98,99))
table(d$educ[d$year==1980], useNA="always")
# 1984 (Use 840432. 840438 is a summary)
table(t1984$v840432, useNA="always")
table(t1984$v840438, useNA="always")
table(t1984$v840432, t1984$v840438, useNA="always")
d$educ[d$year==1984] <- convindex(t1984$v840432, NULL, c(98,99))
table(d$educ[d$year==1984], useNA="always")
# 1992 (923908 is from 1990 for panel cases)
table(t1992$v923905, useNA="always")
table(t1992$v923908, useNA="always")
table(t1992$v923905, t1992$v923908, useNA="always")
t1992$educ <- convindex(t1992$v923905, NULL, c(96,99))
summary(t1992$v912026[t1992$v912026>0], useNA="always") # 1990 ID from 1992 data
t1992$educx <- NA
t1992$educx[t1992$v912026>0] <- 
  t1990$v900554[match(t1992$v912026[t1992$v912026>0], t1990$v900004)]
t1992$educx <- convindex(t1992$educx, NULL, c(98,99))
t1992$educ[t1992$v923905 %in% c(96,99)] <- 
  t1992$educx[t1992$v923905 %in% c(96,99)]
d$educ[d$year==1992] <- t1992$educ
table(d$educ[d$year==1992], useNA="always")
# 1996 (Just use v960607. *v960610 is a summary)
table(t1996$v960607, useNA="always")
table(t1996$v960610, useNA="always")
table(t1996$v960607, t1996$v960610, useNA="always")
d$educ[d$year==1996] <- convindex(t1996$v960607, NULL, c(99))
table(d$educ[d$year==1996], useNA="always")
# 2000 (Use v000910. v000913 is a summary)
table(t2000$v000910, useNA="always")
table(t2000$v000913, useNA="always")
table(t2000$v000910, t2000$v000913, useNA="always")
d$educ[d$year==2000] <- convindex(t2000$v000910, NULL, c(99))
table(d$educ[d$year==2000], useNA="always")
# 2004 (Use v043252. v043254 is a summary)
table(t2004$v043252, useNA="always")
table(t2004$v043254, useNA="always")
table(t2004$v043252, t2004$v043254, useNA="always")
d$educ[d$year==2004] <- convindex(t2004$v043252, NULL, c(88))
table(d$educ[d$year==2004], useNA="always")
# 2008 (Use v083217. v083218x is a summary)
table(t2008$v083217, useNA="always")
table(t2008$v083218x, useNA="always")
table(t2008$v083217, t2008$v083218x, useNA="always")
d$educ[d$year==2008] <- convindex(t2008$v083217, NULL, c(-8,-9))
table(d$educ[d$year==2008], useNA="always")
# 2012 (Use dem_edu. dem_edugroup_x is a summary)
table(t2012$dem_edu, useNA="always")
table(t2012$dem_edugroup_x, useNA="always")
table(t2012$dem_edu, t2012$dem_edugroup_x, useNA="always")
t2012$educ <- t2012$dem_edu
t2012$educ[t2012$dem_edu==95 & t2012$dem_edugroup_x==3] <- 10 # Convert Other
t2012$educ[t2012$dem_edu==95 & t2012$dem_edugroup_x==5] <- 14 # Convert Other
table(t2012$educ, useNA="always")
index <- c(0, #01. Less than 1st grade 
           2.5, # 02. 1st, 2nd, 3rd or 4th grade 
           5.5, # 03. 5th or 6th grade 
           7.5, # 04. 7th or 8th grade 
           9, # 05. 9th grade 
           10, # 06. 10th grade 
           11, # 07. 11th grade 
           12, # 08. 12th grade no diploma 
           12, # 09. High school graduate- high school diploma or equivalent (for example: GED) 
           13, # 10. Some college but no degree 
           14, # 11. Associate degree in college - Occupational/vocational program 
           14, # 12. Associate degree in college -- Academic program 
           16, # 13. Bachelor's degree (For example: BA, AB, BS) 
           17, # 14. Master's degree (For example: MA, MS, MEng, MEd, MSW, MBA) 
           17, # 15. Professional School Degree (For example: MD,DDS,DVM,LLB,JD) 
           17) # 16. Doctorate degree (For example: PhD, EdD))
d$educ[d$year==2012] <- convindex(t2012$educ, index, c(-9,95))
table(d$educ[d$year==2012], useNA="always")
# 2016
table(t2016$v161270, useNA="always")
# Use the same index as 2012
d$educ[d$year==2016] <- convindex(t2016$v161270, index, c(-9,90,95))
table(d$educ[d$year==2016], useNA="always")

# # Alternatively, use percentile conversion function
# # (Education grade categories are unstable across years,
# #  and importance of the specific grade should change over the years)
# d$educp <- NA
# 
# # 1972
# table(t1972$v720300, useNA="always")
# d$educp[d$year==1972] <- convper(t1972$v720300, c(98,99))
# table(d$educp[d$year==1972], useNA="always")
# # 1976 (Use 763384. 763389 is a summary)
# table(t1976$v763384, useNA="always")
# table(t1976$v763389, useNA="always")
# table(t1976$v763384, t1976$v763389, useNA="always")
# t1976$educ <- t1976$v763384
# t1976$educ[t1976$v763384==99 & t1976$v763389==9] <- 16 # Convert missing
# d$educp[d$year==1976] <- convper(t1976$educ, c(98,99)) 
# table(d$educp[d$year==1976], useNA="always")
# # 1980 (Use 800429. 800436 is a summary)
# table(t1980$v800429, useNA="always")
# table(t1980$v800436, useNA="always")
# table(t1980$v800429, t1980$v800436, useNA="always")
# d$educp[d$year==1980] <- convper(t1980$v800429, c(98,99))
# table(d$educp[d$year==1980], useNA="always")
# # 1984 (Use 840432. 840438 is a summary)
# table(t1984$v840432, useNA="always")
# table(t1984$v840438, useNA="always")
# table(t1984$v840432, t1984$v840438, useNA="always")
# d$educp[d$year==1984] <- convper(t1984$v840432, c(98,99))
# table(d$educp[d$year==1984], useNA="always")
# # 1992 (923908 is from 1990 for panel cases)
# table(t1992$v923905, useNA="always")
# table(t1992$v923908, useNA="always")
# table(t1992$v923905, t1992$v923908, useNA="always")
# t1992$educ <- t1992$v923905
# summary(t1992$v912026[t1992$v912026>0], useNA="always") # 1990 ID from 1992 data
# t1992$educx <- NA
# t1992$educx[t1992$v912026>0] <- 
#   t1990$v900554[match(t1992$v912026[t1992$v912026>0], t1990$v900004)]
# t1992$educ[t1992$v923905 %in% c(96,99)] <- 
#   t1992$educx[t1992$v923905 %in% c(96,99)]
# table(t1992$educ, useNA="always")
# d$educp[d$year==1992] <- convper(t1992$educ, c(98))
# table(d$educp[d$year==1992], useNA="always")
# # 1996 (Just use v960607. *v960610 is a summary)
# table(t1996$v960607, useNA="always")
# table(t1996$v960610, useNA="always")
# table(t1996$v960607, t1996$v960610, useNA="always")
# d$educp[d$year==1996] <- convper(t1996$v960607, c(99))
# table(d$educp[d$year==1996], useNA="always")
# # 2000 (Use v000910. v000913 is a summary)
# table(t2000$v000910, useNA="always")
# table(t2000$v000913, useNA="always")
# table(t2000$v000910, t2000$v000913, useNA="always")
# d$educp[d$year==2000] <- convper(t2000$v000910, c(99))
# table(d$educp[d$year==2000], useNA="always")
# # 2004 (Use v043252. v043254 is a summary)
# table(t2004$v043252, useNA="always")
# table(t2004$v043254, useNA="always")
# table(t2004$v043252, t2004$v043254, useNA="always")
# d$educp[d$year==2004] <- convper(t2004$v043252, c(88))
# table(d$educp[d$year==2004], useNA="always")
# # 2008 (Use v083217. v083218x is a summary)
# table(t2008$v083217, useNA="always")
# table(t2008$v083218x, useNA="always")
# table(t2008$v083217, t2008$v083218x, useNA="always")
# d$educp[d$year==2008] <- convper(t2008$v083217, c(-8,-9))
# table(d$educp[d$year==2008], useNA="always")
# # 2012 (Use dem_edu. dem_edugroup_x is a summary)
# table(t2012$dem_edu, useNA="always")
# table(t2012$dem_edugroup_x, useNA="always")
# table(t2012$dem_edu, t2012$dem_edugroup_x, useNA="always")
# t2012$educ <- t2012$dem_edu
# t2012$educ[t2012$dem_edu==95 & t2012$dem_edugroup_x==3] <- 10 # Convert Other
# t2012$educ[t2012$dem_edu==95 & t2012$dem_edugroup_x==5] <- 14 # Convert Other
# d$educp[d$year==2012] <- convper(t2012$educ, c(-9,95))
# table(d$educp[d$year==2012], useNA="always")
# # 2016
# table(t2016$v161270, useNA="always")
# d$educp[d$year==2016] <- convper(t2016$v161270, c(-9,90,95))
# table(d$educp[d$year==2016], useNA="always")

#' 
#' # Adjust Job Variables
#' 
table(d$hmake, useNA="always") # OK
table(d$ret, useNA="always") # OK
table(d$prof, useNA="always")
table(d$cler, useNA="always")

# Recode Occupations as Job Type
# Code Professional/Managerial as 1
# Code Clerical (administrative support)/ sales workers as 2
# Code Skilled (technician), semi-skilled, operatives and service workers as 3
# Code Laborers, except farm as 4, 
# Code  Farmers, farm managers, farm laborers and foremen; forestry and fishermen as 5
# see https://www.eeoc.gov/employers/eeo1survey/jobclassguide.cfm for reference

jobindex <- c(0, # Non-applicables (not working, etc...) (-1)
           1, # 1. Top Executives
           1, # 2. Advertising, Marketing, Promotions,
           1, # 3. Operations Specialties Managers
           1, # 4. Other management Occupations
           1, # 5. Business Operations Specialists
           1, # 6. Financial Specialists
           1, # 7. Computer Occupations
           1, # 8. Mathematical Science Occupations
           1, # 9. Architects, Surveyors, and Cartograp
           1, # 10. Engineers
           1, # 11. Drafters, Engineering Technicians,
           1, # 12. Life Scientists
           1, # 13. Physical Scientists
           1, # 14. Social Scientists and Related Worke
           1, # 15. Life, Physical, and Social Science
           1, # 16. Counselors, Social Workers, and Oth
           1, # 17. Religious Workers
           1, # 18. Lawyers, Judges, and related Worker
           1, # 19. Legal Support Workers
           1, # 20. Postsecondary Teachers
           1, # 21. Preschool, Primary, Secondary, and
           1, # 22. Other Teachers and Instructors
           1, # 23. Librarians, Curators, and Archivist
           1, # 24. Other Education, Training, and Libr
           1, # 25. Art and Design Workers
           1, # 26. Entertainers and Performers, Sports
           1, # 27. Media and Communication Workers
           1, # 28. Media and Communication Equipment W
           1, # 29. Health Diagnosing and Treating Prac
           3, # 30. Health Technologists and Technician
           3, # 31. Other Healthcare Practitioners and
           3, # 32. Nursing, Psychiatric, and Home Heal
           3, # 33. Occupational Therapy and Physical T
           3, # 34. Other Healthcare Support Occupation
           3, # 35. Supervisors of Protective Service W
           3, # 36. Fire Fighting and Prevention Worker
           3, # 37. Law Enforcement Workers
           3, # 38. Other Protective Service Workers
           3, # 39. Supervisors of Food Preparation and
           3, # 40. Cooks and Food Preparation Workers
           3, # 41. Food and Beverage Serving Workers
           3, # 42. Other Food Preparation and Serving
           3, # 43. Supervisors of Building and Grounds
           3, # 44. Building Cleaning and Pest Control
           3, # 45. Grounds Maintenance Workers
           3, # 46. Supervisor of Personal Care and Ser
           3, # 47. Animal Care and Service Workers
           3, # 48. Entertainment Attendants and Relate
           3, # 49. Funeral Service Workers
           3, # 50. Personal Appearance Workers
           3, # 51. Baggage Porters, Bellhops, and Conc
           3, # 52. Tour and Travel Guides
           3, # 53. Other Personal Care and Service Wor
           2, # 54. Supervisors of Sales Workers
           2, # 55. Retail Sales Workers
           2, # 56. Sales Representatives, Services
           2, # 57. Sales Representatives, Wholesale an
           2, # 58. Other Sales and Related Workers
           2, # 59. Supervisors of Office and Administr
           2, # 60. Communications Equipment Operators
           2, # 61. Financial Clerks
           2, # 62. Information and Record Clerks
           2, # 63. Matertial Recordingm Scheduling, Di
           2, # 64. Secretaries and Administrative Assi
           2, # 65. Other Office and Administrative Sup
           5, # 66. Supervisors of Farming, Fishing, an
           5, # 67. Agricultural Workers
           5, # 68. Fishing and Hunting Workers
           5, # 69. Forest, Conservation, and Logging W
           4, # 70. Supervisors of Construction and Ext
           4, # 71. Construction Trades Workers
           4, # 72. Helpers, Construction Trades
           4, # 73. Other Construction and Related Work
           4, # 74. Extraction Workers
           4, # 75. Supervisors of Installation, Mainte
           4, # 76. Electrical and Electronic Equipment
           4, # 77. Vehicle and Mobile Equipment Mechan
           4, # 78. Other Installation, Maintenance, an
           4, # 79. Supervisors of Production Workers
           4, # 80. Assemblers and Fabricators
           4, # 81. Food Processing Workers
           4, # 82. Metal Workers and Plastic Workers
           4, # 83. Printing Workers
           4, # 84. Textile, Apparel, and Furnishing Wo
           4, # 85. Woodworkers
           4, # 86. Plant and System Operators
           4, # 87. Other Production Occupations
           3, # 88. Supervisors of Transportation and M
           3, # 89. Air Transportation Workers
           3, # 90. Motor Vehicle Operators
           3, # 91. Rail Transportation Workers
           3, # 92. Water Transportation Workers
           3, # 93. Other Transportation Workers
           3, # 94. Material Moving Workers
           1, # 95. Military Officer Special and Tactic
           1, # 96. First-Line Enlisted Military Supervisors
           1) # 97. Military Enlisted Tactical Operatio
jobindexid <- seq(0,97,1)
length(jobindex)==length(jobindexid)

# 2008 Occupation
table(!is.na(occ08$COCode),!is.na(occ08$POCode))
# Substitute with Past Job if Current Job is Not Available
occ08$OCode <- occ08$COCode
occ08$OCode[is.na(occ08$COCode)] <- occ08$POCode[is.na(occ08$COCode)]
table(occ08$OCode, useNA="always")
# Attach Occupation Codes to t2008 Data
length(occ08$ID); length(t2008$v080001)
t2008$jobcode <- occ08$OCode[match(t2008$v080001,occ08$ID)]
t2008$jobcode[is.na(t2008$jobcode)] <- 0
table(t2008$jobcode, useNA="always")
# Categorize industry codes into 5 categories
jobindex08 <- jobindex[jobindexid %in% names(table(t2008$jobcode))]
length(table(t2008$jobcode))-length(jobindex08) # Should be 2
t2008$jobtype <- convindex(t2008$jobcode, jobindex08, c(996,997))
table(t2008$jobtype, useNA="always")
# Insert Professional and Clerical Dummies
d$prof[d$year==2008] <- (t2008$jobtype==1)*1
table(d$prof[d$year==2008], useNA="always")
d$cler[d$year==2008] <- (t2008$jobtype==2)*1
table(d$cler[d$year==2008], useNA="always")

# 2012 Occupation
table(t2012$dem_occnow, useNA="always") # NOT provided yet
table(t2012$dem_occpast, useNA="always") # NOT provided yet
# Use Machine Coded Job Type Category
# (Note that the machine learned code only has 3 categories)
length(t2012$caseid)-length(occ12$id) # Not Equal
t2012$jobtype3 <- occ12$jobcat3[match(t2012$caseid,occ12$id)]
t2012$jobtype3[is.na(t2012$jobtype3)] <- 0
table(t2012$jobtype3, useNA="always")
# Insert Professional and Clerical Dummies
d$prof[d$year==2012] <- (t2012$jobtype3==1)*1
table(d$prof[d$year==2012], useNA="always")
d$cler[d$year==2012] <- (t2012$jobtype3==2)*1
table(d$cler[d$year==2012], useNA="always")

# 2016 Occupation
table(t2016$v161282b, useNA="always") # Past
table(t2016$v161291b, useNA="always") # Current
# Substitute with Past Job if Current Job is Not Available
t2016$jobcode <- t2016$v161291b 
t2016$jobcode[t2016$v161291b %in% c(-1,98,99)] <- t2016$v161282b[t2016$v161291b %in% c(-1,98,99)]
t2016$jobcode[t2016$jobcode %in% c(-1)] <- 0
table(t2016$jobcode, useNA="always")
# Categorize industry codes into 5 categories
jobindex16 <- jobindex[jobindexid %in% names(table(t2016$jobcode))]
length(table(t2016$jobcode))-length(jobindex16) # Should be 2
t2016$jobtype <- convindex(t2016$jobcode, jobindex16, c(98,99,-8,-9))
table(t2016$jobtype, useNA="always")
# Insert Professional and Clerical Dummies
d$prof[d$year==2016] <- (t2016$jobtype==1)*1
table(d$prof[d$year==2016], useNA="always")
d$cler[d$year==2016] <- (t2016$jobtype==2)*1
table(d$cler[d$year==2016], useNA="always")

#'
#' # Insert Objective Knowledge Index
#' 

for (i in 1:length(objknlist)){
  setyear <- as.numeric(gsub("yr","",names(objknlist)[i]))
  d[d$year==setyear,]$objkn <- objknlist[[i]]$vars$objkn_irt
}

#'
#' # Remove All Each Year Data
#' 
rm(t1972,t1976,t1980,t1984,t1988,t1992,t1996,
   t2000,t2004,t2008,t2012,t2016,t1990,occ08,
   occ12,objknlist)

#+ eval=FALSE, echo=FALSE
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# rmarkdown::render("recode_eachyear.R",
#                   "pdf_document")
