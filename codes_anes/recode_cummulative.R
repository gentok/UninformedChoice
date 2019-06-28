#' ---
#' title: "Recoding ANES Cummulative Data"
#' author: "Gento Kato"
#' date: "November 7, 2018"
#' ---

#' # Preparation 
#' 
#' ## Require Packages Necessary
require(readstata13)

#' ## Read in Data
#+ echo=FALSE
# source("./anes_dataloc.R") # The vector of Data locations
#+
cumloc <- dataloc[dataid=="cum"]
cum <- read.dta13(cumloc, convert.factors = FALSE)

#' ## making all variables lowercase
names(cum) <- sapply(names(cum), tolower)
head(names(cum))

#' ## Extract Variable Labels
cumlb <- varlabel(cum)
head(cumlb)

#' ## Prepare New Data
d <- data.frame(year=cum$vcf0004, id=cum$vcf0006, 
                state = cum$vcf0901b)
d$state[cum$vcf0901b=="99"] <- NA

#' ## Original Variable Names
ovn <- cumlb[c("vcf0004","vcf0006")]
addovn <- function(vn, custom=FALSE){
  if (custom==TRUE) { c(ovn, vn)
  } else { c(ovn, cumlb[vn])
  }
}

#' # Recoding
#' 
#' ## Mode of Interview
#' 

d$mode <- cum$vcf0017
table(d$mode)
# 4 is only internet
ovn <- addovn("vcf0017")

#' 
#' ## Weight
#' 

d$wgt <- cum$vcf0009x
ovn <- addovn("vcf0009x")
# for 2012 & 2016 this weight is for FTF only 
# (not matter if using vcf0009, vcf0010, or vcf0011)

#' 
#' ## Presidential vote (Two Party Choice)
#' 
d$presvote <- cum$vcf0704 - 1 # 1=REP, 0=DEM
d$presvote[cum$vcf0704 %in% c(0,3)] <- NA
table(d$presvote, cum$vcf0704, useNA="always")
ovn <- addovn(paste(cumlb["vcf0704"],"(Rep=1,Dem=0,Other=NA)"),custom=TRUE)

#' 
#' ## Presidential vote (Abstention/Other Included)
#' 

d$presvote3 <- cum$vcf0704 # 1=REP, 0=DEM
d$presvote3[cum$vcf0704 %in% c(3)] <- 0
table(d$presvote3, cum$vcf0704, useNA="always")
ovn <- addovn(paste(cumlb["vcf0704"],"(Rep=1,Dem=2,Abstain/Other=0)"),custom=TRUE)

#' 
#' ## Information: Pre-Election Interviewer Rating
#' 
d$info <- NA
d$info[cum$vcf0050a == 1] <- .95
d$info[cum$vcf0050a == 2] <- .80
d$info[cum$vcf0050a == 3] <- .50
d$info[cum$vcf0050a == 4] <- .20
d$info[cum$vcf0050a == 5] <- .05
table(d$info, cum$vcf0050a, useNA="always")
ovn <- addovn("vcf0050a")

#'
#' ## Information: Post-Election Interviewer Rating (Missing in 1972, 1976, 1988(Partially))
#'
d$info_post <- NA
d$info_post[cum$vcf0050b == 1] <- .95
d$info_post[cum$vcf0050b == 2] <- .80
d$info_post[cum$vcf0050b == 3] <- .50
d$info_post[cum$vcf0050b == 4] <- .20
d$info_post[cum$vcf0050b == 5] <- .05
table(d$info_post, cum$vcf0050b, useNA="always")
ovn <- addovn("vcf0050b")

#'
#' ## Party Identification (7pt Scale rescaled to -2:2)
#'
d$pid <- (cum$vcf0301 - 4)*(2/3) 
d$pid[cum$vcf0301==0] <- NA
table(d$pid, cum$vcf0301, useNA="always") # 2 as Str Rep -2 as str Dem  
ovn <- addovn("vcf0301")

#'
#' ## Party Affinity (Feeling Thermometer Difference 0-1 Scale)
#'
d$paff <- cum$vcf0291 # Dem-Rep Feeling Therm Diff
d$paff[cum$vcf0291 %in% c(998,999)] <- NA
table(d$paff) # 2-99
d$paff <- 1 - (d$paff - 2)/(99-2) 
table(d$paff) # 1 as most Republican, 0 as most Democrat
ovn <- addovn("vcf0291")

#'
#' ## Ideology
#' 
#' ### Self-ideology
table(cum[d$year!=2016,]$vcf0803, cum[d$year!=2016,]$vcf0824, useNA="always")
# 2016 is messed up
table(cum[d$year==2016,]$vcf0803, useNA="always")
table(cum[d$year==2016,]$vcf0824, useNA="always")
table(cum[d$year==2016,]$vcf0803, cum[d$year==2016,]$vcf0824, useNA="always")
# Recode Data
d$ideself <- cum$vcf0803 - 4
d$ideself[cum$vcf0803==9] <- 0
d$ideself[cum$vcf0803 %in% c(0)] <- NA
d$ideself[cum$vcf0824==1 & d$year != 2016] <- -1
d$ideself[cum$vcf0824==3 & d$year != 2016] <- 0
d$ideself[cum$vcf0824==5 & d$year != 2016] <- 1
d$ideself[cum$vcf0824%in%c(7,8)] <- 0
table(d$ideself, useNA="always")
ovn <- addovn("vcf0803")
#' 
#' ### Democratic Candidate Ideology
table(cum$vcf9088, useNA="always")
d$idedem <- ifelse(cum$vcf9088%in%c(8,9), 0,
                   ifelse(cum$vcf9088==0, NA, 
                   cum$vcf9088 - 4))
table(d$idedem)
ovn <- addovn("vcf9088")
#' ### Republican Candidate Ideology
table(cum$vcf9096, useNA="always")
d$iderep <- ifelse(cum$vcf9096%in%c(8,9), 0,
                   ifelse(cum$vcf9096==0, NA, 
                          cum$vcf9096 - 4))
table(d$iderep)
ovn <- addovn("vcf9096")
#' ### Ideological Alignment (Republican Advantage)
d$iderepadv <- -(abs(d$ideself-d$iderep) - abs(d$ideself-d$idedem))
table(d$iderepadv, useNA="always")
ovn <- addovn(paste(cumlb["vcf0803"], "(how closer the distance from Republican than Democratic candidate, also vcf9088 and vcf9096"),TRUE)
# Quadratic Version
d$iderepadvsq <- -(abs(d$ideself-d$iderep)^2 - abs(d$ideself-d$idedem)^2)
table(d$iderepadvsq, useNA="always")
ovn <- addovn(paste(cumlb["vcf0803"], "(how closer the squared distance from Republican than Democratic candidate, also vcf9088 and vcf9096"),TRUE)

#'
#' # Retrospective Economy (1972 & 76 are missing)
#'
table(cum$vcf0870, useNA="always")
table(cum$vcf0871, useNA="always")
d$evecon <- ifelse(cum$vcf0871%in%c(0,8,9),NA,
                   3 - cum$vcf0871)
d$evecon[is.na(d$evecon) & cum$vcf0870==1] <- 1
d$evecon[is.na(d$evecon) & cum$vcf0870==5] <- -1
d$evecon[is.na(d$evecon) & cum$vcf0870%in%c(3,8)] <- 0
table(d$evecon, useNA="always")
ovn <- addovn(paste(cumlb["vcf0803"],"also vcf0870"),TRUE)
#'
#' ## Income: percentile using midpoints of categories
#' 
#' * note: will get income data from NES yearly files
#' 
d$inc <- NA
d$inc[cum$vcf0114 == 1] <- .08
d$inc[cum$vcf0114 == 2] <- .25
d$inc[cum$vcf0114 == 3] <- .505
d$inc[cum$vcf0114 == 4] <- .815
d$inc[cum$vcf0114 == 5] <- .98
table(d$inc, cum$vcf0114, useNA="always")
ovn <- addovn("vcf0114")

#' 
#' ## Region: East, South, West, excluded category is Central
#' 
d$east <- (cum$vcf0112==1)*1
ovn <- addovn(paste(cumlb["vcf0112"], "is EAST"), custom=TRUE)
d$south <- (cum$vcf0112==3)*1
ovn <- addovn(paste(cumlb["vcf0112"], "is SOUTH"), custom=TRUE)
d$west <- (cum$vcf0112==4)*1
ovn <- addovn(paste(cumlb["vcf0112"], "is WEST"), custom=TRUE)

#' 
#' ## Urban: 
#' 
#' * Note: Lots missing in 2000.  All missing 2004, 2008, 2012 & 2016 
#' will get this variable from NES yearly files
#' 
table(cum$vcf0111) # include large city & suburb
d$urban <- (cum$vcf0111!=3)*1
d$urban[cum$vcf0111==0] <- NA
table(d$urban, cum$vcf0111, useNA="always")
ovn <- addovn("vcf0111")

#' 
#' ## Education: number of years
#' 
#' * note: will need to go to NES yearly files
#' 
d$educ <- NA
d$educ[cum$vcf0140a==1] <- 8
d$educ[cum$vcf0140a==2] <- 10.5
d$educ[cum$vcf0140a %in% c(3,4)] <- 12
d$educ[cum$vcf0140a==5] <- 14.5
d$educ[cum$vcf0140a==6] <- 16
d$educ[cum$vcf0140a==7] <- 18
table(d$educ, cum$vcf0140a, useNA="always")
ovn <- addovn("vcf0140a")

#' 
#' ## Home ownership
#' 
d$own <- 2 - cum$vcf0146
d$own[cum$vcf0146  %in% c(8,9)] <- NA
table(d$own, cum$vcf0146, useNA="always")
ovn <- addovn("vcf0146")
    
#'
#' ## Union household
#' 
d$union <- 2 - cum$vcf0127
d$union[cum$vcf0127  %in% c(0)] <- NA
table(d$union, cum$vcf0127, useNA="always")
ovn <- addovn("vcf0127")

#'
#' ## Female
#' 
d$female <- cum$vcf0104 - 1
d$female[cum$vcf0104  %in% c(0,3)] <- NA
table(d$female, cum$vcf0104, useNA="always")
ovn <- addovn("vcf0104")

#' 
#' ## Age & Age squared: in years
#' 
d$age <- cum$vcf0101
d$age[cum$vcf0101==0] <- NA
ovn <- addovn("vcf0101")
d$agesq <- d$age^2
ovn <- addovn(paste(cumlb["vcf0101"], "Squared"), custom=TRUE)

#' 
#' ## Married
#' 
d$married <- (cum$vcf0147==1)*1
d$married[cum$vcf0147==9] <- NA
table(d$married, cum$vcf0147, useNA="always")
ovn <- addovn("vcf0147")

#'
#' ## Black
#' 
d$black <- (cum$vcf0106==2)*1
d$black[cum$vcf0106 %in% c(0,9)] <- NA
table(d$black, cum$vcf0106, useNA="always")
ovn <- addovn(paste(cumlb["vcf0106"], "(Black)"), custom=TRUE)

#'
#' ## Ethnic Minority (Non-White)
#' 
d$minority <- (cum$vcf0106 != 1)*1
d$minority[cum$vcf0106 %in% c(0,9)] <- NA
table(d$minority, cum$vcf0106, useNA="always")
ovn <- addovn(paste(cumlb["vcf0106"], "(Non-White)"), custom=TRUE)

#'
#' ## Religion
#' 
#' * note: missing in 2016. Need to get to yearly file.
#' 
d$prot <- (cum$vcf0128==1)*1
d$prot[cum$vcf0128==0] <- NA
table(d$prot, cum$vcf0128, useNA="always")
ovn <- addovn(paste(cumlb["vcf0128"], "(Protestant)"), custom=TRUE)
d$cath <- (cum$vcf0128==2)*1
d$cath[cum$vcf0128==0] <- NA
table(d$cath, cum$vcf0128, useNA="always")
ovn <- addovn(paste(cumlb["vcf0128"], "(Catholic)"), custom=TRUE)
d$jew <- (cum$vcf0128==3)*1
d$jew[cum$vcf0128==0] <- NA
table(d$jew, cum$vcf0128, useNA="always")
ovn <- addovn(paste(cumlb["vcf0128"], "(Jews)"), custom=TRUE)

#' 
#' ## Occuptation
#' 
#' * note: For professional and Clerical, need to go to yearly files.
#' 

## Work Status
table(cum$vcf0118[d$year>=1972], useNA="always") # Work Status. 3 is retired, 4 is homemaker
# Retired
d$ret <- (cum$vcf0118==3)*1
d$ret[cum$vcf0118 %in% c(9)] <- NA
table(d$ret, cum$vcf0151, useNA="always")
ovn <- addovn(paste(cumlb["vcf0118"], "(Retired)"), custom=TRUE)
# Homemaker
d$hmake <- (cum$vcf0118==4)*1
d$hmake[cum$vcf0118 %in% c(9)] <- NA
table(d$ret, cum$vcf0118, useNA="always")
ovn <- addovn(paste(cumlb["vcf0118"], "(Homemaker)"), custom=TRUE)

## Job Type
table(cum$vcf0151[d$year>=1972], useNA="always") # 1 is professional, 2 is clerical
table(cum$vcf0151[d$year>=2008], useNA="always") # Missing 2008 or after
#' * will probably need to code this from NES yearly data files *
# Professional
d$prof <- (cum$vcf0151==1)*1 
d$prof[cum$vcf0151==0] <- NA 
table(d$prof, cum$vcf0151, useNA="always") 
ovn <- addovn(paste(cumlb["vcf0151"], "(Professional)"), custom=TRUE)
# Clerical
d$cler <- (cum$vcf0151==2)*1
d$cler[cum$vcf0151==0] <- NA
table(d$cler, cum$vcf0151, useNA="always") 
ovn <- addovn(paste(cumlb["vcf0151"], "(Clerical)"), custom=TRUE)

#' 
#' ## Objective Knowledge
#' 
#' * note: questions vary across surveys, thus need to go to yearly data.
#' 
d$objkn <- NA 
ovn <- addovn("Objective Knowledge", custom=TRUE)

#'
#' ## Policy Principles
#'
#' * note: original questions vary across surveys. Go to yearly data
#'
# d$limitgvt <- NA 
# ovn <- addovn("Limited Government", custom=TRUE)
# d$trdmoral <- NA 
# ovn <- addovn("Traditional Morality", custom=TRUE)
# d$militstr <- NA 
# ovn <- addovn("Military Strength", custom=TRUE)

#'
#' ## Mode of Interview
#'

#' 
#' ## ONLY KEEPING PRESIDENTIAL YEARS 1972-2016
#' 
d <- d[d$year %in% seq(1972,2016,4),]

#' 
#' ## Attach the Label Back to the Data
#' 
length(names(d))==length(ovn)
attr(d, "var.labels") <- ovn

#'
#' ## Remove Original Data and Labels
#' 
rm(cum, cumlb, ovn, addovn)

#+ eval=FALSE, echo=FALSE
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# rmarkdown::render("recode_cummulative.R",
#                   "pdf_document")
