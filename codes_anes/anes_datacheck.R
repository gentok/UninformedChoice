#' ---
#' title: "Check Missingness of ANES Data"
#' author: "Gento Kato"
#' date: "Apr 22, 2019"
#' ---
#' 

#' ## Preparation 
#' 
#' Required Packages
#' 
require(readstata13)
require(pbapply)
require(ggrepel)
#' 
#' Clear Workspace
#' 
rm(list=ls())
#' 
#' Set Working Directory (The directory of this .R file)
#' 
library(rprojroot);library(rstudioapi)
if (rstudioapi::isAvailable()==TRUE) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path)); 
} 
projdir <- find_root(has_file("thisishome.txt"))
cat(paste("Working Directory Set to:\n",projdir))
setwd(projdir)
#'
#' Functions
#'
source(paste(projdir,"codes_anes/anes_dataloc.R",sep="/"))
#'
#' Data location
#'
dloc <- paste(projdir, "data/anes_info_eff.rds", sep="/")
#'
#' Import Data
#'

d <- readRDS(dloc)
d$w <- d$wgt
d$evecon[d$repinc2==0] <- -d$evecon[d$repinc2==0]
#d <- d[d$mode!=4,]

#' 
#' ## Import Each Year Data
#' 
years <- c(seq(1972,2016,4),1990)
eyloc <- dataloc[dataid %in% years]
for (i in 1:length(years)){
  obj <- read.dta13(eyloc[i], convert.factors = FALSE)
  names(obj) <- sapply(names(obj), tolower)
  assign(paste("t",years[i],sep=""), obj)
}

#'
#' ## Check Data
#'
#'

# 1972
t72 <- d[d$year==1972 & !is.na(d$presvote),]
table(t72$info, useNA="always") 
summary(t72$objkn, useNA="always") # Big Missing Due to Mode of Questionaire
table(is.na(t72$objkn), t72$mode_1972, useNA="always")
table(t72$pid, useNA="always") 
table(t72$ideself, useNA="always") # Missing due to questionaire
table(t72$ideself, t72$mode_1972, useNA="always")
table(t72$idedem, useNA="always")
table(t72$iderep, useNA="always") 
table(t72$iderepadv, useNA="always")
table(t72$evecon, useNA="always") # due to questionaire
table(t72$evecon, t72$mode_1972, useNA="always")
table(t72$female, useNA="always")
summary(t72$age)
summary(t72$agesq)
table(t72$educ, useNA="always")
summary(t72$inc)
table(t72$minority, useNA="always")
table(t72$prot, useNA="always")
table(t72$cath, useNA="always")
# Variables not used
table(t72$married, useNA="always")
table(t72$own, useNA="always")
table(t72$hmake, useNA="always")
table(t72$ret, useNA="always")
table(t72$cler, useNA="always")
table(t72$prof, useNA="always")
table(t72$union, useNA="always")
table(t72$urban, useNA="always")
table(t72$east, useNA="always")
table(t72$south, useNA="always")
table(t72$west, useNA="always")


# 1976
t76 <- d[d$year==1976 & !is.na(d$presvote),]
table(t76$info, useNA="always")
summary(t76$objkn, useNA="always")
table(t76$pid, useNA="always") 
table(t76$idedem, useNA="always") 
table(t76$iderep, useNA="always") 
table(t76$ideself, useNA="always")
table(t76$iderepadv, useNA="always") 
table(t76$evecon, useNA="always")
table(t76$female, useNA="always")
summary(t76$age)
summary(t76$agesq)
table(t76$educ, useNA="always")
summary(t76$inc)
table(t76$minority, useNA="always")
table(t76$prot, useNA="always")
table(t76$cath, useNA="always")
# Variables not used
table(t76$married, useNA="always")
table(t76$own, useNA="always")
table(t76$hmake, useNA="always")
table(t76$ret, useNA="always")
table(t76$cler, useNA="always")
table(t76$prof, useNA="always")
table(t76$union, useNA="always")
table(t76$urban, useNA="always")
table(t76$east, useNA="always")
table(t76$south, useNA="always")
table(t76$west, useNA="always")

# 1980
t80 <- d[d$year==1980 & !is.na(d$presvote),]
table(t80$info, useNA="always")
summary(t80$objkn, useNA="always")
table(t80$pid, useNA="always") 
table(t80$idedem, useNA="always") 
table(t80$iderep, useNA="always")
table(t80$ideself, useNA="always") 
table(t80$iderepadv, useNA="always") # moderate missingness
table(t80$evecon, useNA="always")
table(t80$female, useNA="always")
summary(t80$age)
summary(t80$agesq)
table(t80$educ, useNA="always")
summary(t80$inc) # moderate missigness
table(t80$minority, useNA="always")
table(t80$prot, useNA="always")
table(t80$cath, useNA="always")
# Variables not used
table(t80$married, useNA="always")
table(t80$own, useNA="always")
table(t80$hmake, useNA="always")
table(t80$ret, useNA="always")
table(t80$cler, useNA="always")
table(t80$prof, useNA="always")
table(t80$union, useNA="always")
table(t80$urban, useNA="always")
table(t80$east, useNA="always")
table(t80$south, useNA="always")
table(t80$west, useNA="always")

# 1984
t84 <- d[d$year==1984 & !is.na(d$presvote),]
table(t84$info, useNA="always")
summary(t84$objkn, useNA="always")
table(t84$pid, useNA="always") 
table(t84$idedem, useNA="always") 
table(t84$iderep, useNA="always") 
table(t84$ideself, useNA="always") 
table(t84$iderepadv, useNA="always")
table(t84$evecon, useNA="always")
table(t84$female, useNA="always")
summary(t84$age)
summary(t84$agesq)
table(t84$educ, useNA="always")
summary(t84$inc) # moderate missigness
table(t84$minority, useNA="always")
table(t84$prot, useNA="always")
table(t84$cath, useNA="always")
# Variables not used
table(t84$married, useNA="always")
table(t84$own, useNA="always")
table(t84$hmake, useNA="always")
table(t84$ret, useNA="always")
table(t84$cler, useNA="always")
table(t84$prof, useNA="always")
table(t84$union, useNA="always")
table(t84$urban, useNA="always")
table(t84$east, useNA="always")
table(t84$south, useNA="always")
table(t84$west, useNA="always")

# 1988
t88 <- d[d$year==1988 & !is.na(d$presvote),]
table(t88$info, useNA="always")
summary(t88$objkn, useNA="always")
table(t88$pid, useNA="always") 
table(t88$idedem, useNA="always") 
table(t88$iderep, useNA="always") 
table(t88$ideself, useNA="always") 
table(t88$iderepadv, useNA="always")
table(t88$evecon, useNA="always")
table(t88$female, useNA="always")
summary(t88$age)
summary(t88$agesq)
table(t88$educ, useNA="always")
summary(t88$inc) # moderate missigness
table(t88$minority, useNA="always")
table(t88$prot, useNA="always")
table(t88$cath, useNA="always")
# Variables not used
table(t88$married, useNA="always")
table(t88$own, useNA="always")
table(t88$hmake, useNA="always")
table(t88$ret, useNA="always")
table(t88$cler, useNA="always")
table(t88$prof, useNA="always")
table(t88$union, useNA="always")
table(t88$urban, useNA="always")
table(t88$east, useNA="always")
table(t88$south, useNA="always")
table(t88$west, useNA="always")

# 1992
t92 <- d[d$year==1992 & !is.na(d$presvote),]
table(t92$info, useNA="always")
summary(t92$objkn, useNA="always") # due to questionaire mode
table(is.na(t92$objkn), t92$mode_1992, useNA="always")
table(t92$pid, useNA="always") 
table(t92$idedem, useNA="always") 
table(t92$iderep, useNA="always") 
table(t92$ideself, useNA="always") 
table(t92$iderepadv, useNA="always")
table(t92$evecon, useNA="always")
table(t92$female, useNA="always")
summary(t92$age)
summary(t92$agesq)
table(t92$educ, useNA="always")
summary(t92$inc) # moderate missigness
table(t92$minority, useNA="always")
table(t92$prot, useNA="always")
table(t92$cath, useNA="always")
# Variables not used
table(t92$married, useNA="always")
table(t92$own, useNA="always")
table(t92$hmake, useNA="always")
table(t92$ret, useNA="always")
table(t92$cler, useNA="always")
table(t92$prof, useNA="always")
table(t92$union, useNA="always")
table(t92$urban, useNA="always")
table(t92$east, useNA="always")
table(t92$south, useNA="always")
table(t92$west, useNA="always")

# 1996
t96 <- d[d$year==1996 & !is.na(d$presvote),]
table(t96$info, useNA="always")
summary(t96$objkn, useNA="always") # moderate missingness
table(t96$pid, useNA="always") 
table(t96$idedem, useNA="always") 
table(t96$iderep, useNA="always") 
table(t96$ideself, useNA="always") 
table(t96$iderepadv, useNA="always")
table(t96$evecon, useNA="always")
table(t96$female, useNA="always")
summary(t96$age)
summary(t96$agesq)
table(t96$educ, useNA="always")
summary(t96$inc) # moderate missigness
table(t96$minority, useNA="always")
table(t96$prot, useNA="always")
table(t96$cath, useNA="always")
# Variables not used
table(t96$married, useNA="always")
table(t96$own, useNA="always")
table(t96$hmake, useNA="always")
table(t96$ret, useNA="always")
table(t96$cler, useNA="always")
table(t96$prof, useNA="always")
table(t96$union, useNA="always")
table(t96$urban, useNA="always")
table(t96$east, useNA="always")
table(t96$south, useNA="always")
table(t96$west, useNA="always")

# 2000
t00 <- d[d$year==2000 & !is.na(d$presvote),]
table(t00$info, useNA="always")
summary(t00$objkn, useNA="always")
table(t00$pid, useNA="always") 
table(t00$idedem, useNA="always") 
table(t00$iderep, useNA="always") 
table(t00$ideself, useNA="always") # Big Missingness  
table(t00$iderepadv, useNA="always") # Big Missingness
table(t00$evecon, useNA="always")
table(t00$female, useNA="always")
summary(t00$age)
summary(t00$agesq)
table(t00$educ, useNA="always")
summary(t00$inc) # moderate missigness
table(t00$minority, useNA="always")
table(t00$prot, useNA="always")
table(t00$cath, useNA="always")
# Variables not used
table(t00$married, useNA="always")
table(t00$own, useNA="always")
table(t00$hmake, useNA="always")
table(t00$ret, useNA="always")
table(t00$cler, useNA="always")
table(t00$prof, useNA="always")
table(t00$union, useNA="always")
table(t00$urban, useNA="always")
table(t00$east, useNA="always")
table(t00$south, useNA="always")
table(t00$west, useNA="always")

# 2004
t04 <- d[d$year==2004 & !is.na(d$presvote),]
table(t04$info, useNA="always")
summary(t04$objkn, useNA="always") 
table(t04$pid, useNA="always") 
table(t04$idedem, useNA="always") 
table(t04$iderep, useNA="always") 
table(t04$ideself, useNA="always") 
table(t04$iderepadv, useNA="always")
table(t04$evecon, useNA="always")
table(t04$female, useNA="always")
summary(t04$age)
summary(t04$agesq)
table(t04$educ, useNA="always")
summary(t04$inc) # moderate missigness
table(t04$minority, useNA="always")
table(t04$prot, useNA="always")
table(t04$cath, useNA="always")
# Variables not used
table(t04$married, useNA="always")
table(t04$own, useNA="always")
table(t04$hmake, useNA="always")
table(t04$ret, useNA="always")
table(t04$cler, useNA="always")
table(t04$prof, useNA="always")
table(t04$union, useNA="always")
table(t04$urban, useNA="always")
table(t04$east, useNA="always")
table(t04$south, useNA="always")
table(t04$west, useNA="always")

# 2008
t08 <- d[d$year==2008 & !is.na(d$presvote),]
table(t08$info, useNA="always")
summary(t08$objkn, useNA="always")
table(t08$pid, useNA="always") 
table(t08$idedem, useNA="always") 
table(t08$iderep, useNA="always") 
table(t08$ideself, useNA="always") 
table(t08$iderepadv, useNA="always")
table(t08$evecon, useNA="always")
table(t08$female, useNA="always")
summary(t08$age)
summary(t08$agesq)
table(t08$educ, useNA="always")
summary(t08$inc) # moderate missigness
table(t08$minority, useNA="always")
table(t08$prot, useNA="always")
table(t08$cath, useNA="always")
# Variables not used
table(t08$married, useNA="always")
table(t08$own, useNA="always")
table(t08$hmake, useNA="always")
table(t08$ret, useNA="always")
table(t08$cler, useNA="always")
table(t08$prof, useNA="always")
table(t08$union, useNA="always")
table(t08$urban, useNA="always")
table(t08$east, useNA="always")
table(t08$south, useNA="always")
table(t08$west, useNA="always")

# 2012
t12 <- d[d$year==2012 & !is.na(d$presvote),]
table(t12$info, useNA="always") # Huge missingness (due to online sample)
summary(t12$objkn, useNA="always")
table(t12$pid, useNA="always") 
table(t12$idedem, useNA="always") 
table(t12$iderep, useNA="always") 
table(t12$ideself, useNA="always") 
table(t12$iderepadv, useNA="always")
table(t12$evecon, useNA="always")
table(t12$female, useNA="always")
summary(t12$age)
summary(t12$agesq)
table(t12$educ, useNA="always")
summary(t12$inc) # moderate missigness
table(t12$minority, useNA="always")
table(t12$prot, useNA="always")
table(t12$cath, useNA="always")
# Variables not used
table(t12$married, useNA="always")
table(t12$own, useNA="always")
table(t12$hmake, useNA="always")
table(t12$ret, useNA="always")
table(t12$cler, useNA="always")
table(t12$prof, useNA="always")
table(t12$union, useNA="always")
table(t12$urban, useNA="always")
table(t12$east, useNA="always")
table(t12$south, useNA="always")
table(t12$west, useNA="always")

# 2016
t16 <- d[d$year==2016 & !is.na(d$presvote),]
table(t16$info, useNA="always") # Huge missingness (due to online sample?)
table(t16$info, t16$mode, useNA="always")
summary(t16$objkn, useNA="always")
table(t16$pid, useNA="always") 
table(t16$idedem, useNA="always") 
table(t16$iderep, useNA="always") 
table(t16$ideself, useNA="always") 
table(t16$iderepadv, useNA="always")
table(t16$evecon, useNA="always")
table(t16$female, useNA="always")
summary(t16$age)
summary(t16$agesq)
table(t16$educ, useNA="always")
summary(t16$inc) # moderate missigness
table(t16$minority, useNA="always")
table(t16$prot, useNA="always")
table(t16$cath, useNA="always")
# Variables not used
table(t16$married, useNA="always")
table(t16$own, useNA="always")
table(t16$hmake, useNA="always")
table(t16$ret, useNA="always")
table(t16$cler, useNA="always")
table(t16$prof, useNA="always")
table(t16$union, useNA="always")
table(t16$urban, useNA="always")
table(t16$east, useNA="always")
table(t16$south, useNA="always")
table(t16$west, useNA="always")

