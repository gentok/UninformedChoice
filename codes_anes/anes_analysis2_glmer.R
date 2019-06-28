#' ---
#' title: "Analyze ANES Data"
#' author: "Gento Kato"
#' date: "Apr 18, 2019"
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
source(paste(projdir, "codes_anes/anes_analysis0_functions.R", sep="/"))
source(paste(projdir, "codes_anes/edvreg.R", sep="/"))
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

d$iderepadv <- (d$iderepadv + 6)/12
d$evecon <- (d$evecon + 2)/4
d$age <- (d$age)/100
d$agesq <- d$age^2
d$educ <- d$educ/18
d$inc <- d$inc

# Data subsets with Target Respondents

d1 <- d[d$dropalways==0 & !is.na(d$presvote),]
d1$infoph <- d1$info
d1 <- na.omit(d1[,c(all.vars(f1x),"w")])

d2 <- d[d$dropifobjkn==0 & !is.na(d$presvote),] 
d2$infoph <- (d2$objkn + 2)/4
d2 <- na.omit(d2[,c(all.vars(f1x),"w")])

#'
#' ## Change Weight to Equalize the Contribution from Each Year
#'

# Make Every Year to Have 1000 Respondents
(rawn <- sapply(unique(d1$year), function(k) sum(d1$w[d1$year==k])))
(wy <- 1000/rawn)
d1$w <- wy[match(d1$year,seq(1972,2016,4))]*d1$w
sapply(unique(d1$year), function(k) sum(d1$w[d1$year==k]))

(rawn <- sapply(unique(d2$year), function(k) sum(d2$w[d2$year==k])))
(wy <- 1000/rawn)
d2$w <- wy[match(d2$year,seq(1972,2016,4))]*d2$w
sapply(unique(d2$year), function(k) sum(d2$w[d2$year==k]))

#'
#' ## Run Mixed Effect Logistic Regression
#'

## Subjective Knowledge

# Unweighted
m1x <- func_logitml(d1,f1x,FALSE)
summary(m1x)

# Weighted
m1xw <- func_logitml(d1,f1x,TRUE)
summary(m1xw)

## Objective Knowledge

# Unweighted
m2x <- func_logitml(d2,f1x,FALSE)
summary(m2x)

# Weighted
m2xw <- func_logitml(d2,f1x,TRUE)
summary(m2xw)

library(texreg)
screenreg(list(m1x,m2x), single.row=TRUE)

#'
#' # Save Workspace
#'
save.image(paste(projdir, "/codes_anes/processing/anes_analysis1_logit.rda", sep=""))

