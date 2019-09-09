#################################################################################
## File Name: CCES08_analysis1logit.R                                          ##
## Date: 24 Jul 2019                                                           ##
## Author: Gento Kato                                                          ##
## Project: Uninformed Choice                                                  ##
## Purpose: Analyze Data                                                       ##
#################################################################################

#################
## Preparation ##
#################

## Clear Workspace
rm(list=ls())

## Library Required Packages
library(rprojroot);library(questionr)

## Set Working Directory (Automatically) ##
if (rstudioapi::isAvailable()==TRUE) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path)); 
} 
projdir <- find_root(has_file("thisishome.txt"))
cat(paste("Working Directory Set to:\n",projdir))
setwd(projdir)
# For Plotting
plotdir <- "papers/figures"

## For Analysis
#library(mlogit)
#source("codes/cl.mlogit.R")
library(ggplot2)
library(lmtest)
library(multiwayvcov)
library(estvis)
library(texreg)
library(Amelia)
library(stargazer)

## Custom Functions
source("codes_cces/CCES_analysis0_functions.R")

## Initial Data Location
# dloc <- "data/ucdcces08s_cxt.rds"; wtname <- "wt"
dloc <- "data/cces08s_cxt.rds"; wtname <- "wt"


###############
## Load Data ##
###############

# Load
d <- readRDS(dloc)

# Define Weight Variable
d$w <- d[,wtname]

# Simple Rescaling
d$pvi_state <- d$pvi_state/10
d$pvi_county <- d$pvi_county/10
d$age <- d$age/100
d$agesq <- (d$age)^2
d$income <- (d$income - 1)/16
d$edu <- d$edu/5

# Limit Data to Two-party Voters
d$vpres <- as.numeric(factor(d$vpres,levels=c("Democrat","Republican")))-1
d$county[d$county==""] <- NA
d <- d[complete.cases(cbind(d$vpres,d$state,d$county)),]

## Check Scales
summary(d$pvi_state)
summary(d$pvi_county)
summary(d$idedistsq_prepadv)
table(d$pid)
table(d$evecon)
table(d$female)
table(d$age)
table(d$agesq)
table(d$black)
table(d$latino)
table(d$asian)
table(d$other)
table(d$income)
table(d$edu)
table(d$bornagain)

#########################
## Multiple Imputation ##
#########################

# ID Variable
IDS <- c("id","state","county")

# Variables Index
varIndex <- c(all.vars(fp3),IDS,"w")

# Bounds Index
boundsIndex <- rbind(c(5, -36, 36),
                     c(6, -3, 3),
                     c(7, -2, 2),
                     c(9, 0, 1),
                     c(10, 0, 1),
                     c(15, 0, 1),
                     c(16, 0, 1))

# Imputate
set.seed(45678)
dimp5 <- amelia(d[,varIndex], idvars = IDS, 
                noms = c("female", "black","latino","asian","other","bornagain"),
                bounds = boundsIndex,
                m = 5)
summary(dimp5)

##################
## Try Analysis ##
##################

#load("codes_cces/processing/CCES08_analysis1logit.rda")

## Standard Logit with Cluster-Robust SE ###########################################

# Only Social Context
mp1 <- func_logit(dimp5$imputations, fp1)
gdp1 <- gd_export(mp1,d)
gp1 <- plot_coef(list(gdp1$gd0[,2:4],gdp1$gd1[,2:4]),
                 m.names = c("Uninformed","Informed"), #odds=T,
                 title = "Impact on Presidential Vote Choice 2008 \n(1=McCain, 0=Obama)",
                 custom.footnote = gof_export(mp1),
                 custom.variable.names = vn1[-1])

# With Individual Preference
mp2 <- func_logit(dimp5$imputations, fp2)
gdp2 <- gd_export(mp2,d)
gp2 <- plot_coef(list(gdp2$gd0[,2:4],gdp2$gd1[,2:4]),
                 m.names = c("Uninformed","Informed"), #odds=T,
                 title = "Impact on Presidential Vote Choice 2008 \n(1=McCain, 0=Obama)",
                 custom.footnote = gof_export(mp2),
                 custom.variable.names = vn2[-1])

# With Demographic Controls
mp3 <- func_logit(dimp5$imputations, fp3)
gdp3 <- gd_export(mp3,d)
gp3 <- plot_coef(list(gdp3$gd0[,2:4],gdp3$gd1[,2:4]),
                 m.names = c("Uninformed","Informed"), #odds=T,
                 title = "Impact on Presidential Vote Choice 2008 \n(1=McCain, 0=Obama)",
                 custom.footnote = gof_export(mp3),
                 custom.variable.names = vn3[-1])

# Summary Tables
class(mp1) <- c("modlist",class(mp1))
class(mp2) <- c("modlist",class(mp2))
class(mp3) <- c("modlist",class(mp3))
mp1mat <- robusttab(mp1, d)
mp2mat <- robusttab(mp2, d)
mp3mat <- robusttab(mp3, d)

vnt <- c(vn3[1],"Knowledge",
  vn3[2:3],paste0("Knowledge*",sub(" \\(.*\\)","",vn3[2:3])),
  vn3[4:6],paste0("Knowledge*",sub(" \\(.*\\)|Economic ","",vn3[4:6])),
  vn3[7:16],paste0("Knowledge*",sub(" \\(.*\\)","",vn3[7:16])))

(tp0 <- screenreg(list(mp1,mp2,mp3),
          override.se = list(mp1mat$se,mp2mat$se,mp3mat$se),
          override.pvalues = list(mp1mat$p,mp2mat$p,mp3mat$p),
          custom.coef.names = vnt, digits = 3,
          caption = "Local Partisan Environment and Presidential Vote Choice",
          single.row = TRUE))
tp <- texreg(list(mp1,mp2,mp3),
             override.se = list(mp1mat$se,mp2mat$se,mp3mat$se),
             override.pvalues = list(mp1mat$p,mp2mat$p,mp3mat$p),
             custom.coef.names = vnt, digits = 3,
             booktabs=TRUE, dcolumn=TRUE, use.packages=FALSE, table=FALSE,
             single.row = TRUE, file="outputs/cces08tab.tex")

# Save output
save(dimp5,mp1,gdp1,gp1,mp2,gdp2,gp2,mp3,gdp3,gp3,
     mp1mat,mp2mat,mp3mat,vnt,tp0, 
     file="codes_cces/processing/CCES08_analysis1logit.rda")
