#################################################################################
## File Name: CCES08_analysis1mrp.R                                            ##
## Date: 15 APr 2019                                                           ##
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

## Custom Functions
source("codes_vx/CCES_analysis0_functions.R")

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
d$age <- d$age/10
d$agesq <- (d$age/10)^2
d$income <- (d$income - 1)/16
d$edu <- d$edu/5

# Limit Data for Two-party Voters
d$vpres <- as.numeric(factor(d$vpres,levels=c("Democrat","Republican")))-1
d <- d[complete.cases(cbind(d$vpres,d$state,d$county)),]

#########################
## Multiple Imputation ##
#########################

# ID Variable
IDS <- c("id","state","county")
# Variables Index
varIndex <- c(all.vars(fp3),IDS,"w")

# Imputate
set.seed(45678)
dimp5 <- amelia(d[,varIndex], idvars = IDS, 
               noms = c("female", "black","latino","asian","other","bornagain"),
               m = 5)
summary(dimp5)

##################
## Try Analysis ##
##################

## Multilevel Models ###########################################

# load("codes_vx/processing/CCES08_analysis1mer.rda")

# Only Social Context
mp1x <- func_logitml(dimp5$imputations,fp1x)
gdp1x <- gd_export(mp1x,d,type=2)
gp1x <- plot_coef(list(gdp1x$gd0[,2:4],gdp1x$gd1[,2:4]),
                 m.names = c("Uninformed","Informed"), #odds=T,
                 title = "Impact on Presidential Vote Choice 2008 \n(1=McCain, 0=Obama)",
                 custom.footnote = extract_gofchr(mp1$imp1),
                 custom.variable.names = vn1[-1])

# With Individual Preference
mp2x <- func_logitml(dimp5$imputations,fp2x)
gdp2x <- gd_export(mp2x,d,type=2)
gp2x <- plot_coef(list(gdp2x$gd0[,2:4],gdp2x$gd1[,2:4]),
                  m.names = c("Uninformed","Informed"), #odds=T,
                  title = "Impact on Presidential Vote Choice 2008 \n(1=McCain, 0=Obama)",
                  custom.footnote = extract_gofchr(mp2$imp1),
                  custom.variable.names = vn2[-1])

# With Demographic Controls
mp3x <- func_logitml(dimp5$imputations,fp3x)
gdp3x <- gd_export(mp3x,d,type=2)
gp3x <- plot_coef(list(gdp3x$gd0[,2:4],gdp3x$gd1[,2:4]),
                  m.names = c("Uninformed","Informed"), #odds=T,
                  title = "Impact on Presidential Vote Choice 2008 \n(1=McCain, 0=Obama)",
                  custom.footnote = extract_gofchr(mp3$imp1),
                  custom.variable.names = vn3[-1])

# Summary Tables
class(mp1x) <- c("modlist",class(mp1x))
class(mp2x) <- c("modlist",class(mp2x))
class(mp3x) <- c("modlist",class(mp3x))

vnt <- c(vn3[1],"Knowledge",
         vn3[2:3],paste0("Knowledge*",sub(" \\(.*\\)","",vn3[2:3])),
         vn3[4:6],paste0("Knowledge*",sub(" \\(.*\\)","",vn3[4:6])),
         vn3[7:16],paste0("Knowledge*",sub(" \\(.*\\)","",vn3[7:16])))

(tp0x <- screenreg(list(mp1x,mp2x,mp3x),
                  custom.coef.names = vnt,
                  caption = "Impact on Presidential Vote Choice",
                  single.row = TRUE))
tpx <- texreg(list(mp1x,mp2x,mp3x),
             custom.coef.names = vnt,
             caption = "Impact on Presidential Vote Choice",
             booktabs=TRUE,dcolumn=TRUE,
             single.row = TRUE)

# Save output
save(dimp5,mp1x,gdp1x,gp1x,mp2x,gdp2x,gp2x,mp3x,gdp3x,gp3x,
     vnt,tp0x,tpx, 
     file="codes_vx/processing/CCES08_analysis1mer.rda")
