#################################################################################
## File Name: CCES16_analysis2.R                                               ##
## Date: 21 Mar 2019                                                           ##
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

## Set Working Directory (Automatically or Manually) ##
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)); #setwd("../") #In RStudio
projdir <- find_root(has_file("README.md")); projdir; setwd(projdir) #In Atom
#setwd("C:/GoogleDrive/Projects/Uninformed_Choice")
plotdir <- "papers/figures"

## For Analysis
#library(mlogit)
#source("codes/cl.mlogit.R")
library(ggplot2)
library(lmtest)
library(multiwayvcov)
library(estvis)

## Custom Functions
source("codes/CCES_analysis0_functions.R")

# Load Data
load("outputs/CCES16_analysis1.rda")
#load("outputs/CCES16_analysis2.rda")

########################
## Aggregated Outcome ##
########################

# Presidential Election ############################################
(sq <- quantile(d$pvi_state, probs=c(0.05,0.95), na.rm=TRUE))
(cq <- quantile(d$pvi_county, probs=c(0.05,0.95), na.rm=TRUE))
d2v_p16 <- na.omit(d[,c(all.vars(m2v_p16), wtname)])
simval_p16 <- data.frame(pvi=c(rep(seq(sq[1],sq[2],length=10),2),
                               rep(seq(cq[1],cq[2],length=10),2)),
                         context=factor(rep(c("State PVI","County PVI"), each=20),
                                        levels=c("State PVI","County PVI")),
                         knidx = rep(rep(c(0,1), each=10),2) )

dag2v_p16_simu <- function(k) {
  dtemp <- d2v_p16
  geo <- k["context"]
  if (geo=="State PVI") dtemp$pvi_state <- as.numeric(k["pvi"])
  if (geo=="County PVI") dtemp$pvi_county <- as.numeric(k["pvi"]) 
  dtemp$knidx <- as.numeric(k["knidx"])
  
  agp2v_p16_state <- simu_pred(e2v_p16, dtemp, vcov.est=e2vr_p16.vcov,
                               iterate.num = 500) 
  temp <- cbind(agp2v_p16_state$predsum,d2v_p16[wtname])
  
  rowSums(apply(temp, 1, function(k) k[1:5]*k[6]))/sum(temp[wtname])
}
dagp2v_p16 <- t(pbapply(simval_p16, 1, dag2v_p16_simu))
dagp2v_p16_temp <- dagp2v_p16
dagp2v_p16 <- as.data.frame(dagp2v_p16)
dagp2v_p16x <- data.frame(dagp2v_p16, simval_p16)
dagp2v_p16x$year <- 2016

# Plot
gagp2v_p16 <- ggplot(dagp2v_p16x, aes(x=pvi,y=Mean)) + 
  geom_hline(aes(yintercept=0.5), colour="gray50") + 
  geom_line(aes(linetype=as.factor(knidx)),size=0.5) + 
  geom_ribbon(aes(ymin=lowerCI,ymax=upperCI, fill=as.factor(knidx)), alpha=0.5) + 
  facet_grid(.~context, scales = "free_x") + 
  scale_linetype_manual(name="", values = c(1,2),
                        labels=c("Uninformed","Informed")) + 
  scale_fill_brewer(name="", palette=2, type="qual",
                    labels = c("Uninformed","Informed")) + 
  xlab("Partisan Voter Index") + ylab("Probability of Republican Vote") + 
  coord_cartesian(ylim=c(0.35,0.75)) + 
  theme_classic()
gagp2v_p16

# House Election ###########################################################
(sq <- quantile(d$hpvi2_state, probs=c(0.05,0.95), na.rm=TRUE))
(cq <- quantile(d$hpvi2_district, probs=c(0.05,0.95), na.rm=TRUE))
d2v_h16 <- na.omit(d[,c(all.vars(m2v_h16), wtname)])
simval_h16 <- data.frame(hpvi2=c(rep(seq(sq[1],sq[2],length=10),2),
                               rep(seq(cq[1],cq[2],length=10),2)),
                         context=factor(rep(c("House State PVI","House District PVI"), each=20),
                                        levels=c("House State PVI","House District PVI")),
                         knidx = rep(rep(c(0,1), each=10),2) )

dag2v_h16_simu <- function(k) {
  dtemp <- d2v_h16
  geo <- k["context"]
  if (geo=="House State PVI") dtemp$hpvi2_state <- as.numeric(k["hpvi2"])
  if (geo=="House District PVI") dtemp$hpvi2_district <- as.numeric(k["hpvi2"]) 
  dtemp$knidx <- as.numeric(k["knidx"])
  
  agp2v_h16_state <- simu_pred(e2v_h16, dtemp, vcov.est=e2vr_h16.vcov,
                               iterate.num = 500) 
  temp <- cbind(agp2v_h16_state$predsum,d2v_h16[wtname])
  
  rowSums(apply(temp, 1, function(k) k[1:5]*k[6]))/sum(temp[wtname])
}
dagp2v_h16 <- t(pbapply(simval_h16, 1, dag2v_h16_simu))
dagp2v_h16_temp <- dagp2v_h16
dagp2v_h16 <- as.data.frame(dagp2v_h16)
dagp2v_h16x <- data.frame(dagp2v_h16, simval_h16)
dagp2v_h16x$year <- 2016

# Plot
gagp2v_h16 <- ggplot(dagp2v_h16x, aes(x=hpvi2,y=Mean)) + 
  geom_hline(aes(yintercept=0.5), colour="gray50") + 
  geom_line(aes(linetype=as.factor(knidx)),size=0.5) + 
  geom_ribbon(aes(ymin=lowerCI,ymax=upperCI, fill=as.factor(knidx)), alpha=0.5) + 
  facet_grid(.~context, scales = "free_x") + 
  scale_linetype_manual(name="", values = c(1,2),
                        labels=c("Uninformed","Informed")) + 
  scale_fill_brewer(name="", palette=2, type="qual",
                    labels = c("Uninformed","Informed")) + 
  xlab("House Partisan Voter Index") + ylab("Probability of Republican Vote") + 
  coord_cartesian(ylim=c(0.35,0.75)) + 
  theme_classic()
gagp2v_h16

###############
## Save Data ##
###############

save(d2v_p16, simval_p16, dag2v_p16_simu, dagp2v_p16, dagp2v_p16x, gagp2v_p16,  
     d2v_h16, simval_h16, dag2v_h16_simu, dagp2v_h16, dagp2v_h16x, gagp2v_h16,
     wtname, file="outputs/CCES16_analysis2.rda")

