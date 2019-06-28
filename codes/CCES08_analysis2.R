#################################################################################
## File Name: CCES08_analysis2.R                                               ##
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
library(pbapply)

## Custom Functions
source("codes/CCES_analysis0_functions.R")

# Load Data
load("outputs/CCES08_analysis1.rda")
#load("outputs/CCES08_analysis2.rda")

########################
## Aggregated Outcome ##
########################

# Presidential Election ############################################
(sq <- quantile(d$pvi_state, probs=c(0.05,0.95), na.rm=TRUE))
(cq <- quantile(d$pvi_county, probs=c(0.05,0.95), na.rm=TRUE))
d2v_p08 <- na.omit(d[,c(all.vars(m2v_p08), wtname)])
simval_p08 <- data.frame(pvi=c(rep(seq(sq[1],sq[2],length=10),2),
                               rep(seq(cq[1],cq[2],length=10),2)),
                         context=factor(rep(c("State PVI","County PVI"), each=20),
                                        levels=c("State PVI","County PVI")),
                         knidx = rep(rep(c(0,1), each=10),2) )
dag2v_p08_simu <- function(k) {
  dtemp <- d2v_p08
  geo <- k["context"]
  if (geo=="State PVI") dtemp$pvi_state <- as.numeric(k["pvi"])
  if (geo=="County PVI") dtemp$pvi_county <- as.numeric(k["pvi"]) 
  dtemp$knidx <- as.numeric(k["knidx"])
  
  agp2v_p08_state <- simu_pred(e2v_p08, dtemp, vcov.est=e2vr_p08.vcov,
                               iterate.num = 500) 
  temp <- cbind(agp2v_p08_state$predsum,d2v_p08[wtname])
  
  rowSums(apply(temp, 1, function(k) k[1:5]*k[6]))/sum(temp[wtname])
}
dagp2v_p08 <- t(pbapply(simval_p08, 1, dag2v_p08_simu))
dagp2v_p08_temp <- dagp2v_p08
dagp2v_p08 <- as.data.frame(dagp2v_p08)
dagp2v_p08x <- data.frame(dagp2v_p08, simval_p08)
dagp2v_p08x$year <- 2008

# Plot
gagp2v_p08 <- ggplot(dagp2v_p08x, aes(x=pvi,y=Mean)) + 
  geom_hline(aes(yintercept=0.5), colour="gray50") + 
  geom_line(aes(linetype=as.factor(knidx)),size=0.5) + 
  geom_ribbon(aes(ymin=lowerCI,ymax=upperCI, fill=as.factor(knidx)), alpha=0.5) + 
  facet_grid(.~context, scales = "free_x") + 
  scale_linetype_manual(name="", values = c(1,2),
                        labels=c("Uninformed","Informed")) + 
  scale_fill_brewer(name="", palette=2, type="qual",
                    labels = c("Uninformed","Informed")) + 
  xlab("Partisan Voter Index") + ylab("Probability of Republican Vote") + 
  coord_cartesian(ylim=c(0.25,0.65)) + 
  theme_classic()
gagp2v_p08

# House Election ###########################################################
(sq <- quantile(d$hpvi2_state, probs=c(0.05,0.95), na.rm=TRUE))
(cq <- quantile(d$hpvi2_district, probs=c(0.05,0.95), na.rm=TRUE))
d2v_h08 <- na.omit(d[,c(all.vars(m2v_h08), wtname)])
simval_h08 <- data.frame(hpvi2=c(rep(seq(sq[1],sq[2],length=10),2),
                                 rep(seq(cq[1],cq[2],length=10),2)),
                         context=factor(rep(c("House State PVI","House District PVI"), each=20),
                                        levels=c("House State PVI","House District PVI")),
                         knidx = rep(rep(c(0,1), each=10),2) )

dag2v_h08_simu <- function(k) {
  dtemp <- d2v_h08
  geo <- k["context"]
  if (geo=="House State PVI") dtemp$hpvi2_state <- as.numeric(k["hpvi2"])
  if (geo=="House District PVI") dtemp$hpvi2_district <- as.numeric(k["hpvi2"]) 
  dtemp$knidx <- as.numeric(k["knidx"])
  
  agp2v_h08_state <- simu_pred(e2v_h08, dtemp, vcov.est=e2vr_h08.vcov,
                               iterate.num = 500) 
  temp <- cbind(agp2v_h08_state$predsum,d2v_h08[wtname])
  
  rowSums(apply(temp, 1, function(k) k[1:5]*k[6]))/sum(temp[wtname])
}
dagp2v_h08 <- t(pbapply(simval_h08, 1, dag2v_h08_simu))
dagp2v_h08_temp <- dagp2v_h08
dagp2v_h08 <- as.data.frame(dagp2v_h08)
dagp2v_h08x <- data.frame(dagp2v_h08, simval_h08)
dagp2v_h08x$year <- 2008

# Plot
gagp2v_h08 <- ggplot(dagp2v_h08x, aes(x=hpvi2,y=Mean)) + 
  geom_hline(aes(yintercept=0.5), colour="gray50") + 
  geom_line(aes(linetype=as.factor(knidx)),size=0.5) + 
  geom_ribbon(aes(ymin=lowerCI,ymax=upperCI, fill=as.factor(knidx)), alpha=0.5) + 
  facet_grid(.~context, scales = "free_x") + 
  scale_linetype_manual(name="", values = c(1,2),
                        labels=c("Uninformed","Informed")) + 
  scale_fill_brewer(name="", palette=2, type="qual",
                    labels = c("Uninformed","Informed")) + 
  xlab("House Partisan Voter Index") + ylab("Probability of Republican Vote") + 
  coord_cartesian(ylim=c(0.25,0.65)) + 
  theme_classic()
gagp2v_h08

###############
## Save Data ##
###############

save(d2v_p08, simval_p08, dag2v_p08_simu, dagp2v_p08, dagp2v_p08x, gagp2v_p08,  
     d2v_h08, simval_h08, dag2v_h08_simu, dagp2v_h08, dagp2v_h08x, gagp2v_h08,
     wtname, file="outputs/CCES08_analysis2.rda")
