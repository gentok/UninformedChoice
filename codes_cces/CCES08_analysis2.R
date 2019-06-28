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
#plotdir <- "papers/figures"

## For Analysis
#library(mlogit)
#source("codes/cl.mlogit.R")
library(ggplot2)
library(lmtest)
library(multiwayvcov)
library(estvis)
library(pbapply)

## Custom Functions
source("codes_cces/CCES_analysis0_functions.R")

# Load Data
load("codes_cces/processing/CCES08_analysis1logit.rda")

########################
## Aggregated Outcome ##
########################

# Presidential Election ############################################

# Set Range of PVI values
(sq <- quantile(dimp5$imputations$imp1$pvi_state, probs=c(0.05,0.95), na.rm=TRUE))
(cq <- quantile(dimp5$imputations$imp1$pvi_county, probs=c(0.05,0.95), na.rm=TRUE))

# Simulate First Difference
a1 <- gensimudata(mp3$imp1,dimp5$imputations$imp1,sq,cq,2008)
a2 <- gensimudata(mp3$imp2,dimp5$imputations$imp2,sq,cq,2008)
a3 <- gensimudata(mp3$imp3,dimp5$imputations$imp3,sq,cq,2008)
a4 <- gensimudata(mp3$imp4,dimp5$imputations$imp4,sq,cq,2008)
a5 <- gensimudata(mp3$imp5,dimp5$imputations$imp5,sq,cq,2008)
a <- data.frame(Mean = rowMeans(cbind(a1$Mean,a2$Mean,a3$Mean,a4$Mean,a5$Mean)),
                Median = rowMeans(cbind(a1$Median,a2$Median,a3$Median,a4$Median,a5$Median)),
                SE = rowMeans(cbind(a1$SE,a2$SE,a3$SE,a4$SE,a5$SE)),
                lowerCI = rowMeans(cbind(a1$lowerCI,a2$lowerCI,a3$lowerCI,a4$lowerCI,a5$lowerCI)),
                upperCI = rowMeans(cbind(a1$upperCI,a2$upperCI,a3$upperCI,a4$upperCI,a5$upperCI)),
                a1[,6:9])
a$pvi <- a$pvi*10

# Plot
p <- ggplot(a, aes(x=pvi,y=Mean)) + 
  geom_hline(aes(yintercept=0.5), colour="gray50") + 
  geom_line(aes(linetype=as.factor(knidx)),size=0.5) + 
  geom_ribbon(aes(ymin=lowerCI,ymax=upperCI, fill=as.factor(knidx)), alpha=0.5) + 
  facet_grid(.~context, scales = "free_x") + 
  scale_linetype_manual(name="", values = c(1,2),
                        labels=c("Uninformed","Informed")) + 
  scale_fill_brewer(name="", palette=2, type="qual",
                    labels = c("Uninformed","Informed")) + 
  xlab("Partisan Voter Index") + ylab("Probability of Republican Vote") + 
  coord_cartesian(ylim=c(0.3,0.6)) + 
  theme_classic()
p

###############
## Save Data ##
###############

save(a1,a2,a3,a4,a5,a,p, file="codes_cces/processing/CCES08_analysis2.rda")
#load("codes_cces/processing/CCES08_analysis2.rda")
saveRDS(a, paste(projdir,"codes_cces/processing/simu08.rds", sep="/"))
