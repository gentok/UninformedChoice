#################################################################################
## File Name: CCES16_descriptives.R                                            ##
## Date: 20 Feb 2018                                                           ##
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
#plotdir <- "papers/figures"

## For Analysis
library(ggplot2)
library(lmtest)
library(multiwayvcov)
library(estvis)
library(gridExtra)

## Custom Functions
source("codes/CCES_analysis0_functions.R")

## Initial Data Location
d08loc <- "data/cces08s_cxt2.rds"; wtname <- "wt"
d16loc <- "data/cces16s_cxt2.rds"; wtname <- "wt_post"

###############
## Load Data ##
###############

d08 <- readRDS(d08loc)
d08 <- d08[-which(d08$state %in% c("Alaska","District of Columbia")),]
#d08 <- d08[d08$pidstr <= 1,]
d08 <- d08[d08$vpresx %in% c(1,2),]
d08$wtv <- d08$wt

d16 <- readRDS(d16loc)
d16 <- d16[-which(d16$state %in% c("Alaska","District of Columbia")),]
#d16 <- d16[d16$pidstr <= 1,]
d16 <- d16[d16$vpresx %in% c(1,2),]
d16$wtv <- d16$wt_post

##################
## Descriptives ##
##################

d_kn <- rbind(d08[,c("year","knidx","wtv")],
               d16[,c("year","knidx","wtv")])
d_kn <- na.omit(d_kn)
knidxgraph <- ggplot(d_kn, aes(x=knidx)) + 
  geom_histogram(aes(y=..density.., weight=wtv), 
                 colour="gray20", fill="gray90", binwidth = 0.15) + 
  facet_grid(.~year) + 
  xlab("Knowledge Levels") + ylab("Density") + 
  scale_x_continuous(breaks=c(0,0.5,1)) + 
  theme_classic()
knidxgraph

ggsave(paste(projdir, "/outputs/cces_knowdist.png", sep=""),
       knidxgraph, w=6.5, h=4)

pvi08p <- readRDS("data/PVI08_president.rds")
pvi08h <- readRDS("data/PVI08_house.rds")
pvi16p <- readRDS("data/PVI16_president.rds")
pvi16h <- readRDS("data/PVI16_house.rds")


pvip <- data.frame(pvi08p,pvi16p)
pvip_s <- aggregate(pvip[c("statePVI","statePVI.1")], by=list(State=pvip$State),mean)

a <- ggplot(pvip_s) + 
  geom_density(aes(x=statePVI, fill="2008", col="2008"), alpha=0.7) + 
  geom_density(aes(x=statePVI.1, fill="2016", col="2016"), alpha=0.7) + 
  scale_fill_brewer(name="Year", palette=3, type="qual") +
  scale_color_brewer(name="Year", palette=3, type="qual") +
  xlab("State PVI (President)") + ylab("Density") +
  estvis.theme() + theme(legend.position=c(0.2,0.7))
b <- ggplot(pvip) + 
  geom_density(aes(x=countyPVI, fill="2008", col="2008"), alpha=0.7) + 
  geom_density(aes(x=countyPVI.1, fill="2016", col="2016"), alpha=0.7) + 
  scale_fill_brewer(name="Year", palette=3, type="qual") +
  scale_color_brewer(name="Year", palette=3, type="qual") +
  xlab("County PVI (President)") + ylab("Density") +
  estvis.theme() + theme(legend.position=c(0.2,0.7))

pvi08h_s <- aggregate(pvi08h[c("statePVI")], by=list(State=pvi08h$State),mean)
pvi16h_s <- aggregate(pvi16h[c("statePVI")], by=list(State=pvi16h$State),mean)

c <- ggplot() + 
  geom_density(data=pvi08h_s, aes(x=statePVI, fill="2008", col="2008"), alpha=0.7) + 
  geom_density(data=pvi16h_s, aes(x=statePVI, fill="2016", col="2016"), alpha=0.7) + 
  scale_fill_brewer(name="Year", palette=3, type="qual") +
  scale_color_brewer(name="Year", palette=3, type="qual") +
  xlab("House State PVI") + ylab("Density") +
  estvis.theme() + theme(legend.position=c(0.2,0.7))

e <- ggplot() + 
  geom_density(data=pvi08h, aes(x=districtPVI, fill="2008", col="2008"), alpha=0.7) + 
  geom_density(data=pvi16h, aes(x=districtPVI, fill="2016", col="2016"), alpha=0.7) + 
  scale_fill_brewer(name="Year", palette=3, type="qual") +
  scale_color_brewer(name="Year", palette=3, type="qual") +
  xlab("House District PVI") + ylab("Density") +
  estvis.theme() + theme(legend.position=c(0.2,0.7))

pvigraph1 <- arrangeGrob(a,b, nrow=1)
png_save(pvigraph1, file="outputs/cces_pvidist.png", h=400, w=700)

pvigraph2 <- arrangeGrob(a,b,c,e, nrow=2)
grid.arrange(pvigraph2)

#################
## Save Output ##
#################

save(knidxgraph, pvigraph2, file="outputs/CCES_descriptives2.rda")

