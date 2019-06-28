#' ---
#' title: "Descriptive Statistics for ANES Data"
#' author: "Gento Kato"
#' date: "June 25, 2019"
#' ---
#' 

#' ## Preparation 
#' 
#' Required Packages
#' 
require(readstata13)
require(pbapply)
require(ggplot2)
require(ggrepel)
library(Hmisc)

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

# Data subsets with Target Respondents
# Subjective Knowledge
d1 <- d[d$dropalways==0 & !is.na(d$presvote),]
d1$infoph <- d1$info
# Objective Knowledge
d2 <- d[d$dropifobjkn==0 & !is.na(d$presvote),] 
d2$infoph <- (d2$objkn + 2)/4

#'
#' ## Interviewer's Rating
#'

tmp <- data.frame(y = as.numeric(t(t(table(d1$infoph, d1$year))/colSums(table(d1$infoph, d1$year)))),
                  x = c(0.05,0.2,0.5,0.8,0.95),
                  year=rep(seq(1972,2016,4),each=5))

p <- ggplot(tmp, aes(x=as.factor(x),y=y)) + 
  geom_col() + facet_wrap(tmp$year) + 
  ylab("Proportion") + xlab("Interviewer's Rating of Knowledge") + 
  estvis.theme() + 
  theme(strip.text = element_text(size=10))

p

png_save(p, w=800, h=500, file = paste0(projdir, "/outputs/anes_sbjkndist.png"))

#'
#' ## Objective Knowledge
#'

p <- ggplot(d2, aes(x=infoph, y=..density..)) + 
  geom_histogram(binwidth=0.1) + facet_wrap(~year) + 
  ylab("Density") + 
  xlab("Objective Knowledge Score") + 
  coord_cartesian(xlim=c(0,1)) + 
  estvis.theme() + 
  theme(strip.text = element_text(size=10),
        axis.text.x = element_text(angle=30, hjust=0.75))

p

png_save(p, w=800, h=500, file = paste0(projdir, "/outputs/anes_objkndist.png"))
