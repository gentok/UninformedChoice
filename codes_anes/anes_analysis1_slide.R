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
source(paste(projdir, "codes_anes/anes_analysis0_slide_functions.R", sep="/"))
source(paste(projdir, "codes_anes/edvreg.R", sep="/"))

#' Load Data

load("./codes_anes/processing/anes_analysis1_logit.rda")

#' 
#' Prep

# Background Theme
bgcolor <- rgb(245, 245, 245,maxColorValue = 255)
themeadd <- 
  theme(panel.background = element_rect(fill=bgcolor, colour="black"),
        plot.background = element_rect(fill=bgcolor, colour=bgcolor),
        legend.background = element_rect(fill=bgcolor, colour=bgcolor),
        strip.background = element_rect(fill=bgcolor, color="black"),
        legend.position = "bottom")

m1sq_pred_1992 <- export_predplot(m1sq_pred_1992,projdir,"m1sq_1992")

p <- m1sq_pred_1992$p + 
  theme(text = element_text(size=12),
        legend.title = element_text(size=12, face="bold"),
        legend.text = element_text(size=12, face="bold"),
        strip.text.x = element_text(size=14, face="bold"),
        axis.text.y = element_text(size=12, face="bold"),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text(size=13, face="bold")) + 
  themeadd

# Save
ggsave("./slides/pictures/anespredplot.png", p, width=7, height=5)

p <- export_predplot_informed(m1sq_pred_1992,projdir,"m1sq_1992")
