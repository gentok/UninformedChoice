#################################################################################
## File Name: CCES_analysis1.R                                               ##
## Date: 5 May 2019                                                           ##
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

## For Analysis
#library(mlogit)
#source("codes/cl.mlogit.R")
library(ggplot2)
library(lmtest)
library(multiwayvcov)
library(estvis)
library(grid)

## Custom Functions
source("codes_cces/CCES_analysis0_functions.R")

# Load Data
load("codes_cces/processing/CCES08_analysis2.rda")
a08 <- a
load("codes_cces/processing/CCES16_analysis2.rda")
a16 <- a

a <- rbind(a08,a16)

# Plot
p <- ggplot(a, aes(x=pvi,y=Mean)) + 
  geom_hline(aes(yintercept=0.5), colour="gray50") + 
  geom_line(aes(linetype=as.factor(knidx)),size=0.7) + 
  geom_ribbon(aes(ymin=lowerCI,ymax=upperCI, fill=as.factor(knidx)), alpha=0.5) + 
  facet_grid(. ~ context + year, scales = "free_x",
             labeller = label_bquote(cols = .(year))) + 
  scale_linetype_manual(name="", values = c(1,2),
                        labels=c("Uninformed","Informed")) + 
  scale_fill_brewer(name="", palette=2, type="qual",
                    labels = c("Uninformed","Informed")) + 
  scale_x_continuous(breaks=c(-10,0,10)) + 
  xlab("PVI Score (5 percentile to 95 percentile)") + 
  ylab("Average Predicted Probability \nof Republican Vote") + 
  coord_cartesian(ylim=c(0.35,0.65)) + 
  theme_classic() + 
  theme(legend.position="bottom",
        strip.text.x = element_text(face="bold", size=11))
p

addstriplabel <- function(p) {
  
  require(grid)
  require(gtable)
  
  # Labels 
  labelT1 = "State PVI"
  labelT2 = "County PVI"
  
  # Get the ggplot grob
  z <- ggplotGrob(p)
  
  # Get the positions of the strips in the gtable: t = top, l = left, ...
  posT1 <- subset(z$layout, grepl("strip-t", name), select = t:r)[1:2,]
  posT2 <- subset(z$layout, grepl("strip-t", name), select = t:r)[3:4,]
  
  # Add a new row on top of current top strips
  height <- unit(0.65,"cm") #z$heights[min(posT$t)]  # height of current top strips
  z <- gtable_add_rows(z, height, min(posT1$t)-1)
  
  # Construct the new strip grobs
  stripT1 <- gTree(name = "Strip_top", children = gList(
    rectGrob(gp = gpar(col = "black", fill = "white")),
    textGrob(labelT1, gp = gpar(fontsize = 11, col = "black", fontface="bold"))))
  stripT2 <- gTree(name = "Strip_top", children = gList(
    rectGrob(gp = gpar(col = "black", fill = "white")),
    textGrob(labelT2, gp = gpar(fontsize = 11, col = "black", fontface="bold"))))
  
  # Position the grobs in the gtable
  z <- gtable_add_grob(z, stripT1, t = min(posT1$t), l = min(posT1$l), r = max(posT1$r), 
                       name = "strip-top")
  z <- gtable_add_grob(z, stripT2, t = min(posT2$t), l = min(posT2$l), r = max(posT2$r), 
                       name = "strip-top")
  # Add small gaps between strips
  z <- gtable_add_rows(z, unit(1/5, "line"), min(posT1$t))
  
  return(z)
}

pz <- addstriplabel(p)

grid.draw(pz)

# Save
ggsave("outputs/ccespredplot.png", pz, width=7, height=4.5)
