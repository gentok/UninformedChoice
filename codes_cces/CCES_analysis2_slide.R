#################################################################################
## File Name: CCES_analysis2_slide.R                                           ##
## Date: 30 Jun 2019                                                            ##
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
# load("codes_cces/processing/CCES08_analysis2.rda")
# a08 <- a
a08 <- readRDS("codes_cces/processing/simu08.rds")
# load("codes_cces/processing/CCES16_analysis2.rda")
# a16 <- a
a16 <- readRDS("codes_cces/processing/simu16.rds")

a <- rbind(a08,a16)

# Background Theme
bgcolor <- rgb(245, 245, 245,maxColorValue = 255)
themeadd <- 
  theme(panel.background = element_rect(fill=bgcolor, colour="black"),
        plot.background = element_rect(fill=bgcolor, colour=bgcolor),
        legend.background = element_rect(fill=bgcolor, colour=bgcolor),
        strip.background = element_rect(fill=bgcolor, color="black"),
        legend.position = "bottom")

# Adding Strip Label
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
    rectGrob(gp = gpar(col = bgcolor, fill = bgcolor)),
    textGrob(labelT1, gp = gpar(fontsize = 14, col = "black", fontface="bold"))))
  stripT2 <- gTree(name = "Strip_top", children = gList(
    rectGrob(gp = gpar(col = bgcolor, fill = bgcolor)),
    textGrob(labelT2, gp = gpar(fontsize = 14, col = "black", fontface="bold"))))
  
  # Position the grobs in the gtable
  z <- gtable_add_grob(z, stripT1, t = min(posT1$t), l = min(posT1$l), r = max(posT1$r), 
                       name = "strip-top")
  z <- gtable_add_grob(z, stripT2, t = min(posT2$t), l = min(posT2$l), r = max(posT2$r), 
                       name = "strip-top")
  # Add small gaps between strips
  z <- gtable_add_rows(z, unit(1/5, "line"), min(posT1$t))
  
  return(z)
}

# Plot
p <- ggplot(a, aes(x=pvi,y=Mean)) + 
  geom_hline(aes(yintercept=0.5), colour="gray50") + 
  geom_line(aes(linetype=as.factor(knidx)),size=1) + 
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
  theme(text = element_text(size=12),
        legend.text = element_text(size=12, face="bold"),
        strip.text = element_text(size=12, face="bold"),
        axis.text.y = element_text(size=12, face="bold"),
        axis.title = element_text(size=12, face="bold")) + 
  #theme(strip.text.x = element_text(face="bold", size=11)) + 
  themeadd
pz <- addstriplabel(p)
grid.draw(pz)

# Save
ggsave("./slides/pictures/ccespredplot.png", pz, width=8, height=5)

# Plot
p <- ggplot(a, aes(x=pvi,y=Mean)) + 
  geom_hline(aes(yintercept=0.5), colour="gray50") + 
  geom_line(aes(linetype=as.factor(knidx),
                alpha=as.factor(knidx)),size=1) + 
  geom_ribbon(aes(ymin=lowerCI,ymax=upperCI, fill=as.factor(knidx),
                  alpha=as.factor(knidx))) + 
  facet_grid(. ~ context + year, scales = "free_x",
             labeller = label_bquote(cols = .(year))) + 
  scale_linetype_manual(name="", values = c(1,2),
                        labels=c("Uninformed","Informed")) + 
  scale_fill_brewer(name="", palette=2, type="qual",
                    labels = c("Uninformed","Informed")) + 
  scale_alpha_manual(name="", values = c(0,0.5),
                     labels=c("Uninformed","Informed")) + 
  scale_x_continuous(breaks=c(-10,0,10)) + 
  xlab("PVI Score (5 percentile to 95 percentile)") + 
  ylab("Average Predicted Probability \nof Republican Vote") + 
  coord_cartesian(ylim=c(0.35,0.65)) + 
  theme_classic() + 
  theme(text = element_text(size=12),
        legend.text = element_text(size=12, face="bold"),
        strip.text = element_text(size=12, face="bold"),
        axis.text.y = element_text(size=12, face="bold"),
        axis.title = element_text(size=12, face="bold")) + 
  #theme(strip.text.x = element_text(face="bold", size=11)) + 
  themeadd
pz <- addstriplabel(p)
grid.draw(pz)

# Save
ggsave("./slides/pictures/ccespredplot_informed.png", pz, width=8, height=5)
