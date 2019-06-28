#################################################################################
## File Name: CCES_analysis_slide.R                                            ##
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
plotdir <- "papers/figures"

## For Analysis
library(ggplot2)
library(lmtest)
library(multiwayvcov)
library(estvis)

## Custom Functions
source("codes/CCES_analysis0_functions.R")

## Load Data
load("outputs/CCES08_analysis1.rda")
load("outputs/CCES16_analysis1.rda")
load("outputs/CCES08_analysis2.rda")
load("outputs/CCES16_analysis2.rda")

## Presidential Elections ##################################################

###################
## Visualization ##
###################

bgcolor <- rgb(245, 245, 245,maxColorValue = 255)

themeadd <- 
  theme(panel.background = element_rect(fill=bgcolor, colour="black"),
        plot.background = element_rect(fill=bgcolor, colour=bgcolor),
        legend.background = element_rect(fill=bgcolor, colour=bgcolor),
        legend.position = "bottom")

vn2v[2:6] <- 
  c("State PVI (10%)",
    "County PVI (10%)", 
    "Ideological Proximity (-6:6)",
    "Party ID (-3:3)", 
    "Retrospective Economic\n Evaluation (-2:2)")

s <- 2; e <- 6
fd <- rbind(gdl2v_p08$gd0[s:e,],gdl2v_p08$gd1[s:e,],
            gdl2v_p16$gd0[s:e,],gdl2v_p16$gd1[s:e,])
fd$year <- rep(c(2008,2016), each=nrow(gdl2v_p08$gd0[s:e,])*2)
fd$info <- rep(rep(c("Uninformed","Informed"), each=nrow(gdl2v_p08$gd0[s:e,])),2)
fd$info <- factor(fd$info, levels=c("Uninformed","Informed"))
fd$vn <- factor(rep(vn2v[s:e],4), levels=rev(vn2v[s:e]))

#ticks <- c(seq(.1, 1, by =.1), seq(0, 10, by =1), seq(10, 100, by =10))
ticks <- c(0.2,0.4,0.7,1,2,5,10)

coeflist <-
  ggplot(data=fd, aes(x=vn,y=exp(cf))) + 
  geom_hline(aes(yintercept=1), colour="black", linetype=2) +
  geom_errorbar(aes(ymin=exp(lCI), ymax=exp(uCI), colour=info), 
                position=position_dodge(width = -0.5),
                size=0.8, width=0) + 
  geom_point(aes(shape=info, colour=info), size=2, 
             position=position_dodge(width = -0.5)) + 
  scale_colour_manual(values = c(2,1)) + 
  xlab(NULL) + ylab("Odds Ratio") + 
  scale_y_log10(breaks=ticks, labels = ticks) + 
  facet_grid(.~year) + estvis.theme() + themeadd + 
  theme(text = element_text(size=12),
        legend.text = element_text(size=12, face="bold"),
        strip.text = element_text(size=12, face="bold"),
        axis.text.y = element_text(size=12, face="bold")) + 
  coord_flip() 

ft <- paste("Higher values indicate Republican advantage except for retrospective evaluation.\n",
            "N =", sub("N:","",extract_gofchr(e2v_p08, gof.extracts = "none")),
            "in 2008 and N =", sub("N:","",extract_gofchr(e2v_p16, gof.extracts = "none")),
            "in 2016.")
ft

coefpt <- plot_footnote(coeflist,ft,fontsize=10,caption=TRUE)

ggsave("./slides/pictures/coefpt.png", width=8, height=5)

coeflist_informed <-
  ggplot(data=fd, aes(x=vn,y=exp(cf))) + 
  geom_hline(aes(yintercept=1), colour="black", linetype=2) +
  geom_errorbar(aes(ymin=exp(lCI), ymax=exp(uCI), 
                    colour=info, alpha=info), 
                position=position_dodge(width = -0.5),
                size=0.8, width=0) + 
  geom_point(aes(shape=info, colour=info, alpha=info), size=2, 
             position=position_dodge(width = -0.5)) + 
  scale_colour_manual(values = c(2,1)) + 
  scale_alpha_manual(values = c(0,1)) + 
  xlab(NULL) + ylab("Odds Ratio") + 
  scale_y_log10(breaks=ticks, labels = ticks) + 
  facet_grid(.~year) + estvis.theme() + themeadd + 
  theme(text = element_text(size=12),
        legend.text = element_text(size=12, face="bold"),
        strip.text = element_text(size=12, face="bold"),
        axis.text.y = element_text(size=12, face="bold")) + 
  coord_flip() 
coeflist_informed

coefpt_informed <- plot_footnote(coeflist_informed,ft,fontsize=10,caption=TRUE)

ggsave("./slides/pictures/coefpt_informed.png", width=8, height=5)

coeflist_null <-
  ggplot(data=fd, aes(x=vn,y=exp(cf))) + 
  geom_hline(aes(yintercept=1), colour="black", linetype=2) +
  geom_errorbar(aes(ymin=exp(lCI), ymax=exp(uCI), colour=info), 
                position=position_dodge(width = -0.5),
                size=0.8, width=0, alpha=0) + 
  geom_point(aes(shape=info, colour=info), size=2, alpha=0, 
             position=position_dodge(width = -0.5)) + 
  scale_colour_manual(values = c(2,1)) + 
  xlab(NULL) + ylab("Odds Ratio") + 
  scale_y_log10(breaks=ticks, labels = ticks) + 
  facet_grid(.~year) + estvis.theme() + themeadd + 
  theme(text = element_text(size=12),
        legend.text = element_text(size=12, face="bold"),
        strip.text = element_text(size=12, face="bold"),
        axis.text.y = element_text(size=12, face="bold")) + 
  coord_flip() 
coeflist_null

coefpt_null <- plot_footnote(coeflist_null,ft,fontsize=10,caption=TRUE)

ggsave("./slides/pictures/coefpt_null.png", width=8, height=5)

###########
## state ##
###########

dagp2v_pstate <- rbind(
  dagp2v_p08x[dagp2v_p08x$context=="State PVI",],
  dagp2v_p16x[dagp2v_p16x$context=="State PVI",]
)

gagp2v_pstate <- ggplot(dagp2v_pstate, aes(x=pvi,y=Mean)) + 
  geom_hline(aes(yintercept=0.5), colour="gray50") + 
  geom_ribbon(aes(ymin=lowerCI,ymax=upperCI, fill=as.factor(knidx)), alpha=0.5) + 
  geom_line(aes(linetype=as.factor(knidx)),size=1, color="gray20") + 
  facet_grid(.~year, scales = "free_x") + 
  scale_linetype_manual(name="", values = c(1,2),
                        labels=c("Uninformed","Informed")) + 
  scale_fill_brewer(name="", palette=2, type="qual",
                    labels = c("Uninformed","Informed")) + 
  xlab("Partisan Voter Index") + ylab("Predicted Share of Republican Vote") + 
  coord_cartesian(ylim=c(0.3,0.7)) + 
  estvis.theme() + 
  themeadd + 
  theme(text = element_text(size=12),
        legend.text = element_text(size=12, face="bold"),
        strip.text = element_text(size=12, face="bold"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title = element_text(size=13, face="bold"))
gagp2v_pstate

ggsave("./slides/pictures/gagp2v_pstate.png", width=8, height=5)

gagp2v_pstate_informed <- ggplot(dagp2v_pstate, aes(x=pvi,y=Mean)) + 
  geom_hline(aes(yintercept=0.5), colour="gray50") + 
  geom_ribbon(aes(ymin=lowerCI, ymax=upperCI, 
                  fill=as.factor(knidx), alpha=as.factor(knidx))) + 
  geom_line(aes(linetype=as.factor(knidx), alpha=as.factor(knidx)),size=1) + 
  facet_grid(.~year, scales = "free_x") + 
  scale_linetype_manual(name="", values = c(1,2),
                        labels=c("Uninformed","Informed")) + 
  scale_fill_brewer(name="", palette=2, type="qual",
                    labels = c("Uninformed","Informed")) + 
  scale_alpha_manual(name="", values = c(0,0.5),
                     labels=c("Uninformed","Informed")) + 
  xlab("Partisan Voter Index") + ylab("Predicted Share of Republican Vote") + 
  coord_cartesian(ylim=c(0.3,0.7)) + 
  estvis.theme() + 
  themeadd + 
  theme(text = element_text(size=12),
        legend.text = element_text(size=12, face="bold"),
        strip.text = element_text(size=12, face="bold"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title = element_text(size=13, face="bold"))
gagp2v_pstate_informed

ggsave("./slides/pictures/gagp2v_pstate_informed.png", width=8, height=5)

############
## county ##
############

dagp2v_pcounty <- rbind(
  dagp2v_p08x[dagp2v_p08x$context=="County PVI",],
  dagp2v_p16x[dagp2v_p16x$context=="County PVI",]
)

gagp2v_pcounty <- ggplot(dagp2v_pcounty, aes(x=pvi,y=Mean)) + 
  geom_hline(aes(yintercept=0.5), colour="gray50") + 
  geom_ribbon(aes(ymin=lowerCI,ymax=upperCI, fill=as.factor(knidx)), alpha=0.5) + 
  geom_line(aes(linetype=as.factor(knidx)),size=1, color="gray20") + 
  facet_grid(.~year, scales = "free_x") + 
  scale_linetype_manual(name="", values = c(1,2),
                        labels=c("Uninformed","Informed")) + 
  scale_fill_brewer(name="", palette=2, type="qual",
                    labels = c("Uninformed","Informed")) + 
  xlab("Partisan Voter Index") + ylab("Predicted Share of Republican Vote") + 
  coord_cartesian(ylim=c(0.3,0.7)) + 
  estvis.theme() + 
  themeadd + 
  theme(text = element_text(size=12),
        legend.text = element_text(size=12, face="bold"),
        strip.text = element_text(size=12, face="bold"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title = element_text(size=13, face="bold"))
gagp2v_pcounty

ggsave("./slides/pictures/gagp2v_pcounty.png", width=8, height=5)

gagp2v_pcounty_informed <- ggplot(dagp2v_pcounty, aes(x=pvi,y=Mean)) + 
  geom_hline(aes(yintercept=0.5), colour="gray50") + 
  geom_ribbon(aes(ymin=lowerCI, ymax=upperCI, 
                  fill=as.factor(knidx), alpha=as.factor(knidx))) + 
  geom_line(aes(linetype=as.factor(knidx), alpha=as.factor(knidx)),size=1) + 
  facet_grid(.~year, scales = "free_x") + 
  scale_linetype_manual(name="", values = c(1,2),
                        labels=c("Uninformed","Informed")) + 
  scale_fill_brewer(name="", palette=2, type="qual",
                    labels = c("Uninformed","Informed")) + 
  scale_alpha_manual(name="", values = c(0,0.5),
                     labels=c("Uninformed","Informed")) + 
  xlab("Partisan Voter Index") + ylab("Predicted Share of Republican Vote") + 
  coord_cartesian(ylim=c(0.3,0.7)) + 
  estvis.theme() + 
  themeadd + 
  theme(text = element_text(size=12),
        legend.text = element_text(size=12, face="bold"),
        strip.text = element_text(size=12, face="bold"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title = element_text(size=13, face="bold"))
gagp2v_pcounty_informed

ggsave("./slides/pictures/gagp2v_pcounty_informed.png", width=8, height=5)

## House Elections ##################################################

###################
## Visualization ##
###################

vn2v2[2:6] <- 
  c("House State PVI (10%)",
    "House County PVI (10%)", 
    "Ideological Proximity (-6:6)",
    "Party ID (-3:3)", 
    "Retrospective Economic\n Evaluation (-2:2)")

s <- 2; e <- 6
fd <- rbind(gdl2v_h08$gd0[s:e,],gdl2v_h08$gd1[s:e,],
            gdl2v_h16$gd0[s:e,],gdl2v_h16$gd1[s:e,])
fd$year <- rep(c(2008,2016), each=nrow(gdl2v_h08$gd0[s:e,])*2)
fd$info <- rep(rep(c("Uninformed","Informed"), each=nrow(gdl2v_h08$gd0[s:e,])),2)
fd$info <- factor(fd$info, levels=c("Uninformed","Informed"))
fd$vn <- factor(rep(vn2v2[s:e],4), levels=rev(vn2v2[s:e]))

#ticks <- c(seq(.1, 1, by =.1), seq(0, 10, by =1), seq(10, 100, by =10))
ticks <- c(0.2,0.4,0.7,1,2,5,10)

hcoeflist <-
  ggplot(data=fd, aes(x=vn,y=exp(cf))) + 
  geom_hline(aes(yintercept=1), colour="black", linetype=2) +
  geom_errorbar(aes(ymin=exp(lCI), ymax=exp(uCI), colour=info), 
                position=position_dodge(width = -0.5),
                size=0.8, width=0) + 
  geom_point(aes(shape=info, colour=info), size=2, 
             position=position_dodge(width = -0.5)) + 
  scale_colour_manual(values = c(2,1)) + 
  xlab(NULL) + ylab("Odds Ratio") + 
  scale_y_log10(breaks=ticks, labels = ticks) + 
  facet_grid(.~year) + estvis.theme() + themeadd + 
  theme(text = element_text(size=12),
        legend.text = element_text(size=12, face="bold"),
        strip.text = element_text(size=12, face="bold"),
        axis.text.y = element_text(size=12, face="bold")) + 
  coord_flip() 

ft <- paste("Higher values indicate Republican advantage except for retrospective evaluation.\n",
            "N =", sub("N:","",extract_gofchr(e2v_h08, gof.extracts = "none")),
            "in 2008 and N =", sub("N:","",extract_gofchr(e2v_h16, gof.extracts = "none")),
            "in 2016.")
ft

hcoefpt <- plot_footnote(hcoeflist,ft,fontsize=10,caption=TRUE)

ggsave("./slides/pictures/hcoefpt.png", width=8, height=5)

hcoeflist_informed <-
  ggplot(data=fd, aes(x=vn,y=exp(cf))) + 
  geom_hline(aes(yintercept=1), colour="black", linetype=2) +
  geom_errorbar(aes(ymin=exp(lCI), ymax=exp(uCI), 
                    colour=info, alpha=info), 
                position=position_dodge(width = -0.5),
                size=0.8, width=0) + 
  geom_point(aes(shape=info, colour=info, alpha=info), size=2, 
             position=position_dodge(width = -0.5)) + 
  scale_colour_manual(values = c(2,1)) + 
  scale_alpha_manual(values = c(0,1)) + 
  xlab(NULL) + ylab("Odds Ratio") + 
  scale_y_log10(breaks=ticks, labels = ticks) + 
  facet_grid(.~year) + estvis.theme() + themeadd + 
  theme(text = element_text(size=12),
        legend.text = element_text(size=12, face="bold"),
        strip.text = element_text(size=12, face="bold"),
        axis.text.y = element_text(size=12, face="bold")) + 
  coord_flip() 
hcoeflist_informed

hcoefpt_informed <- plot_footnote(hcoeflist_informed,ft,fontsize=10,caption=TRUE)

ggsave("./slides/pictures/hcoefpt_informed.png", width=8, height=5)

hcoeflist_null <-
  ggplot(data=fd, aes(x=vn,y=exp(cf))) + 
  geom_hline(aes(yintercept=1), colour="black", linetype=2) +
  geom_errorbar(aes(ymin=exp(lCI), ymax=exp(uCI), colour=info), 
                position=position_dodge(width = -0.5),
                size=0.8, width=0, alpha=0) + 
  geom_point(aes(shape=info, colour=info), size=2, alpha=0, 
             position=position_dodge(width = -0.5)) + 
  scale_colour_manual(values = c(2,1)) + 
  xlab(NULL) + ylab("Odds Ratio") + 
  scale_y_log10(breaks=ticks, labels = ticks) + 
  facet_grid(.~year) + estvis.theme() + themeadd + 
  theme(text = element_text(size=12),
        legend.text = element_text(size=12, face="bold"),
        strip.text = element_text(size=12, face="bold"),
        axis.text.y = element_text(size=12, face="bold")) + 
  coord_flip() 
hcoeflist_null

hcoefpt_null <- plot_footnote(hcoeflist_null,ft,fontsize=10,caption=TRUE)

ggsave("./slides/pictures/hcoefpt_null.png", width=8, height=5)

###########
## state ##
###########

dagp2v_hstate <- rbind(
  dagp2v_h08x[dagp2v_h08x$context=="House State PVI",],
  dagp2v_h16x[dagp2v_h16x$context=="House State PVI",]
)

gagp2v_hstate <- ggplot(dagp2v_hstate, aes(x=hpvi2,y=Mean)) + 
  geom_hline(aes(yintercept=0.5), colour="gray50") + 
  geom_ribbon(aes(ymin=lowerCI,ymax=upperCI, fill=as.factor(knidx)), alpha=0.5) + 
  geom_line(aes(linetype=as.factor(knidx)),size=1, color="gray20") + 
  facet_grid(.~year, scales = "free_x") + 
  scale_linetype_manual(name="", values = c(1,2),
                        labels=c("Uninformed","Informed")) + 
  scale_fill_brewer(name="", palette=2, type="qual",
                    labels = c("Uninformed","Informed")) + 
  xlab("Partisan Voter Index") + ylab("Predicted Share of Republican Vote") + 
  coord_cartesian(ylim=c(0.2,0.7)) + 
  estvis.theme() + 
  themeadd + 
  theme(text = element_text(size=12),
        legend.text = element_text(size=12, face="bold"),
        strip.text = element_text(size=12, face="bold"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title = element_text(size=13, face="bold"))
gagp2v_hstate

ggsave("./slides/pictures/gagp2v_hstate.png", width=8, height=5)

gagp2v_hstate_informed <- ggplot(dagp2v_hstate, aes(x=hpvi2,y=Mean)) + 
  geom_hline(aes(yintercept=0.5), colour="gray50") + 
  geom_ribbon(aes(ymin=lowerCI, ymax=upperCI, 
                  fill=as.factor(knidx), alpha=as.factor(knidx))) + 
  geom_line(aes(linetype=as.factor(knidx), alpha=as.factor(knidx)),size=1) + 
  facet_grid(.~year, scales = "free_x") + 
  scale_linetype_manual(name="", values = c(1,2),
                        labels=c("Uninformed","Informed")) + 
  scale_fill_brewer(name="", palette=2, type="qual",
                    labels = c("Uninformed","Informed")) + 
  scale_alpha_manual(name="", values = c(0,0.5),
                     labels=c("Uninformed","Informed")) + 
  xlab("Partisan Voter Index") + ylab("Predicted Share of Republican Vote") + 
  coord_cartesian(ylim=c(0.2,0.7)) + 
  estvis.theme() + 
  themeadd + 
  theme(text = element_text(size=12),
        legend.text = element_text(size=12, face="bold"),
        strip.text = element_text(size=12, face="bold"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title = element_text(size=13, face="bold"))
gagp2v_hstate_informed

ggsave("./slides/pictures/gagp2v_hstate_informed.png", width=8, height=5)

##############
## district ##
##############

dagp2v_hdistrict <- rbind(
  dagp2v_h08x[dagp2v_h08x$context=="House District PVI",],
  dagp2v_h16x[dagp2v_h16x$context=="House District PVI",]
)

gagp2v_hdistrict <- ggplot(dagp2v_hdistrict, aes(x=hpvi2,y=Mean)) + 
  geom_hline(aes(yintercept=0.5), colour="gray50") + 
  geom_ribbon(aes(ymin=lowerCI,ymax=upperCI, fill=as.factor(knidx)), alpha=0.5) + 
  geom_line(aes(linetype=as.factor(knidx)),size=1, color="gray20") + 
  facet_grid(.~year, scales = "free_x") + 
  scale_linetype_manual(name="", values = c(1,2),
                        labels=c("Uninformed","Informed")) + 
  scale_fill_brewer(name="", palette=2, type="qual",
                    labels = c("Uninformed","Informed")) + 
  xlab("Partisan Voter Index") + ylab("Predicted Share of Republican Vote") + 
  coord_cartesian(ylim=c(0.2,0.7)) + 
  estvis.theme() + 
  themeadd + 
  theme(text = element_text(size=12),
        legend.text = element_text(size=12, face="bold"),
        strip.text = element_text(size=12, face="bold"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title = element_text(size=13, face="bold"))
gagp2v_hdistrict

ggsave("./slides/pictures/gagp2v_hdistrict.png", width=8, height=5)

gagp2v_hdistrict_informed <- ggplot(dagp2v_hdistrict, aes(x=hpvi2,y=Mean)) + 
  geom_hline(aes(yintercept=0.5), colour="gray50") + 
  geom_ribbon(aes(ymin=lowerCI, ymax=upperCI, 
                  fill=as.factor(knidx), alpha=as.factor(knidx))) + 
  geom_line(aes(linetype=as.factor(knidx), alpha=as.factor(knidx)),size=1) + 
  facet_grid(.~year, scales = "free_x") + 
  scale_linetype_manual(name="", values = c(1,2),
                        labels=c("Uninformed","Informed")) + 
  scale_fill_brewer(name="", palette=2, type="qual",
                    labels = c("Uninformed","Informed")) + 
  scale_alpha_manual(name="", values = c(0,0.5),
                     labels=c("Uninformed","Informed")) + 
  xlab("Partisan Voter Index") + ylab("Predicted Share of Republican Vote") + 
  coord_cartesian(ylim=c(0.2,0.7)) + 
  estvis.theme() + 
  themeadd + 
  theme(text = element_text(size=12),
        legend.text = element_text(size=12, face="bold"),
        strip.text = element_text(size=12, face="bold"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title = element_text(size=13, face="bold"))
gagp2v_hdistrict_informed

ggsave("./slides/pictures/gagp2v_hdistrict_informed.png", width=8, height=5)



## Visualization of Demographic Variables ##

s <- 7; e <- length(vn2v)
fd <- rbind(gdl2v_p08$gd0[s:e,],gdl2v_p08$gd1[s:e,],
            gdl2v_p16$gd0[s:e,],gdl2v_p16$gd1[s:e,])
fd$year <- rep(c(2008,2016), each=nrow(gdl2v_p08$gd0[s:e,])*2)
fd$info <- rep(rep(c("Uninformed","Informed"), each=nrow(gdl2v_p08$gd0[s:e,])),2)
fd$info <- factor(fd$info, levels=c("Uninformed","Informed"))
fd$vn <- factor(rep(vn2v[s:e],4), levels=rev(vn2v[s:e]))

#ticks <- c(seq(.1, 1, by =.1), seq(0, 10, by =1), seq(10, 100, by =10))
ticks <- c(0.2,0.4,0.7,1,2,5,10)

coeflist <-
  ggplot(data=fd, aes(x=vn,y=exp(cf))) + 
  geom_hline(aes(yintercept=1), colour="black", linetype=2) +
  geom_errorbar(aes(ymin=exp(lCI), ymax=exp(uCI), colour=info), 
                position=position_dodge(width = -0.5),
                size=0.8, width=0) + 
  geom_point(aes(shape=info, colour=info), size=2, 
             position=position_dodge(width = -0.5)) + 
  scale_colour_manual(values = c(2,1)) + 
  xlab(NULL) + ylab("Odds Ratio") + 
  scale_y_log10(breaks=ticks, labels = ticks) + 
  facet_grid(.~year) + estvis.theme() + themeadd + 
  theme(text = element_text(size=12),
        legend.text = element_text(size=12, face="bold"),
        strip.text = element_text(size=12, face="bold"),
        axis.text.y = element_text(size=12, face="bold")) + 
  coord_flip() 

ft <- paste("Higher values indicate Republican advantage except for retrospective evaluation.\n",
            "N =", sub("N:","",extract_gofchr(e2v_p08, gof.extracts = "none")),
            "in 2008 and N =", sub("N:","",extract_gofchr(e2v_p16, gof.extracts = "none")),
            "in 2016.")
ft

coefpt <- plot_footnote(coeflist,ft,fontsize=10,caption=TRUE)

ggsave("./slides/pictures/coefpt_dem.png", width=8, height=5)


###############
## Save Data ##
###############

#save(hcoeflist, hcoefpt, file="./outputs/CCES_analysis_slide.RData")
