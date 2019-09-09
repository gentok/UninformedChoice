#################################################################################
## File Name: CCES_analysis1_slide.R                                           ##
## Date: 5 May 2019                                                            ##
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
library(grid)

## Custom Functions
source("codes_cces/CCES_analysis0_functions.R")

# Load Data
load("codes_cces/processing/CCES08_analysis1logit.rda")
gdp08 <- gdp3
mp08 <- mp3
dimp08 <- dimp5
load("codes_cces/processing/CCES16_analysis1logit.rda")
gdp16 <- gdp3
mp16 <- mp3
dimp16 <- dimp5

# Save County and State Params

# param_voting <- readRDS("data/param_voting.rds")
# param_voting <- param_voting[-c(6,7),]
# gdpadd <- rbind(gdp08$gd0[grep("pvi_",row.names(gdp08$gd0)),],
#                 gdp16$gd0[grep("pvi_",row.names(gdp16$gd0)),])
# gdpadd$Median <- NA
# gdpadd$SE <- NA
# param_voting <- rbind(param_voting,
#                       colMeans(gdpadd[,c("Mean","Median","SE","lowerCI","upperCI")][c(1,3),])/10,
#                       colMeans(gdpadd[,c("Mean","Median","SE","lowerCI","upperCI")][c(2,4),])/10)
# rownames(param_voting)[6:7] <- c("pvi_state","pvi_county")
# param_voting
# saveRDS(param_voting,"data/param_voting.rds")

# Coefficient Plots 

## Prepare Data
tab1 <- gdp08$gd0[,2:4]
tab1[3,] <- tab1[3,]*3
tab2 <- gdp08$gd1[,2:4]
tab2[3,] <- tab2[3,]*3
tab3 <- gdp16$gd0[,2:4]
tab3[3,] <- tab3[3,]*3
tab4 <- gdp16$gd1[,2:4]
tab4[3,] <- tab4[3,]*3

# Background Theme
bgcolor <- rgb(245, 245, 245,maxColorValue = 255)
themeadd <- 
  theme(panel.background = element_rect(fill=bgcolor, colour="black"),
        plot.background = element_rect(fill=bgcolor, colour=bgcolor),
        legend.background = element_rect(fill=bgcolor, colour=bgcolor),
        legend.position = "bottom")


# Drop Following Variables
dropvars <- c("female","age","agesq","black","latino",
              "asian","other","edu","income","bornagain")
dropvars2 <- c("pvi_state","pvi_county","idedistsq_prepadv",
               "evecon","pid")

# Footnote
gp_foot <- 
  paste0("Demographic variables and intecept are omitted from the figure. All variables are scaled so that higher score favors Republican party.\n",
         "2008 Model Statistics: ", gof_export(mp08), "\n",
         "2016 Model Statistics: ", gof_export(mp16))

# Variable Labels
vnt <- vn3[2:6]
vnt[3] <- "Ideological Advantage (By 3)"
vnt2 <- vn3[7:16]

# Draw Plot
ticks <- c(1,2,3,4)
gp <- plot_coef(list(tab1,tab2,tab3,tab4),
                   overlap.names = c("Uninformed","Informed","Uninformed","Informed"),
                   overlap.shape.index = c(19,17,19,17),
                   overlap.color.index = c("red","black","red","black"),
                   overlap.linetype.index = c(1,1,1,1),
                   point.size = 2.5,
                   ci.size = 0.7, ci.height = 0.2,
                   facet.names = c("2008","2008","2016","2016"),
                   title = "Dependent Variable: Presidential Vote Choice \n(1=Republican, 0=Democrat)",
                   drop.variable.names = dropvars, 
                   odds=T, custom.footnote = gp_foot, 
                   footnote.caption = TRUE,
                   custom.variable.names = vnt) + 
  scale_y_log10(breaks=ticks, labels = ticks) + 
  theme(text = element_text(size=12),
        legend.text = element_text(size=12, face="bold"),
        strip.text.x = element_text(size=14, face="bold"),
        axis.text.y = element_text(size=12, face="bold"),
        plot.title = element_text(size=14, face="bold")) + 
  themeadd
gp

ggsave("./slides/pictures/coefpt.png", width=8, height=5)

# Draw Plot
ticks <- c(1,2,3,4)
gp <- plot_coef(list(tab1,tab2,tab3,tab4),
                overlap.names = c("Uninformed","Informed","Uninformed","Informed"),
                overlap.shape.index = c(19,17,19,17),
                overlap.color.index = c("white","black","white","black"),
                overlap.linetype.index = c(1,1,1,1),
                point.size = 2.5,
                ci.size = 0.7, ci.height = 0.2,
                facet.names = c("2008","2008","2016","2016"),
                title = "Dependent Variable: Presidential Vote Choice \n(1=Republican, 0=Democrat)",
                drop.variable.names = dropvars, 
                odds=T, custom.footnote = gp_foot, 
                footnote.caption = TRUE,
                custom.variable.names = vnt) + 
  scale_y_log10(breaks=ticks, labels = ticks) + 
  theme(text = element_text(size=12),
        legend.text = element_text(size=12, face="bold"),
        strip.text.x = element_text(size=14, face="bold"),
        axis.text.y = element_text(size=12, face="bold"),
        plot.title = element_text(size=14, face="bold")) + 
  themeadd
gp

ggsave("./slides/pictures/coefpt_informed.png", width=8, height=5)

# Draw Plot
ticks <- c(1,2,3,4)
gp <- plot_coef(list(tab1,tab2,tab3,tab4),
                overlap.names = c("Uninformed","Informed","Uninformed","Informed"),
                overlap.shape.index = c(19,17,19,17),
                overlap.color.index = c("red","black","red","black"),
                overlap.linetype.index = c(1,1,1,1),
                point.size = 2.5,
                ci.size = 0.7, ci.height = 0.2,
                facet.names = c("2008","2008","2016","2016"),
                title = "Dependent Variable: Presidential Vote Choice \n(1=Republican, 0=Democrat)",
                drop.variable.names = dropvars2, 
                odds=T, custom.footnote = gp_foot, 
                footnote.caption = TRUE,
                custom.variable.names = vnt2) + 
  scale_y_log10(breaks=ticks, labels = ticks) + 
  theme(text = element_text(size=12),
        legend.text = element_text(size=12, face="bold"),
        strip.text.x = element_text(size=14, face="bold"),
        axis.text.y = element_text(size=12, face="bold"),
        plot.title = element_text(size=14, face="bold")) + 
  themeadd
gp

ggsave("./slides/pictures/coefpt_dem.png", width=8, height=5)

