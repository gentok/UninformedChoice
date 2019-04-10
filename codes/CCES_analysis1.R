#################################################################################
## File Name: CCES_analysis1.R                                               ##
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
library(grid)

## Custom Functions
source("codes/CCES_analysis0_functions.R")

# Load Data
load("outputs/CCES16_analysis1.rda")
rm(d)
load("outputs/CCES08_analysis1.rda")
rm(d)

# Plots 
dropvars <- c("(Intercept)","female","I((age - 46)/10)","black","latino",
              "asian","other","I(edu - 2)","bornagain")

g2v_p_foot <- 
  paste0("Demographic variables and intecept are omitted from the figure.\n",
         "2008 Model Statistics: ", extract_gofchr(e2v_p08), "\n",
         "2016 Model Statistics: ", extract_gofchr(e2v_p16))

g2v_p <- plot_coef(list(gdl2v_p08$gd0,gdl2v_p08$gd1,
                        gdl2v_p16$gd0,gdl2v_p16$gd1),
                   overlap.names = c("Uninformed","Informed","Uninformed","Informed"),
                   overlap.shape.index = c(19,17,19,17),
                   facet.names = c("2008","2008","2016","2016"),
                   title = "Impact on Presidential Vote Choice \n(1=Republican, 0=Democrat)",
                   drop.intercept = TRUE,
                   drop.intercept.names = dropvars,
                   #                     drop.variable.names = dropvars, 
                   odds=T, custom.footnote = g2v_p_foot, 
                   footnote.caption = TRUE,
                   custom.variable.names = vn2v[2:6])
ticks <- c(0.2,0.4,0.7,1,2,5,10)
g2v_p <- g2v_p + scale_y_log10(breaks=ticks, labels = ticks)
grid.draw(g2v_p)


g2v_h_foot <- 
  paste0("Demographic variables and intecept are omitted from the figure (see Appendix for full set of estimates).\n",
        "2008 Model Statistics: ", extract_gofchr(e2v_h08), "\n",
        "2016 Model Statistics: ", extract_gofchr(e2v_h16))

g2v_h <- plot_coef(list(gdl2v_h08$gd0,gdl2v_h08$gd1,
                          gdl2v_h16$gd0,gdl2v_h16$gd1),
                     overlap.names = c("Uninformed","Informed","Uninformed","Informed"),
                     overlap.shape.index = c(19,17,19,17),
                     facet.names = c("2008","2008","2016","2016"),
                     title = "Impact on House Vote Choice \n(1=Republican, 0=Democrat)",
                     drop.intercept = TRUE,
                     drop.intercept.names = dropvars,
#                     drop.variable.names = dropvars, 
                     odds=T, custom.footnote = g2v_h_foot,
                     footnote.caption = TRUE,
                     custom.variable.names = vn2v2[2:6])
ticks <- c(0.5,0.7,1,1.5,2)
g2v_h <- g2v_h + scale_y_log10(breaks=ticks, labels = ticks)
grid.draw(g2v_h)

# Tables 
nvn2vx <- vn2v[-1]
nvn2vy <- paste("Knowledge *", nvn2vx)
nvn2v <- c(vn2v[1], "Political Knowledge (0-1)", vn2v[-1], nvn2vy)

te2v_p <- table_coef(list(e2v_p08,e2v_p16),
           vcov.est=list(e2vr_p08.vcov,e2vr_p16.vcov),
           m.names = c("2008","2016"),
           single.row = TRUE,
           custom.variable.names = nvn2v,
           format = "tex",booktabs=TRUE,dcolumn=TRUE,
           caption = "Impact on Presidential Vote Choice")
te2v_p

nvn2v2x <- vn2v2[-1]
nvn2v2y <- paste("Knowledge *", nvn2v2x)
nvn2v2 <- c(vn2v[1], "Political KNowledge (0-1)", vn2v2[-1], nvn2v2y)

te2v_h <- table_coef(list(e2v_h08,e2v_h16),
                     vcov.est=list(e2vr_h08.vcov,e2vr_h16.vcov),
                     m.names = c("2008","2016"),
                     single.row = TRUE,
                     custom.variable.names = nvn2v2,
                     format = "tex",booktabs=TRUE,dcolumn=TRUE,
                     caption = "Impact on House Vote Choice")
te2v_h

## Save

save(g2v_p, g2v_h, te2v_p, te2v_h,
     file="outputs/CCES_analysis1.rda")
