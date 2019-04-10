#################################################################################
## File Name: CCES16_analysis1.R                                               ##
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

## Custom Functions
source("codes/CCES_analysis0_functions.R")

## Initial Data Location
# dloc <- "data/ucdcces16s_cxt.rds"; wtname <- "wt"
dloc <- "data/cces16s_cxt2.rds"; wtname <- "wt_post"


###############
## Load Data ##
###############

d <- readRDS(dloc)
#d <- d[-which(d$state %in% c("Alaska","District of Columbia")),]
#d <- d[d$pidstr<=1,]

#dc <- d[-which(d$state %in% c("Alaska")),]

##################
## Try Analysis ##
##################

## 1. Presidents ##################################################

# 1.0. No Context 

# Specify Model to Be Used
m0_p16 <- update(update(update(formula(vpres ~ 1),m_cxt0),m_ide_p),m_ctl)

# Estimate Models
e0_p16 <- func_logit(d,m0_p16,wtname)
e0r_p16.vcov <- cluster.vcov(e0_p16,e0_p16$dt$state)
e0r_p16 <- coeftest(e0_p16,e0r_p16.vcov)
round(e0r_p16,4)

# 1.1. Presidential Voting Behavior (State)

# Specify Model to Be Used
m1v_p16 <- update(update(update(formula(vpres ~ 1),m_cxt1v),m_ide_p),m_ctl)
# m1c_p16 <- update(update(update(formula(vpres ~ 1),m_cxt1c),m_ide_p),m_ctl)

# Estimate Models
e1v_p16 <- func_logit(d,m1v_p16,wtname)
e1vr_p16.vcov <- cluster.vcov(e1v_p16,e1v_p16$dt$state)
e1vr_p16 <- coeftest(e1v_p16,e1vr_p16.vcov)
round(e1vr_p16,4)
# e1c_p16 <- func_logit(d,m1c_p16,wtname)
# e1cr_p16.vcov <- cluster.vcov(e1c_p16,e1c_p16$dt$state)
# e1cr_p16 <- coeftest(e1c_p16,e1cr_p16.vcov)
# round(e1cr_p16,4)

# Plot Coefficients
gdl1v_p16 <- gd_export(e1v_p16,e1vr_p16,e1vr_p16.vcov)
g1v_p16 <- plot_coef(list(gdl1v_p16$gd0,gdl1v_p16$gd1),
                    m.names = c("Uninformed","Informed"),
                    title = "Impact on Presidential Vote Choice 2016 \n(1=Trump, 0=Clinton)",
                    odds=T, custom.footnote = extract_gofchr(e1v_p16),
                    custom.variable.names = vn1v)
# pdf_save(g1v_p16,8,5,"president_statePVI.pdf",plotdir)
# gdl1c_p16 <- gd_export(e1c_p16,e1cr_p16,e1cr_p16.vcov)
# g1c_p16 <- plot_coef(list(gdl1c_p16$gd0,gdl1c_p16$gd1),
#                      m.names = c("Uninformed","Informed"),
#                      title = "Impact on Presidential Vote Choice 2016 \n(1=Trump, 0=Clinton)",
#                      odds=T, custom.footnote = extract_gofchr(e1c_p16),
#                      custom.variable.names = vn1c)
# pdf_save(g1c_p16,8,5,"president_statePCI.pdf",plotdir)

# 1.2. Presidential Voting Behavior (County)

# Specify Model to Be Used
m2v_p16 <- update(update(update(formula(vpres ~ 1),m_cxt2v),m_ide_p),m_ctl)
# m2c_p16 <- update(update(update(formula(vpres ~ 1),m_cxt2c),m_ide_p),m_ctl)

# Estimate County Level Models
# Estimate Models
e2v_p16 <- func_logit(d,m2v_p16,wtname)
e2vr_p16.vcov <- cluster.vcov(e2v_p16,e2v_p16$dt$state)
e2vr_p16 <- coeftest(e2v_p16,e2vr_p16.vcov)
round(e2vr_p16,4)
# e2c_p16 <- func_logit(d,m2c_p16,wtname)
# e2cr_p16.vcov <- cluster.vcov(e2c_p16,e2c_p16$dt$state)
# e2cr_p16 <- coeftest(e2c_p16,e2cr_p16.vcov)
# round(e2cr_p16,4)

# Plot Coefficients
gdl2v_p16 <- gd_export(e2v_p16,e2vr_p16,e2vr_p16.vcov)
g2v_p16 <- plot_coef(list(gdl2v_p16$gd0,gdl2v_p16$gd1),
                     m.names = c("Uninformed","Informed"),
                     title = "Impact on Presidential Vote Choice 2016 \n(1=Trump, 0=Clinton)",
                     odds=T, custom.footnote = extract_gofchr(e2v_p16),
                     custom.variable.names = vn2v)
# pdf_save(g2v_p16,8,5,"president_statePVI.pdf",plotdir)
# gdl2c_p16 <- gd_export(e2c_p16,e2cr_p16,e2cr_p16.vcov)
# g2c_p16 <- plot_coef(list(gdl2c_p16$gd0,gdl2c_p16$gd1),
#                      m.names = c("Uninformed","Informed"),
#                      title = "Impact on Presidential Vote Choice 2016 \n(1=Trump, 0=Clinton)",
#                      odds=T, custom.footnote = extract_gofchr(e2c_p16),
#                      custom.variable.names = vn2c)
# pdf_save(g2c_p16,8,5,"president_statePCI.pdf",plotdir)

## 3. House ##################################################


# Specify Model to Be Used
m0_h16 <- update(update(update(formula(vhres ~ 1),m_cxt0),m_ide_p),m_ctl)

# Estimate Models
e0_h16 <- func_logit(d,m0_h16,wtname)
e0r_h16.vcov <- cluster.vcov(e0_h16,e0_h16$dt$state)
e0r_h16 <- coeftest(e0_h16,e0r_h16.vcov)
round(e0r_h16,4)

# 3.1. House Voting Behavior (State)

# Specify Model to Be Used
# m1v_h16 <- update(update(update(formula(vhres ~ 1),m_hcxt1v),m_ide_p),m_ctl)
# m1c_h16 <- update(update(update(formula(vhres ~ 1),m_hcxt1c),m_ide_p),m_ctl)
m1v2_h16 <- update(update(update(formula(vhres ~ 1),m_hcxt1v2),m_ide_p),m_ctl)
# m1c2_h16 <- update(update(update(formula(vhres ~ 1),m_hcxt1c2),m_ide_p),m_ctl)

# Estimate Models
# e1v_h16 <- func_logit(d,m1v_h16,wtname)
# e1vr_h16.vcov <- cluster.vcov(e1v_h16,e1v_h16$dt$state)
# e1vr_h16 <- coeftest(e1v_h16,e1vr_h16.vcov)
# round(e1vr_h16,4)
# e1c_h16 <- func_logit(d,m1c_h16,wtname)
# e1cr_h16.vcov <- cluster.vcov(e1c_h16,e1c_h16$dt$state)
# e1cr_h16 <- coeftest(e1c_h16,e1cr_h16.vcov)
# round(e1cr_h16,4)
e1v2_h16 <- func_logit(d,m1v2_h16,wtname)
e1v2r_h16.vcov <- cluster.vcov(e1v2_h16,e1v2_h16$dt$state)
e1v2r_h16 <- coeftest(e1v2_h16,e1v2r_h16.vcov)
round(e1v2r_h16,4)
# e1c2_h16 <- func_logit(d,m1c2_h16,wtname)
# e1c2r_h16.vcov <- cluster.vcov(e1c2_h16,e1c2_h16$dt$state)
# e1c2r_h16 <- coeftest(e1c2_h16,e1c2r_h16.vcov)
# round(e1c2r_h16,4)

# Plot Coefficients
# gdl1v_h16 <- gd_export(e1v_h16,e1vr_h16,e1vr_h16.vcov)
# g1v_h16 <- plot_coef(list(gdl1v_h16$gd0,gdl1v_h16$gd1),
#                      m.names = c("Uninformed","Informed"),
#                      title = "Impact on House Vote Choice 2016 \n(1=Republican, 0=Democrat)",
#                      odds=T, custom.footnote = extract_gofchr(e1v_h16),
#                      custom.variable.names = vn1v2)
# # pdf_save(g1v_h16,8,5,"house_statePVI.pdf",plotdir)
# gdl1c_h16 <- gd_export(e1c_h16,e1cr_h16,e1cr_h16.vcov)
# g1c_h16 <- plot_coef(list(gdl1c_h16$gd0,gdl1c_h16$gd1),
#                      m.names = c("Uninformed","Informed"),
#                      title = "Impact on House Vote Choice 2016 \n(1=Republican, 0=Democrat)",
#                      odds=T, custom.footnote = extract_gofchr(e1c_h16),
#                      custom.variable.names = vn1c2)
# # pdf_save(g1c_h16,8,5,"house_statePCI_2yrs.pdf",plotdir)
gdl1v2_h16 <- gd_export(e1v2_h16,e1v2r_h16,e1v2r_h16.vcov)
g1v2_h16 <- plot_coef(list(gdl1v2_h16$gd0,gdl1v2_h16$gd1),
                      m.names = c("Uninformed","Informed"),
                      title = "Impact on House Vote Choice 2016 \n(1=Republican, 0=Democrat)",
                      odds=T, custom.footnote = extract_gofchr(e1v2_h16),
                      custom.variable.names = vn1v2)
# pdf_save(g1v2_h16,8,5,"house_statePVI_2yrs.pdf",plotdir)
# gdl1c2_h16 <- gd_export(e1c2_h16,e1c2r_h16,e1c2r_h16.vcov)
# g1c2_h16 <- plot_coef(list(gdl1c2_h16$gd0,gdl1c2_h16$gd1),
#                       m.names = c("Uninformed","Informed"),
#                       title = "Impact on House Vote Choice 2016 \n(1=Republican, 0=Democrat)",
#                       odds=T, custom.footnote = extract_gofchr(e1c2_h16),
#                       custom.variable.names = vn1c2)
# pdf_save(g1c2_h16,8,5,"house_statePCI.pdf",plotdir)

# . House Voting Behavior (County)

# Specify Model to Be Used
m2v_h16 <- update(update(update(formula(vhres ~ 1),m_hcxt3v2),m_ide_p),m_ctl)
# m2c_h16 <- update(update(update(formula(vhres ~ 1),m_hcxt3c2),m_ide_p),m_ctl)

# Estimate County Level Models
# Estimate Models
e2v_h16 <- func_logit(d,m2v_h16,wtname)
e2vr_h16.vcov <- cluster.vcov(e2v_h16,e2v_h16$dt$county)
e2vr_h16 <- coeftest(e2v_h16,e2vr_h16.vcov)
round(e2vr_h16,4)
# e2c_h16 <- func_logit(d,m2c_h16,wtname)
# e2cr_h16.vcov <- cluster.vcov(e2c_h16,e2c_h16$dt$county)
# e2cr_h16 <- coeftest(e2c_h16,e2cr_h16.vcov)
# round(e2cr_h16,4)

# Plot Coefficients
gdl2v_h16 <- gd_export(e2v_h16,e2vr_h16,e2vr_h16.vcov)
g2v_h16 <- plot_coef(list(gdl2v_h16$gd0,gdl2v_h16$gd1),
                     m.names = c("Uninformed","Informed"),
                     title = "Impact on House Vote Choice 2016 \n(1=Republican, 0=Democrat)",
                     odds=T, custom.footnote = extract_gofchr(e2v_h16),
                     custom.variable.names = vn2v2)
# pdf_save(g2v_h16,8,5,"house_statePVI_2yrs.pdf",plotdir)
# gdl2c_h16 <- gd_export(e2c_h16,e2cr_h16,e2cr_h16.vcov)
# g2c_h16 <- plot_coef(list(gdl2c_h16$gd0,gdl2c_h16$gd1),
#                      m.names = c("Uninformed","Informed"),
#                      title = "Impact on House Vote Choice 2016 \n(1=Republican, 0=Democrat)",
#                      odds=T, custom.footnote = extract_gofchr(e2c_h16),
#                      custom.variable.names = vn2c2)
# pdf_save(g2c_h16,8,5,"house_statePCI_2yrs.pdf",plotdir)

#################
## Simulations ##
#################

# setctl <- function(prof) {
#   prof$idedist_padvm = 0
#   prof$idedist_prepadv = 0
#   prof$idedist_srepadv = 0
#   prof$repstr = 0
#   prof$demstr = 0
#   prof$pidstr = 0
#   prof$polint = 3
#   prof$evecon = 0
#   prof$female = 0
#   prof$age = 47
#   prof$white = 1
#   prof$black = 0
#   prof$latino = 0
#   prof$asian = 0
#   prof$other = 0
#   married = 1
#   prof$fulltime = 0
#   #prof$income = 6
#   prof$edu = 2
#   prof$bornagain = 0
#   prof$chatd = 1
#   return(prof)
# }
# 
# # Check Knowledge QUantile 0 to 1
# quantile(d$knidx, probs=c(0.05,0.95))
# 
# ## Presidential Election Simulation
# 
# # State
# (sq <- quantile(d$pvi_state, probs=c(0.05,0.95), na.rm=TRUE))
# newprof_state16 <- data.frame(
#   pvi_county = 0,
#   pvi_state = rep(seq(sq[1],sq[2],length=20),2),
#   knidx = rep(c(0,1),each=20))
# newprof_state16 <- setctl(newprof_state16)
# # Simulation
# p2v_p16_state <- simu_pred(e2v_p16, newprof_state16, vcov.est=e2vr_p16.vcov) 
# dp2v_p16_state <- cbind(p2v_p16_state$predsum, p2v_p16_state$profile)
# 
# # County
# (cq <- quantile(d$pvi_county, probs=c(0.05,0.95), na.rm=TRUE))
# newprof_county16 <- data.frame(
#   pvi_county = rep(seq(cq[1],cq[2],length=20),2),
#   pvi_state = 0,
#   knidx = rep(c(0,1),each=20))
# newprof_county16 <- setctl(newprof_county16)
# # Simulation
# p2v_p16_county <- simu_pred(e2v_p16, newprof_county16, vcov.est=e2vr_p16.vcov) 
# dp2v_p16_county <- cbind(p2v_p16_county$predsum, p2v_p16_county$profile)
# 
# # Combine
# dp2v_p16 <- rbind(dp2v_p16_state,dp2v_p16_county)
# dp2v_p16$context <- factor(rep(c("State PVI","County PVI"), each=40),
#                            levels=c("State PVI","County PVI"))
# dp2v_p16$pvi <- c(newprof_state16$pvi_state,newprof_county16$pvi_county)
# head(dp2v_p16)
# 
# # Plot
# gp2v_p16 <- ggplot(dp2v_p16, aes(x=pvi,y=Mean)) + 
#   geom_line(aes(linetype=as.factor(knidx)),size=0.5) + 
#   geom_ribbon(aes(ymin=lowerCI,ymax=upperCI, fill=as.factor(knidx)), alpha=0.5) + 
#   facet_grid(.~context, scales = "free_x") + 
#   scale_linetype_manual(name="", values = c(1,2),
#                         labels=c("Uninformed","Informed")) + 
#   scale_fill_brewer(name="", palette=2, type="qual",
#                     labels = c("Uninformed","Informed")) + 
#   xlab("Partisan Voter Index") + ylab("Probability of Republican Vote") + 
#   coord_cartesian(ylim=c(0,1)) + 
#   theme_classic()
# gp2v_p16
# 
# ## House Election Simulation
# 
# # State
# (sq <- quantile(d$hpvi2_state, probs=c(0.05,0.95), na.rm=TRUE))
# newprof_state16 <- data.frame(
#   hpvi2_district = 0,
#   hpvi2_state = rep(seq(sq[1],sq[2],length=20),2),
#   knidx = rep(c(0,1),each=20))
# newprof_state16 <- setctl(newprof_state16)
# # Simulation
# p2v_h16_state <- simu_pred(e2v_h16, newprof_state16, vcov.est=e2vr_h16.vcov) 
# dp2v_h16_state <- cbind(p2v_h16_state$predsum, p2v_h16_state$profile)
# 
# # District
# (cq <- quantile(d$hpvi2_district, probs=c(0.05,0.95), na.rm=TRUE))
# newprof_district16 <- data.frame(
#   hpvi2_district = rep(seq(cq[1],cq[2],length=20),2),
#   hpvi2_state = 0,
#   knidx = rep(c(0,1),each=20))
# newprof_district16 <- setctl(newprof_district16)
# # Simulation
# p2v_h16_district <- simu_pred(e2v_h16, newprof_district16, vcov.est=e2vr_h16.vcov) 
# dp2v_h16_district <- cbind(p2v_h16_district$predsum, p2v_h16_district$profile)
# 
# # Combine
# dp2v_h16 <- rbind(dp2v_h16_state,dp2v_h16_district)
# dp2v_h16$context <- factor(rep(c("State PVI","District PVI"), each=40),
#                            levels=c("State PVI","District PVI"))
# dp2v_h16$hpvi2 <- c(newprof_state16$hpvi2_state,newprof_district16$hpvi2_district)
# head(dp2v_h16)
# 
# # Plot
# gp2v_h16 <- ggplot(dp2v_h16, aes(x=hpvi2,y=Mean)) + 
#   geom_line(aes(linetype=as.factor(knidx)),size=0.5) + 
#   geom_ribbon(aes(ymin=lowerCI,ymax=upperCI, fill=as.factor(knidx)), alpha=0.5) + 
#   facet_grid(.~context, scales = "free_x") + 
#   scale_linetype_manual(name="", values = c(1,2),
#                         labels=c("Uninformed","Informed")) + 
#   scale_fill_brewer(name="", palette=2, type="qual",
#                     labels = c("Uninformed","Informed")) + 
#   xlab("House Partisan Voter Index") + 
#   ylab("Probability of Republican Vote") + 
#   coord_cartesian(ylim=c(0,1)) + 
#   theme_classic()
# gp2v_h16

################
## Save Plots ##
################

save.image("outputs/CCES16_analysis1.rda")
