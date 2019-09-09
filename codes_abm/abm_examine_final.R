#' ---
#' title: "Visualizing the ABM Results"
#' author: "Gento Kato"
#' date: "June 25, 2019"
#' ---
#' 

#' ## Preparation 
#' 
#' Required Packages
#' 
require(reshape2)
#' 
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
#' Data
#'
r <- readRDS("codes_abm/processing/abm_res_final.rds")
a <- melt(r)
a <- a[,c(1:4)]

# No Failed Simulations
l <- sapply(r, function(k) length(k$value))
droprows <- which(l==0)
droprows

# Create Data.Frame for Analysis
if (length(droprows)==0) {
  d <- as.data.frame(t(sapply(r, function(k) k$value)))
  d <- cbind(d, a)
} else {
  d <- as.data.frame(t(sapply(r[-droprows], function(k) k$value)))
  d <- cbind(d, a[-droprows,])
}
names(d) <- gsub(" ","_", names(d))

# Check Data
dim(d)
names(d)
class(d)

#'
#' ## Summarizing Quality 
#'

# Deviation (Negative Value Means Better)
with(d, summary(inf_deviation1 - inf_deviation2))
with(d, summary(inf_deviation1 - inf_deviation3))
with(d, summary(inf_deviation1 - inf_deviation4))
with(d, summary(inf_deviation1 - inf_deviation5))

# Same Winner (Positive Value Means Better)
with(d, summary(inf_samewinner1 - inf_samewinner2))
with(d, summary(inf_samewinner1 - inf_samewinner3))
with(d, summary(inf_samewinner1 - inf_samewinner4))
with(d, summary(inf_samewinner1 - inf_samewinner5))

# Aggregated Utility Loss (Positive Value is Better)
with(d, summary(inf_aggloss1 - inf_aggloss2))
with(d, summary(inf_aggloss1 - inf_aggloss3))
with(d, summary(inf_aggloss1 - inf_aggloss4))
with(d, summary(inf_aggloss1 - inf_aggloss5))

# Aggregated Utility Loss for Blue (Positive Value is Better)
with(d, summary(inf_aggloss_blue1 - inf_aggloss_blue2))
with(d, summary(inf_aggloss_blue1 - inf_aggloss_blue3))
with(d, summary(inf_aggloss_blue1 - inf_aggloss_blue4))
with(d, summary(inf_aggloss_blue1 - inf_aggloss_blue5))

# Aggregated Utility Loss for Red (Positive Value is Better)
with(d, summary(inf_aggloss_red1 - inf_aggloss_red2))
with(d, summary(inf_aggloss_red1 - inf_aggloss_red3))
with(d, summary(inf_aggloss_red1 - inf_aggloss_red4))
with(d, summary(inf_aggloss_red1 - inf_aggloss_red5))

#'
#' ## Fitting data by Linear Regression
#'

# Deviation model
m1c <- lm(inf_deviation1 ~ 
            alike_threshold*signalbias_sd*signalbias_mean*knowmean1,
          data=d)
summary(m1c)
m1cl <- lm(inf_deviation2 ~ 
            alike_threshold*signalbias_sd*signalbias_mean*knowmean1,
          data=d)
summary(m1cl)
m1cn <- lm(inf_deviation3 ~ 
            alike_threshold*signalbias_sd*signalbias_mean*knowmean1,
          data=d)
summary(m1cn)
m1s <- lm(inf_deviation4 ~ 
            alike_threshold*signalbias_sd*signalbias_mean*knowmean1,
          data=d)
summary(m1s)

m2c <- lm(inf_samewinner1 ~ 
            alike_threshold*signalbias_sd*signalbias_mean*knowmean1,
          data=d)
summary(m2c)
m2cl <- lm(inf_samewinner2 ~ 
            alike_threshold*signalbias_sd*signalbias_mean*knowmean1,
          data=d)
summary(m2cl)
m2cn <- lm(inf_samewinner3 ~ 
            alike_threshold*signalbias_sd*signalbias_mean*knowmean1,
          data=d)
summary(m2cn)
m2s <- lm(inf_samewinner4 ~ 
            alike_threshold*signalbias_sd*signalbias_mean*knowmean1,
          data=d)
summary(m2s)

m3c <- lm(inf_aggloss1 ~ 
            alike_threshold*signalbias_sd*signalbias_mean*knowmean1,
          data=d)
summary(m3c)
m3cl <- lm(inf_aggloss2 ~ 
            alike_threshold*signalbias_sd*signalbias_mean*knowmean1,
          data=d)
summary(m3cl)
m3cn <- lm(inf_aggloss3 ~ 
             alike_threshold*signalbias_sd*signalbias_mean*knowmean1,
           data=d)
summary(m3cn)
m3s <- lm(inf_aggloss4 ~ 
            alike_threshold*signalbias_sd*signalbias_mean*knowmean1,
          data=d)
summary(m3s)

m4c <- lm(inf_aggloss_blue1 - inf_aggloss_red1 ~ 
            alike_threshold*signalbias_sd*signalbias_mean*knowmean1,
          data=d)
summary(m4c)
m4cl <- lm(inf_aggloss_blue2 - inf_aggloss_red2 ~ 
            alike_threshold*signalbias_sd*signalbias_mean*knowmean1,
          data=d)
summary(m4cl)
m4cn <- lm(inf_aggloss_blue3 - inf_aggloss_red3 ~ 
            alike_threshold*signalbias_sd*signalbias_mean*knowmean1,
          data=d)
summary(m4cn)
m4s <- lm(inf_aggloss_blue4 - inf_aggloss_red4 ~ 
            alike_threshold*knowmean1 + 
            signalbias_sd*signalbias_mean*knowmean1,
          data=d)
summary(m4s)

#'
#' ## Prepartion Functions for Visualization
#'

#install.packages("devtools")
#devtools::install_github("gentok/estvis")
library(estvis)

newprof <- data.frame(alike_threshold = rep(rep(c(0.2,0.7),each=3),4),
                      signalbias_sd = rep(c(0,1,0,1),each=6),
                      signalbias_mean = rep(c(0,0,1,1),each=6),
                      signalbias = rep(c("Mean=0\nSD=0","Mean=0\nSD=1",
                                         "Mean=1\nSD=0","Mean=1\nSD=1"),each=6),
                      knowmean1 = rep(rep(c(0.45,0.55,0.65),2),4),
                      alikecat = rep(rep(c("Low\n(Tolerance=0.8)","High\n(Tolerance=0.3)"),each=3),4) )
newprof$alikecat <- factor(newprof$alikecat,
                           levels=unique(newprof$alikecat))

gens <- function(mc, ms, newprof) {
  ac <- predict(mc, newdata=newprof, se.fit=TRUE)
  as <- predict(ms, newdata=newprof, se.fit=TRUE)
  s <- data.frame(Mean = c(ac$fit,as$fit),
                  lowerCI = c(ac$fit - qnorm(0.975)*ac$se.fit,
                              as$fit - qnorm(0.975)*as$se.fit),
                  upperCI = c(ac$fit + qnorm(0.975)*ac$se.fit,
                              as$fit + qnorm(0.975)*as$se.fit),
                  pref = rep(c("Context-Based",
                               "Signal-Based"), each=nrow(newprof)))
  s <- cbind(s, rbind(newprof,newprof))
  return(s)
}

gens2 <- function(mc, mcl, mcn, newprof) {
  ac <- predict(mc, newdata=newprof, se.fit=TRUE)
  acl <- predict(mcl, newdata=newprof, se.fit=TRUE)
  acn <- predict(mcn, newdata=newprof, se.fit=TRUE)
  s <- data.frame(Mean = c(ac$fit,acl$fit,acn$fit),
                  lowerCI = c(ac$fit - qnorm(0.975)*ac$se.fit,
                              acl$fit - qnorm(0.975)*acl$se.fit,
                              acn$fit - qnorm(0.975)*acn$se.fit),
                  upperCI = c(ac$fit + qnorm(0.975)*ac$se.fit,
                              acl$fit + qnorm(0.975)*acl$se.fit,
                              acn$fit + qnorm(0.975)*acn$se.fit),
                  pref = rep(c("Context-Based",
                               "Local-Only",
                               "National-Only"), each=nrow(newprof)))
  s <- cbind(s, rbind(newprof,newprof,newprof))
  return(s)
}

addstriplabel <- function(p) {
  
  require(grid)
  require(gtable)
  
  # Labels 
  labelT = "Common Signal Bias"
  labelR = "Geographic Sorting"
  
  # Get the ggplot grob
  z <- ggplotGrob(p)
  
  # Get the positions of the strips in the gtable: t = top, l = left, ...
  posT <- subset(z$layout, grepl("strip-t", name), select = t:r)
  posR <- subset(z$layout, grepl("strip-r", name), select = t:r)
  
  # Add a new row on top of current top strips
  height <- unit(0.5,"cm") #z$heights[min(posT$t)]  # height of current top strips
  z <- gtable_add_rows(z, height, min(posT$t)-1)
  width <- unit(0.5,"cm") #z$widths[max(posR$r)] 
  z <- gtable_add_cols(z, width, max(posR$r))
  
  # Construct the new strip grobs
  stripT <- gTree(name = "Strip_top", children = gList(
    rectGrob(gp = gpar(col = NA, fill = "white")),
    textGrob(labelT, gp = gpar(fontsize = 12, col = "black", fontface="bold"))))
  stripR <- gTree(name = "Strip_right", children = gList(
    rectGrob(gp = gpar(col = NA, fill = "white")),
    textGrob(labelR, rot = -90, gp = gpar(fontsize = 12, col = "black", fontface="bold"))))
  
  # Position the grobs in the gtable
  z <- gtable_add_grob(z, stripR, t = min(posR$t)+1, l = max(posR$r) + 1, 
                       b = max(posR$b)+1, name = "strip-right")
  z <- gtable_add_grob(z, stripT, t = min(posT$t), l = min(posT$l), r = max(posT$r), 
                       name = "strip-top")
  
  # Add small gaps between strips
  z <- gtable_add_cols(z, unit(1/5, "line"), max(posR$r))
  z <- gtable_add_rows(z, unit(1/5, "line"), min(posT$t))
  
  return(z)
}

#'
#' ## Visualization
#'

p1 <- 
  plot_simu(gens(m1c,m1s,newprof), 
            name.x="knowmean1",
            name.linetype = "pref",
            name.fill = "pref",
            label.linetype = "Uninformed Voting",
            label.fill = "Uninformed Voting",
            name.facet.y="alikecat",
            name.facet.x="signalbias") + 
  scale_y_continuous(breaks = c(0.03,0.05,0.07), labels=c("3%","5%","7%")) + 
  scale_x_reverse(breaks = c(0.45,0.55,0.65), labels=c("0.4","0.2","0.0")) + 
  ylab("Deviation from \nInformed Red Share") + 
  theme(axis.text.x = element_text(angle=45, hjust=1)) + 
  xlab("Knowledge Inequality Between Groups \n(Red Group Mean - Blue Group Mean)")  
p1 <- addstriplabel(p1)
grid.draw(p1)

png_save(p1, file="outputs/abmres1.png")

p2 <- 
  plot_simu(gens(m2c,m2s,newprof), 
            name.x="knowmean1",
            name.linetype = "pref",
            name.fill = "pref",
            label.linetype = "Uninformed Voting",
            label.fill = "Uninformed Voting",
            name.facet.y="alikecat",
            name.facet.x="signalbias") + 
  scale_y_continuous(breaks = c(0.65,0.75,0.85), labels=c("65%","75%","85%")) + 
  ylab("Probability of Same Winner \nas Informed Vote") + 
  scale_x_reverse(breaks = c(0.45,0.55,0.65), labels=c("0.4","0.2","0.0")) + 
  theme(axis.text.x = element_text(angle=45, hjust=1)) + 
  xlab("Knowledge Inequality Between Groups \n(Red Group Mean - Blue Group Mean)")
p2 <- addstriplabel(p2)
grid.draw(p2)

png_save(p2, file="outputs/abmres2.png")

p3 <- 
  plot_simu(gens(m3c,m3s,newprof), 
            name.x="knowmean1",
            name.linetype = "pref",
            name.fill = "pref",
            label.linetype = "Uninformed Voting",
            label.fill = "Uninformed Voting",
            name.facet.y="alikecat",
            name.facet.x="signalbias") + 
  scale_y_continuous(breaks = c(-0.3,-0.2,-0.1,0.0), limits=c(-0.3,0.02)) + 
  ylab("Average Loss of Utility \nCompared to Informed Outcome") + 
  scale_x_reverse(breaks = c(0.45,0.55,0.65), labels=c("0.4","0.2","0.0")) + 
  theme(axis.text.x = element_text(angle=45, hjust=1)) + 
  xlab("Knowledge Inequality Between Groups \n(Red Group Mean - Blue Group Mean)")
p3 <- addstriplabel(p3)
grid.draw(p3)

png_save(p3, file="outputs/abmres3.png")

p4 <- 
  plot_simu(gens(m4c,m4s,newprof), 
            name.x="knowmean1",
            name.linetype = "pref",
            name.fill = "pref",
            label.linetype = "Uninformed Voting",
            label.fill = "Uninformed Voting",
            name.facet.y="alikecat",
            name.facet.x="signalbias") + 
  scale_y_continuous(breaks = c(-3,-2,-1,0), limits=c(-3,0.17)) + 
  ylab("Difference in Average Loss of Utility \n(Blue Group Loss - Red Group Loss)") + 
  scale_x_reverse(breaks = c(0.45,0.55,0.65), labels=c("0.4","0.2","0.0")) + 
  theme(axis.text.x = element_text(angle=45, hjust=1)) + 
  xlab("Knowledge Inequality Between Groups \n(Red Group Mean - Blue Group Mean)")
p4 <- addstriplabel(p4)
grid.draw(p4)

png_save(p4, file="outputs/abmres4.png")

p1c <- 
  plot_simu(gens2(m1c,m1cl,m1cn,newprof), 
            name.x="knowmean1",
            name.linetype = "pref",
            name.fill = "pref",
            label.linetype = "Uninformed Voting",
            label.fill = "Uninformed Voting",
            name.facet.y="alikecat",
            name.facet.x="signalbias") + 
  scale_y_continuous(breaks = c(0.03,0.05,0.07), labels=c("3%","5%","7%")) + 
  ylab("Deviation from \nInformed Red Share") + 
  scale_x_reverse(breaks = c(0.45,0.55,0.65), labels=c("0.4","0.2","0.0")) + 
  theme(axis.text.x = element_text(angle=45, hjust=1)) + 
  xlab("Knowledge Inequality Between Groups \n(Red Group Mean - Blue Group Mean)")
p1c <- addstriplabel(p1c)
grid.draw(p1c)

png_save(p1c, file="outputs/abmres1c.png")

p2c <- 
  plot_simu(gens2(m2c,m2cl,m2cn,newprof), 
            name.x="knowmean1",
            name.linetype = "pref",
            name.fill = "pref",
            label.linetype = "Uninformed Voting",
            label.fill = "Uninformed Voting",
            name.facet.y="alikecat",
            name.facet.x="signalbias") + 
  scale_y_continuous(breaks = c(0.65,0.75,0.85), labels=c("65%","75%","85%")) + 
  ylab("Probability of Same Winner \nas Informed Vote") + 
  scale_x_reverse(breaks = c(0.45,0.55,0.65), labels=c("0.4","0.2","0.0")) + 
  theme(axis.text.x = element_text(angle=45, hjust=1)) + 
  xlab("Knowledge Inequality Between Groups \n(Red Group Mean - Blue Group Mean)")
p2c <- addstriplabel(p2c)
grid.draw(p2c)

png_save(p2c, file="outputs/abmres2c.png")

p3c <- 
  plot_simu(gens2(m3c,m3cl,m3cn,newprof), 
            name.x="knowmean1",
            name.linetype = "pref",
            name.fill = "pref",
            label.linetype = "Uninformed Voting",
            label.fill = "Uninformed Voting",
            name.facet.y="alikecat",
            name.facet.x="signalbias") + 
  scale_y_continuous(breaks = c(-0.3,-0.2,-0.1,0.0), limits=c(-0.36,0)) + 
  ylab("Average Loss of Utility \nCompared to Informed Outcome") + 
  scale_x_reverse(breaks = c(0.45,0.55,0.65), labels=c("0.4","0.2","0.0")) + 
  theme(axis.text.x = element_text(angle=45, hjust=1)) + 
  xlab("Knowledge Inequality Between Groups \n(Red Group Mean - Blue Group Mean)")
p3c <- addstriplabel(p3c)
grid.draw(p3c)

png_save(p3c, file="outputs/abmres3c.png")

p4c <- 
  plot_simu(gens2(m4c,m4cl,m4cn,newprof), 
            name.x="knowmean1",
            name.linetype = "pref",
            name.fill = "pref",
            label.linetype = "Uninformed Voting",
            label.fill = "Uninformed Voting",
            name.facet.y="alikecat",
            name.facet.x="signalbias") + 
  #scale_y_continuous(breaks = c(-0.25,-0.15,-0.05)) + 
  ylab("Difference in Average Loss of Utility \n(Blue Group Loss - Red Group Loss)") + 
  scale_x_reverse(breaks = c(0.45,0.55,0.65), labels=c("0.4","0.2","0.0")) + 
  theme(axis.text.x = element_text(angle=45, hjust=1)) + 
  xlab("Knowledge Inequality Between Groups \n(Red Group Mean - Blue Group Mean)")
p4c <- addstriplabel(p4c)
grid.draw(p4c)

png_save(p4c, file="outputs/abmres4c.png")

# 1. actual      
# 2. actual1    
# 3. actual2      
# 4. signal       
# 5. wsignal      
# 6. context      
# 7. context1    
# 8. context2

