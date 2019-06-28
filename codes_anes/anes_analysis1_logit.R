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
#' Set important parameters
#'

# Presidential Election Years
pyears <- seq(1972,2016,4)


#'
#' ## Run Logistic Regression
#'

# With Subjective Knowledge Variable
m1<- run_logit(d1,f1,pyears,"m1",projdir,1)
m1sq <- run_logit(d1,f1sq,pyears,"m1sq",projdir)
m1ext <- run_logit(d1,f2sq,pyears,"m1ext",projdir)
# Squared Ideological Distance Model Performs Better 8/12
m1$AIC - m1sq$AIC
m1$logLik - m1sq$logLik
# Adding more demographic variables not effective (reduce AIC in 10/12)
m1ext$AIC - m1sq$AIC
# Kept Cases
m1sq$nobs/table(d1$year)
m1ext$nobs/table(d1$year)

# With Objective Knowledge Variable
m2 <- run_logit(d2,f1,pyears,"m2",projdir)
m2sq <- run_logit(d2,f1sq,pyears,"m2sq",projdir)
m2ext <- run_logit(d2,f2sq,pyears,"m2ext",projdir)
# Squared Ideological Distance Model Performs Better 6/12
m2$AIC - m2sq$AIC
m2$logLik - m2sq$logLik
# Adding more demographic variables not effective (reduce AIC in 8/12)
m2ext$AIC - m2sq$AIC
# Kept Cases
m2$nobs/table(d2$year)
m2ext$nobs/table(d2$year)

#'
#' ## Predicted Probabilities from Logistic Regression
#'

m1sq_pred_1972 <- run_pred(d1,m1sq,1972)
m2sq_pred_1972 <- run_pred(d2,m2sq,1972)
m1sq_pred_1976 <- run_pred(d1,m1sq,1976)
m2sq_pred_1976 <- run_pred(d2,m2sq,1976)
m1sq_pred_1980 <- run_pred(d1,m1sq,1980)
m2sq_pred_1980 <- run_pred(d2,m2sq,1980)
m1sq_pred_1984 <- run_pred(d1,m1sq,1984)
m2sq_pred_1984 <- run_pred(d2,m2sq,1984)
m1sq_pred_1988 <- run_pred(d1,m1sq,1988)
m2sq_pred_1988 <- run_pred(d2,m2sq,1988)
m1sq_pred_1992 <- run_pred(d1,m1sq,1992)
m2sq_pred_1992 <- run_pred(d2,m2sq,1992)
m1sq_pred_1996 <- run_pred(d1,m1sq,1996)
m2sq_pred_1996 <- run_pred(d2,m2sq,1996)
m1sq_pred_2000 <- run_pred(d1,m1sq,2000)
m2sq_pred_2000 <- run_pred(d2,m2sq,2000)
m1sq_pred_2004 <- run_pred(d1,m1sq,2004)
m2sq_pred_2004 <- run_pred(d2,m2sq,2004)
m1sq_pred_2008 <- run_pred(d1,m1sq,2008)
m2sq_pred_2008 <- run_pred(d2,m2sq,2008)
m1sq_pred_2012 <- run_pred(d1,m1sq,2012)
m2sq_pred_2012 <- run_pred(d2,m2sq,2012)
m1sq_pred_2016 <- run_pred(d1,m1sq,2016)
m2sq_pred_2016 <- run_pred(d2,m2sq,2016)

m1sq_pred_1972 <- export_predplot(m1sq_pred_1972,projdir,"m1sq_1972")
m2sq_pred_1972 <- export_predplot(m2sq_pred_1972,projdir,"m2sq_1972")
m1sq_pred_1976 <- export_predplot(m1sq_pred_1976,projdir,"m1sq_1976")
m2sq_pred_1976 <- export_predplot(m2sq_pred_1976,projdir,"m2sq_1976")
m1sq_pred_1980 <- export_predplot(m1sq_pred_1980,projdir,"m1sq_1980")
m2sq_pred_1980 <- export_predplot(m2sq_pred_1980,projdir,"m2sq_1980")
m1sq_pred_1984 <- export_predplot(m1sq_pred_1984,projdir,"m1sq_1984")
m2sq_pred_1984 <- export_predplot(m2sq_pred_1984,projdir,"m2sq_1984")
m1sq_pred_1988 <- export_predplot(m1sq_pred_1988,projdir,"m1sq_1988")
m2sq_pred_1988 <- export_predplot(m2sq_pred_1988,projdir,"m2sq_1988")
m1sq_pred_1992 <- export_predplot(m1sq_pred_1992,projdir,"m1sq_1992")
m2sq_pred_1992 <- export_predplot(m2sq_pred_1992,projdir,"m2sq_1992")
m1sq_pred_1996 <- export_predplot(m1sq_pred_1996,projdir,"m1sq_1996")
m2sq_pred_1996 <- export_predplot(m2sq_pred_1996,projdir,"m2sq_1996")
m1sq_pred_2000 <- export_predplot(m1sq_pred_2000,projdir,"m1sq_2000")
m2sq_pred_2000 <- export_predplot(m2sq_pred_2000,projdir,"m2sq_2000")
m1sq_pred_2004 <- export_predplot(m1sq_pred_2004,projdir,"m1sq_2004")
m2sq_pred_2004 <- export_predplot(m2sq_pred_2004,projdir,"m2sq_2004")
m1sq_pred_2008 <- export_predplot(m1sq_pred_2008,projdir,"m1sq_2008")
m2sq_pred_2008 <- export_predplot(m2sq_pred_2008,projdir,"m2sq_2008")
m1sq_pred_2012 <- export_predplot(m1sq_pred_2012,projdir,"m1sq_2012")
m2sq_pred_2012 <- export_predplot(m2sq_pred_2012,projdir,"m2sq_2012")
m1sq_pred_2016 <- export_predplot(m1sq_pred_2016,projdir,"m1sq_2016")
m2sq_pred_2016 <- export_predplot(m2sq_pred_2016,projdir,"m2sq_2016")

## Create the Table of Estimates ...

m1sqprtab <- rbind(extEDV(m1sq_pred_1972),
                 extEDV(m1sq_pred_1976),
                 extEDV(m1sq_pred_1980),
                 extEDV(m1sq_pred_1984),
                 extEDV(m1sq_pred_1988),
                 extEDV(m1sq_pred_1992),
                 extEDV(m1sq_pred_1996),
                 extEDV(m1sq_pred_2000),
                 extEDV(m1sq_pred_2004),
                 extEDV(m1sq_pred_2008),
                 extEDV(m1sq_pred_2012),
                 extEDV(m1sq_pred_2016))
m1sqprtab_pvi0 <- m1sqprtab[m1sqprtab$type=="uninformed" & m1sqprtab$coef=="pvi",][,c("est","lCI","uCI")]
m1sqprtab_pvi1 <- m1sqprtab[m1sqprtab$type=="informed" & m1sqprtab$coef=="pvi",][,c("est","lCI","uCI")]
m1sqprtab_inc0 <- m1sqprtab[m1sqprtab$type=="uninformed" & m1sqprtab$coef=="inc",][,c("est","lCI","uCI")]
m1sqprtab_inc1 <- m1sqprtab[m1sqprtab$type=="informed" & m1sqprtab$coef=="inc",][,c("est","lCI","uCI")]
rownames(m1sqprtab_pvi0) <- rownames(m1sqprtab_pvi1) <- 
rownames(m1sqprtab_inc0) <- rownames(m1sqprtab_inc1) <- seq(1972,2016,4)


p <- plot_coef(list(m1sqprtab_pvi0,m1sqprtab_pvi1,m1sqprtab_inc0,m1sqprtab_inc1),
          overlap.names = rep(c("Uninformed","Informed"),2),
          facet.names = rep(c("National PVI (10%)",
                              "Incumbent Party is Republican"),each=2),
          facet.x.scale = "free", y.title="Voter Profile Year") + 
  theme(plot.caption = element_text(hjust=0))

foottxt <- paste("Note: The coefficents are from Estimated Dependent Variable (EDV) model (Lewis and Linzer 2005) with dependent \nvariable as the weighted average of predicted probability of Republican vote based on voter profile of each presidential \nelection year and imputed knowledge of 0 (as if uninformed) and 1 (as if informed).")
p <- plot_footnote(p, foottxt, caption=TRUE)

ggsave(paste(projdir, "/outputs/m1sq_anespredtable.png", sep=""),
       p, w=6.5, h=5)

m2sqprtab <- rbind(extEDV(m2sq_pred_1972),
                 extEDV(m2sq_pred_1976),
                 extEDV(m2sq_pred_1980),
                 extEDV(m2sq_pred_1984),
                 extEDV(m2sq_pred_1988),
                 extEDV(m2sq_pred_1992),
                 extEDV(m2sq_pred_1996),
                 extEDV(m2sq_pred_2000),
                 extEDV(m2sq_pred_2004),
                 extEDV(m2sq_pred_2008),
                 extEDV(m2sq_pred_2012),
                 extEDV(m2sq_pred_2016))
m2sqprtab_pvi0 <- m2sqprtab[m2sqprtab$type=="uninformed" & m2sqprtab$coef=="pvi",][,c("est","lCI","uCI")]
m2sqprtab_pvi1 <- m2sqprtab[m2sqprtab$type=="informed" & m2sqprtab$coef=="pvi",][,c("est","lCI","uCI")]
m2sqprtab_inc0 <- m2sqprtab[m2sqprtab$type=="uninformed" & m2sqprtab$coef=="inc",][,c("est","lCI","uCI")]
m2sqprtab_inc1 <- m2sqprtab[m2sqprtab$type=="informed" & m2sqprtab$coef=="inc",][,c("est","lCI","uCI")]
rownames(m2sqprtab_pvi0) <- rownames(m2sqprtab_pvi1) <- 
  rownames(m2sqprtab_inc0) <- rownames(m2sqprtab_inc1) <- seq(1972,2016,4)

p <- plot_coef(list(m2sqprtab_pvi0,m2sqprtab_pvi1,m2sqprtab_inc0,m2sqprtab_inc1),
               overlap.names = rep(c("As If Uninformed","As If Informed"),2),
               facet.names = rep(c("National PVI (10%)",
                                   "Incumbent Party is Republican"),each=2),
               facet.x.scale = "free", y.title="Voter Profile Year") + 
  theme(plot.caption = element_text(hjust=0))

foottxt <- paste("Note: The coefficents are from Estimated Dependent Variable (EDV) model (Lewis and Linzer 2005) with dependent \nvariable as the weighted average of predicted probability of Republican vote based on voter profile of each presidential \nelection year and imputed objective knowledge of 0 (as if uninformed) and 1 (as if informed).")
p <- plot_footnote(p, foottxt, caption=TRUE)

ggsave(paste(projdir, "/outputs/m2sq_anespredtable.png", sep=""),
       p, w=6.5, h=5)

#'
#' # Extract Parameters for Simulation
#'

# Informed Parameter
## of Squared Ideological Distance
(param_iderepadv <- colMeans(do.call("rbind",lapply(m1sq$gd, function(k) k$gd1[1,c(1:5)]))))
## of Economic Evaluation -2:2
(param_evecon <- colMeans(do.call("rbind",lapply(m1sq$gd, function(k) k$gd1[3,c(1:5)]))))

# Unconditional Parameter
## of PID (combining both Informed and Uninformed) 0-1
(param_pid <- colMeans(rbind(do.call("rbind",lapply(m1sq$gd, function(k) k$gd0[2,c(1:5)])),
                             do.call("rbind",lapply(m1sq$gd, function(k) k$gd1[2,c(1:5)])))))

# Uninformed Parameter
## of Partisan Advantage

m1sq_logit_1972 <- run_pred(d1,m1sq,1972,type="asis")
m1sq_logit_1976 <- run_pred(d1,m1sq,1976,type="asis")
m1sq_logit_1980 <- run_pred(d1,m1sq,1980,type="asis")
m1sq_logit_1984 <- run_pred(d1,m1sq,1984,type="asis")
m1sq_logit_1988 <- run_pred(d1,m1sq,1988,type="asis")
m1sq_logit_1992 <- run_pred(d1,m1sq,1992,type="asis")
m1sq_logit_1996 <- run_pred(d1,m1sq,1996,type="asis")
m1sq_logit_2000 <- run_pred(d1,m1sq,2000,type="asis")
m1sq_logit_2004 <- run_pred(d1,m1sq,2004,type="asis")
m1sq_logit_2008 <- run_pred(d1,m1sq,2008,type="asis")
m1sq_logit_2012 <- run_pred(d1,m1sq,2012,type="asis")
m1sq_logit_2016 <- run_pred(d1,m1sq,2016,type="asis")

m1sq_logit_1972 <- export_predplot(m1sq_logit_1972,projdir,"m1sq_logit_1972",ylab="asis")
m1sq_logit_1976 <- export_predplot(m1sq_logit_1976,projdir,"m1sq_logit_1976",ylab="asis")
m1sq_logit_1980 <- export_predplot(m1sq_logit_1980,projdir,"m1sq_logit_1980",ylab="asis")
m1sq_logit_1984 <- export_predplot(m1sq_logit_1984,projdir,"m1sq_logit_1984",ylab="asis")
m1sq_logit_1988 <- export_predplot(m1sq_logit_1988,projdir,"m1sq_logit_1988",ylab="asis")
m1sq_logit_1992 <- export_predplot(m1sq_logit_1992,projdir,"m1sq_logit_1992",ylab="asis")
m1sq_logit_1996 <- export_predplot(m1sq_logit_1996,projdir,"m1sq_logit_1996",ylab="asis")
m1sq_logit_2000 <- export_predplot(m1sq_logit_2000,projdir,"m1sq_logit_2000",ylab="asis")
m1sq_logit_2004 <- export_predplot(m1sq_logit_2004,projdir,"m1sq_logit_2004",ylab="asis")
m1sq_logit_2008 <- export_predplot(m1sq_logit_2008,projdir,"m1sq_logit_2008",ylab="asis")
m1sq_logit_2012 <- export_predplot(m1sq_logit_2012,projdir,"m1sq_logit_2012",ylab="asis")
m1sq_logit_2016 <- export_predplot(m1sq_logit_2016,projdir,"m1sq_logit_2016",ylab="asis")


m1sqlgtab <- rbind(extEDV(m1sq_logit_1972),
                   extEDV(m1sq_logit_1976),
                   extEDV(m1sq_logit_1980),
                   extEDV(m1sq_logit_1984),
                   extEDV(m1sq_logit_1988),
                   extEDV(m1sq_logit_1992),
                   extEDV(m1sq_logit_1996),
                   extEDV(m1sq_logit_2000),
                   extEDV(m1sq_logit_2004),
                   extEDV(m1sq_logit_2008),
                   extEDV(m1sq_logit_2012),
                   extEDV(m1sq_logit_2016))
m1sqlgtab_pvi0 <- m1sqlgtab[m1sqlgtab$type=="uninformed" & m1sqlgtab$coef=="pvi",][,c("est","lCI","uCI")]
m1sqlgtab_pvi1 <- m1sqlgtab[m1sqlgtab$type=="informed" & m1sqlgtab$coef=="pvi",][,c("est","lCI","uCI")]
m1sqlgtab_inc0 <- m1sqlgtab[m1sqlgtab$type=="uninformed" & m1sqlgtab$coef=="inc",][,c("est","lCI","uCI")]
m1sqlgtab_inc1 <- m1sqlgtab[m1sqlgtab$type=="informed" & m1sqlgtab$coef=="inc",][,c("est","lCI","uCI")]
rownames(m1sqlgtab_pvi0) <- rownames(m1sqlgtab_pvi1) <- 
  rownames(m1sqlgtab_inc0) <- rownames(m1sqlgtab_inc1) <- seq(1972,2016,4)


p <- plot_coef(list(m1sqlgtab_pvi0,m1sqlgtab_pvi1,m1sqlgtab_inc0,m1sqlgtab_inc1),
               overlap.names = rep(c("As If Uninformed","As If Informed"),2),
               facet.names = rep(c("National PVI (10%)",
                                   "Incumbent Party is Republican"),each=2),
               facet.x.scale = "free", y.title="Voter Profile Year") + 
  theme(plot.caption = element_text(hjust=0))

foottxt <- paste("Note: The coefficents are from Estimated Dependent Variable (EDV) model (Lewis and Linzer 2005) with dependent \nvariable as the weighted average of predicted logit of Republican vote based on voter profile of each presidential \nelection year and imputed knowledge of 0 (as if uninformed) and 1 (as if informed).")
p <- plot_footnote(p, foottxt, caption=TRUE)

ggsave(paste(projdir, "/outputs/m1sq_aneslogittable.png", sep=""),
       p, w=6.5, h=5)


tmp <- m1sqlgtab[m1sqlgtab$type=="uninformed",]
tmp$med <- NA
tmp <- tmp[,c("est","med","se","lCI","uCI")]
colnames(tmp) <- c("Mean","Median","SE","lowerCI","upperCI")
(param_pvi_nation <- colMeans(rbind(tmp[grep("pvi_nation",rownames(tmp)),]))/10)
## of Republican Incumbency
(param_repinc2 <- colMeans(rbind(tmp[grep("repinc",rownames(tmp)),])))

param_voting <- rbind(param_iderepadv,
      param_evecon,
      param_pid, 
      param_pvi_nation,
      param_repinc2)
param_voting <- as.data.frame(param_voting)

saveRDS(param_voting, paste(projdir,"data/param_voting.rds",sep="/"))

# # 
param_society <- data.frame(year=pyears)

# Ideology & Economic Evaluation Mean (To See Overall Average and rate of change)
param_society$evecon_mean <- sapply(pyears, function(k) wtd.mean(d1[d1$year==k,]$evecon,
                                                                 d1[d1$year==k,]$w,na.rm=TRUE))
mean(param_society$evecon_mean) # Slightly Negative on Average
sd(param_society$evecon_mean) # Standard Deviaiton in the Change
param_society$iderep_mean <- sapply(pyears, function(k) wtd.mean(d1[d1$year==k,]$iderep,
                                                                 d1[d1$year==k,]$w,na.rm=TRUE))
mean(param_society$iderep_mean) # approx 0.85
param_society$idedem_mean <- sapply(pyears, function(k) wtd.mean(d1[d1$year==k,]$idedem,
                                                                 d1[d1$year==k,]$w,na.rm=TRUE))
mean(param_society$idedem_mean) # approx 0.85
param_society$idedistrep_sd <- sapply(pyears, function(k) sqrt(wtd.var(d1[d1$year==k,]$iderep - d1[d1$year==k,]$ideself,
                                                                 d1[d1$year==k,]$w,na.rm=TRUE)))
mean(param_society$idedistrep_sd) # approx 2
param_society$idedistdem_sd <- sapply(pyears, function(k) sqrt(wtd.var(d1[d1$year==k,]$idedem - d1[d1$year==k,]$ideself,
                                                                 d1[d1$year==k,]$w,na.rm=TRUE)))
mean(param_society$idedistdem_sd) # approx 2

# Self Ideology Standard Deviation
# all
param_society$ideself_sd <- sapply(pyears, function(k) sqrt(wtd.var(d1[d1$year==k,]$ideself, 
                                                         d1[d1$year==k,]$w,na.rm=TRUE)))
mean(param_society$ideself_sd) # About 1
# rep
param_society$ideselfrep_sd <- sapply(pyears, function(k) sqrt(wtd.var(d1[d1$year==k & d1$pid > 0,]$ideself, 
                                                               d1[d1$year==k & d1$pid > 0,]$w,na.rm=TRUE)))
mean(param_society$ideselfrep_sd) # About 1
# dem
param_society$ideselfdem_sd <- sapply(pyears, function(k) sqrt(wtd.var(d1[d1$year==k & d1$pid < 0,]$ideself, 
                                                                  d1[d1$year==k & d1$pid < 0,]$w,na.rm=TRUE)))
mean(param_society$ideselfdem_sd) # About 1

# Self Ideology Location
# all
param_society$ideself_mean <- sapply(pyears, function(k) (wtd.mean(d1[d1$year==k,]$ideself, 
                                                                    d1[d1$year==k,]$w,na.rm=TRUE)))
mean(param_society$ideself_mean) # 0.5
# rep
param_society$ideselfrep_mean <- sapply(pyears, function(k) (wtd.mean(d1[d1$year==k & d1$pid > 0,]$ideself, 
                                                                       d1[d1$year==k & d1$pid > 0,]$w,na.rm=TRUE)))
mean(param_society$ideselfrep_mean) # About 1
# dem
param_society$ideselfdem_mean <- sapply(pyears, function(k) (wtd.mean(d1[d1$year==k & d1$pid < 0,]$ideself, 
                                                                       d1[d1$year==k & d1$pid < 0,]$w,na.rm=TRUE)))
mean(param_society$ideselfdem_mean) # About 1

# Knowledge Distribution Mean
param_society$knolwledge_mean <- sapply(pyears, function(k) (wtd.mean(d1[d1$year==k,]$info, 
                                                                       d1[d1$year==k,]$w,na.rm=TRUE)))
mean(param_society$knolwledge_mean) # 0.25
# Knowledge Distribution SD
param_society$knolwledge_sd <- sapply(pyears, function(k) sqrt(wtd.var(d1[d1$year==k,]$info, 
                                                                  d1[d1$year==k,]$w,na.rm=TRUE)))
mean(param_society$knolwledge_sd) # 0.25

saveRDS(param_society, paste(projdir,"data/param_society.rds",sep="/"))

#'
#' # Save Workspace
#'

rm(d,d1,d2,tmp)
save.image(paste(projdir, "/codes_anes/processing/anes_analysis1_logit.rda", sep=""))

# load(paste(projdir, "/codes_anes/processing/anes_analysis1_logit.rda", sep=""))
