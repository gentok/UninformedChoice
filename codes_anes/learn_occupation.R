#' ---
#' title: "Machine Learn & Predict Occupation Codes"
#' author: "Gento Kato"
#' date: "November 3, 2018"
#' ---

#' ##################
#' # Preparation
#' ##################

#' Clear Workspace
rm(list=ls())

#' Set Working Directory
library(rprojroot);library(rstudioapi)
if (rstudioapi::isAvailable()==TRUE) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path)); 
} 
projdir <- find_root(has_file("thisishome.txt"))
cat(paste("Working Directory Set to:\n",projdir))
setwd(projdir)
getwd()

# Library package
library(tm)
library(dplyr)
library(stringr)
library(randomForest)

occdata <- read.csv(paste0(projdir,"/codes_anes/processing/occlearn.csv"))
occdatax <- occdata[complete.cases(occdata$dtmid),]
occdatax0816 <- occdatax[occdatax$yr %in% c(2008,2016) & occdatax$jobcat %in% c(1,2,3,4,5),]
occdatax12 <- occdatax[occdatax$yr %in% c(2012),]
dim(occdatax0816)
occdtm <- readRDS(paste0(projdir,"/codes_anes/processing/occdtm.rds"))
occdtmx0816 <- occdtm[occdatax0816$dtmid,]
occdtmx12 <- occdtm[occdatax12$dtmid,]
dim(occdtmx0816)


# Create Training Set and Test Set
rn <- seq(1,dim(occdtmx0816)[1],1)
set.seed(1234573)
rn_train <- sort(sample(rn, round(max(rn)*0.8)))
rn_test <- rn[-which(rn %in% rn_train)]

occdtmx0816_train <- data.frame(as.matrix(occdtmx0816[rn_train,]))
occdtmx0816_test <- data.frame(as.matrix(occdtmx0816[rn_test,]))
occdtmx12 <- data.frame(as.matrix(occdtmx12))

# Make the DV 3 Categories
occdatax0816$jobcat3 <- occdatax0816$jobcat
occdatax0816$jobcat3[occdatax0816$jobcat %in% c(4,5)] <- 3
table(occdatax0816$jobcat3)

# Train Random Forest Classifier (Slow)
#+ eval=FALSE
set.seed(546)
occ.rf <- randomForest(x=occdtmx0816_train,
                       y=as.factor(occdatax0816[rn_train,]$jobcat3),
                       ntree=500, do.trace=TRUE)
saveRDS(occ.rf, "occ.rf.500.rds")
#+ echo=FALSE
occ.rf <- readRDS(paste0(projdir,"/codes_anes/processing/occ.rf.500.rds"))
#+
print(occ.rf)

# Testing the Quality of RF Classifier
occ.rf.pr.test <- predict(occ.rf, occdtmx0816_test)
(ptb <- table(occdatax0816[rn_test,]$jobcat3, occ.rf.pr.test))

# precision OK. All above 79%
ptb[1,1]/sum(ptb[,1]) # For Professional
ptb[2,2]/sum(ptb[,2]) # For Clerical 
ptb[3,3]/sum(ptb[,3]) # For Others 

# Recall
ptb[1,1]/sum(ptb[1,]) # For Professional
ptb[2,2]/sum(ptb[2,]) # For Clerical
ptb[3,3]/sum(ptb[3,]) # For Others

# Predicting ANES2012 Codes
occdatax12$jobcat_pred <- predict(occ.rf, occdtmx12)
table(occdatax12$jobcat_pred, useNA="always")

# Dataset
occ12 <- data.frame(id=occdatax12$id,
                    jobcat3=occdatax12$jobcat_pred)
write.csv(occ12, paste0(projdir,"/data/occ12_RFpred.csv"))
