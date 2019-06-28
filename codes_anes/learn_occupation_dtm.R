#' ---
#' title: "Create Document Term Matrix for Machine Learning"
#' author: "Gento Kato"
#' date: "Novemeber 3, 2018"
#' ---

#' ##################
#' # Preparation
#' ##################

#' Clear Workspace
rm(list=ls())

#' Set Working Directory to ProjectDirectory
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

#' 
d <- read.csv(paste0(projdir,"/codes_anes/processing/occlearn.csv"), stringsAsFactors = F)

## Function to apply to 
stemExport <- function(d,vn,path){
  # Creating Corpus
  qt <- gsub("^[[:space:]]+$","",as.character(d[,vn]))
  emptyrows <- grep("^$|^NA$",qt)
  emptyrows <- c(emptyrows,which(is.na(qt)))
  if (length(emptyrows)>0){
    selectrows <- seq(1:length(qt))[-emptyrows]
  } else {
    selectrows <- seq(1:length(qt))
  }

  aqt <- SimpleCorpus(VectorSource(qt[selectrows]))
  length(qt);length(aqt)

  # Add id to newlabdt
  d$dtmid <- NA
  d$dtmid[selectrows] <- seq(1,length(aqt),1)
  write.csv(d,path, row.names=F)

  # Eliminating extra white spaces
  aqt <- tm_map(aqt, stripWhitespace)

  # Remove stopwords
  aqt <- tm_map(aqt, removeWords, stopwords("english"))

  # Stemming
  aqt <- tm_map(aqt, stemDocument)

  return(aqt)
}

occstem <- stemExport(d,"occ",paste0(projdir,"/codes_anes/processing/occlearn.csv"))
occdtm <- DocumentTermMatrix(occstem, control=list(removePunctuation = TRUE))
dim(occdtm)

# Convert it to a Dataframe
#occdtmx <- as.data.frame(as.matrix(occdtm))
#dim(occdtmx)
#head(occdtmx[,1:10])

saveRDS(occdtm,paste0(projdir,"/codes_anes/processing/occdtm.rds"))
#write.csv(occdtmx,"../data/occdtm.csv",row.names = F)
