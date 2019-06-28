#' ---
#' title: "Recoding ANES Data (Summary)"
#' author: "Gento Kato"
#' date: "Apr 10, 2018"
#' ---
#' 

#' ## Preparation 
#' 
#' Required Packages
#' 
require(readstata13)
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
#' ## Specifying Original Data Locations
#' 
source(paste(projdir,"codes_anes/anes_dataloc.R",sep="/"))
# Show Required Original Data and Data ID for Dataset Creation
cbind(dataid,dataname)
#' 
#' ## Recoding Cummulative Data
#' 
source(paste(projdir,"codes_anes/recode_cummulative.R",sep="/"))
#' 
#' ## Adjusting Variables from Data from Each Year
#' 
source(paste(projdir,"codes_anes/recode_eachyear.R",sep="/"))
#' 
#' ## Adding Contextual Variables
#' 
source(paste(projdir,"codes_anes/recode_context.R",sep="/"))
#'
#' ## Drop Web Sample
#'

#d <- d[d$mode!=4,]

#' 
#' ## Save Data
#'
saveRDS(d, file=paste(projdir,"data/anes_info_eff.rds",sep="/")) 
#save.dta13(d, paste(projdir,"data/anes_info_eff.dta",sep="/"))

#+ eval=FALSE, echo=FALSE
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# rmarkdown::render("recode_all.R",
#                    "pdf_document")
