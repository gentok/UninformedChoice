################################################################################# 
## File Name: import_data.R                                                    ##
## Date: 10 Apr 2019                                                           ##
## Author: Gento Kato                                                          ##
## Project: Uninformed Choice                                                  ##
## Purpose: Import Data and Data Locations                                     ##
################################################################################# 

#################
## Preparation ##
#################

## Clear Workspace
rm(list=ls())

## Library Required Packages
library(rprojroot); library(readstata13); #library(readr)

## Set Working Directory (Automatically) ##
if (rstudioapi::isAvailable()==TRUE) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path)); 
} 
projdir <- find_root(has_file("thisishome.txt"))
cat(paste("Working Directory Set to:\n",projdir))
setwd(projdir)

########################
## Set Data Locations ##
########################

## CHANGE THE FOLLOWING PATHS TO PATHS IN YOUR COMPUTER ##

## CCES2008 Stata Binary Data Location 
## Download from "https://dataverse.harvard.edu/dataset.xhtml?persistentId=hdl:1902.1/14003"
d08loc <- "D:/BoxSync/data/CCES/CCES2008/cces_2008_common.dta"

## CCES2016 Stata Binary Data Location 
## Download from "https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi%3A10.7910/DVN/GDF6Z0"
d16loc <- "D:/BoxSync/data/CCES/CCES2016/CCES16_Common_OUTPUT_Feb2018_VV.dta"

## National Level Election Returns (from CQ Voting and Elections Collection)
## See "https://github.com/gentok/cqvec" for how to create data 
pcq0loc <- "D:/BoxSync/data/CQ_VEC/cqvec_president_nation.rds" # President
hcq0loc <- "D:/BoxSync/data/CQ_VEC/cqvec_house_nation.rds" # House

## State Level Election Returns (from CQ Voting and Elections Collection)
## See "https://github.com/gentok/cqvec" for how to create data 
pcq1loc <- "D:/BoxSync/data/CQ_VEC/cqvec_president_state.rds" # President
hcq1loc <- "D:/BoxSync/data/CQ_VEC/cqvec_house_state.rds" # House

## County/district Level Election Returns (from CQ Voting and Elections Collection)
## See "https://github.com/gentok/cqvec" for how to create data 
pcq2loc <- "D:/BoxSync/data/CQ_VEC/cqvec_president_county_wAlaska.rds" # President
hcq2loc <- "D:/BoxSync/data/CQ_VEC/cqvec_house_district.rds" # House

#############################
## Load and Save CCES Data ##
#############################

d <- read.dta13(d08loc, convert.factors = T)
saveRDS(d,file="data/cces08x.rds")

d <- read.dta13(d08loc, convert.factors = F)
saveRDS(d,file="data/cces08y.rds")

d <- read.dta13(d16loc, convert.factors = T)
saveRDS(d,file="data/cces16x.rds")

d <- read.dta13(d16loc, convert.factors = F)
saveRDS(d,file="data/cces16y.rds")

################################
## Save  CQVEC Data Locations ##
################################

cqloc <- list(nation = pcq0loc, state = pcq1loc, county = pcq2loc)
saveRDS(cqloc,file="data/cqloc_president.rds")

cqloc <- list(nation = hcq0loc, state = hcq1loc, district = hcq2loc)
saveRDS(cqloc,file="data/cqloc_house.rds")