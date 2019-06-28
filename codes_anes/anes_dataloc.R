#' ---
#' title: "ANES Data Locations"
#' author: "Gento Kato"
#' date: "Apr 16, 2018"
#' ---
#' 

# Project Directory
library(rprojroot);library(rstudioapi)
if (rstudioapi::isAvailable()==TRUE) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path)); 
} 
projdir <- find_root(has_file("thisishome.txt"))
cat(paste("Working Directory Set to:\n",projdir))
setwd(projdir)

# Dataset Names (As it is from ANES Website)
dataname <- c("anes_timeseries_cdf.dta", #1948-2016 Cummulative (Downloaded Apr 22/2019)
              "NES1972.dta", # 1972 (Downloaded Oct 2/2017)
              "NES1976.dta", # 1976 (Downloaded Oct 2/2017)
              "NES1980.dta", # 1980 (Downloaded Oct 2/2017)
              "NES1984.dta", # 1984 (Downloaded Oct 2/2017)
              "NES1988.dta", # 1988 (Downloaded Oct 2/2017)
              "NES1992.dta", # 1992 (Downloaded Oct 2/2017)
              "nes96.dta",   # 1996 (Downloaded Oct 2/2017)
              "anes2000TS.dta", # 2000 (Downloaded Oct 2/2017)
              "anes2004TS.dta", # 2004 (Downloaded Oct 2/2017)
              "anes_timeseries_2008.dta", # 2008 (Downloaded Oct 2/2017)
              "anes_timeseries_2012.dta", # 2012 (Downloaded Oct 2/2017)
              "anes_timeseries_2016_Stata13.dta", # 2016 (Downloaded Nov 2/2018)
              "NES1990.dta", 
              # 1990 (Downloaded Nov 2/2018)
              # Needed to fill education data for panel variables in 1992
              "Occupation and Industry codes.sav", 
              # 2008 Occupation/Industry Open-ended Codes (Downloaded Nov 2/2018) 
              # Needed to fill occupation data for 2008
              "occ12_RFpred.csv",
              # Machine Learned Occupation Category (3 cat) in ANES2012 
              # Predicted using RF classifier trained on ANES2008, 2016 Data.
              # Check learn_occupation... files for the procedure.
              # Needed to fill occulation data for 2012
              "objkn_irt.rds",
              # Objective Knowledge Indexes created by Bayesian IRT.
              # Check create_objkn_irt.R for the procedure.
              "house_candidate_names_1972.csv",
              # 1972 House Candidate Names Correspondence. Extracted from appendix.
              "state_id_correspondence_1972.csv",
              # 1972 State ID Correspondence. Extracted from vardoc VAR720004
              "Political knowledge - All codes2.sav",
              # 2008 Office Recognition Coding Data
              "cqvec_president_nation.rds",
              # National Level Voting Results
              "cqvec_president_state.rds"
              # State Level Voting Results
              )
# Data ID (Use Later)
dataid <- c("cum",seq(1972,2016,4),1990,"occ08","occ12","objkn",
            "hn72","sc72","or08","cqna","cqst")

# Data Folder Locations 
# (Corresponding to "dataname" files. Change to the local directory)
datafolder <- 
  c("D:/BoxSync/Data/ANES/ANES 1948-2016 Cummulative/data/", #1948-2016 Cummulative
    "D:/BoxSync/Data/ANES/ANES 1972 Time Series/data/", # 1972
    "D:/BoxSync/Data/ANES/ANES 1976 Time Series/data/", # 1976
    "D:/BoxSync/Data/ANES/ANES 1980 Time Series/data/", # 1980
    "D:/BoxSync/Data/ANES/ANES 1984 Time Series/data/", # 1984
    "D:/BoxSync/Data/ANES/ANES 1988 Time Series/data/", # 1988
    "D:/BoxSync/Data/ANES/ANES 1992 Time Series/data/", # 1992
    "D:/BoxSync/Data/ANES/ANES 1996 Time Series/data/", # 1996
    "D:/BoxSync/Data/ANES/ANES 2000 Time Series/data/", # 2000
    "D:/BoxSync/Data/ANES/ANES 2004 Time Series/data/", # 2004
    "D:/BoxSync/Data/ANES/ANES 2008 Time Series/data/", # 2008
    "D:/BoxSync/Data/ANES/ANES 2012 Time Series/data/", # 2012
    "D:/BoxSync/Data/ANES/ANES 2016 Time Series/data/", # 2016
    "D:/BoxSync/Data/ANES/ANES 1990 Time Series/data/", # 1990
    "D:/BoxSync/Data/ANES/ANES 2008 Time Series/data/anes_timeseries_2008_occupation_industry/", # 2008 Occupation
    paste0(projdir,"/data/"), # 2012 Machine-Learned Occupation Category
    paste0(projdir,"/data/"), # Objective Knowledge Indexes
    "D:/BoxSync/Data/ANES/ANES 1972 Time Series/data/", # 1972 House Candidate Names
    "D:/BoxSync/Data/ANES/ANES 1972 Time Series/data/", # 1972 State ID Correspondence 
    "D:/BoxSync/Data/ANES/ANES 2008 Time Series/data/ANES2008TS_OfficeRecognition/", # 2008 Office Recognition Codes
    "D:/BoxSync/Data/CQ_VEC/", # Aggregated voting results
    "D:/BoxSync/Data/CQ_VEC/" # Aggregated voting results
    ) 
# Generate Data Loacations
dataloc <- paste(datafolder,dataname,sep="")