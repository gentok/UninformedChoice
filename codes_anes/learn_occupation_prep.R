#' ---
#' title: "Prepare Occupation Data 08, 12, 16 for Machine Learning"
#' author: "Gento Kato"
#' date: "November 3, 2018"
#' ---
#' 

#' Set Working Directory to ProjectDirectory
library(rprojroot);library(rstudioapi)
if (rstudioapi::isAvailable()==TRUE) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path)); 
} 
projdir <- find_root(has_file("thisishome.txt"))
cat(paste("Working Directory Set to:\n",projdir))
setwd(projdir)
getwd()

#' Required Packages
require(readstata13)
require(foreign)

#' Clear Workspace
rm(list=ls())

## Locations of Redacted Open End Response Files
oloc <- c("D:/BoxSync/Data/ANES/ANES 2008 Time Series/data/anes2008TSopenends_redacted_Dec2012Revision.xls",
          "D:/BoxSync/Data/ANES/ANES 2012 Time Series/data/anes2012TS_openends.xlsx",
          "D:/BoxSync/Data/ANES/ANES 2016 Time Series/data/anes_timeseries_2016_redacted_openends_V161291a.csv",
          "D:/BoxSync/Data/ANES/ANES 2016 Time Series/data/anes_timeseries_2016_redacted_openends_V161282a.csv")
# For the last two files, you need to re-save specific sheets of  
# anes_timeseries_2016_redacted_openends.xlsx as CSV files.

## 2008 Data
tloc2008 <- "D:/BoxSync/Data/ANES/ANES 2008 Time Series/data/anes_timeseries_2008.dta"
t2008 <- read.dta13(tloc2008, convert.factors = FALSE)
dim(t2008)

## 2008 Occupation Data
occ08loc <- "D:/BoxSync/Data/ANES/ANES 2008 Time Series/data/anes_timeseries_2008_occupation_industry/Occupation and Industry codes.sav"
#' Import Occupation data for 2008
occ08 <- read.spss(occ08loc, use.value.labels = FALSE,
                   to.data.frame=TRUE)

## 2012 Data
# tloc2012 <- "D:/BoxSync/Data/ANES/ANES 2012 Time Series/data/anes_timeseries_2012.dta"
# t2012 <- read.dta13(tloc2012, convert.factors = FALSE)
# dim(t2012)

## 2016 Data
tloc2016 <- "D:/BoxSync/Data/ANES/ANES 2016 Time Series/data/anes_timeseries_2016_Stata13.dta"
t2016 <- read.dta13(tloc2016, convert.factors = FALSE)
t2016 <- t2016[-936,] # Drop one ineligible respondent
dim(t2016)

require(xlsx)
# read.xlsx method requires sufficient memory in the computer
# If the method produces memory errors, try converting the original files into csv
# and use read.csv method

# 2008 Data 
o2008occnow <- read.xlsx(oloc[1], 12, startRow=2)
o2008occpast <- read.xlsx(oloc[1], 10, startRow=2)
table(o2008occnow$V080001==o2008occpast$V080001) # CheckID
o2008 <- data.frame(yr=2008,
                    id=o2008occnow$V080001)
o2008$occ <- as.character(o2008occnow$NA.)
o2008$occ[is.na(o2008occnow$NA.)] <- as.character(o2008occpast$NA.)[is.na(o2008occnow$NA.)]
table(!is.na(o2008$occ))

# 2012 Data
o2012 <- read.xlsx(oloc[2], 1, startRow=1)
o2012 <- o2012[,c("caseid","dem_occpast","dem_occnow")]
table(is.na(o2012$dem_occnow),is.na(o2012$dem_occpast))
o2012$occ <- o2012$dem_occnow
o2012$occ[is.na(o2012$dem_occnow)]  <- o2012$dem_occpast[is.na(o2012$dem_occnow)]
names(o2012) <- c("id","occnow","occpast","occ")
o2012$yr <- 2012
o2012$job <- NA
o2012$jobcat <- NA
o2012 <- o2012[,c("yr","id","occ","job","jobcat")]

# 2016 Data
o2016occnow <- read.csv(oloc[3], stringsAsFactors = FALSE)
dim(o2016occnow)
o2016occpast <- read.csv(oloc[4], stringsAsFactors = FALSE)
dim(o2016occpast)
o2016 <- data.frame(yr=2016,
                    id=t2016$V160001)
o2016$occ <- o2016occnow[match(t2016$V160001,o2016occnow$V160001),2]
o2016$occ[is.na(o2016occnow[match(t2016$V160001,o2016occnow$V160001),2])] <- 
  o2016occpast[match(t2016$V160001,o2016occpast$V160001),2][is.na(o2016occnow[match(t2016$V160001,o2016occnow$V160001),2])]
table(!is.na(o2016$occ))

# Conversion Function by Index
convindex <- function(old.var,index,missing.val){
  r <- old.var
  r[r %in% missing.val] <- NA
  if (is.null(index)) {
    return(r)
  }
  rt <- table(r) # Just a table
  if (length(rt)==length(index)){
    r <- index[match(r, names(rt))]
    return(r)
  } else {
    stop("incorrect index length")
  }
  
}

# Recode Occupations as Job Type
# Code Professional/Managerial as 1
# Code Clerical (administrative support)/ sales workers as 2
# Code Skilled (technician), semi-skilled, operatives and service workers as 3
# Code Laborers, except farm as 4, 
# Code  Farmers, farm managers, farm laborers and foremen; forestry and fishermen as 5
# see https://www.eeoc.gov/employers/eeo1survey/jobclassguide.cfm for reference

jobindex <- c(0, # Non-applicables (not working, etc...) (-1)
              1, # 1. Top Executives
              1, # 2. Advertising, Marketing, Promotions,
              1, # 3. Operations Specialties Managers
              1, # 4. Other management Occupations
              1, # 5. Business Operations Specialists
              1, # 6. Financial Specialists
              1, # 7. Computer Occupations
              1, # 8. Mathematical Science Occupations
              1, # 9. Architects, Surveyors, and Cartograp
              1, # 10. Engineers
              1, # 11. Drafters, Engineering Technicians,
              1, # 12. Life Scientists
              1, # 13. Physical Scientists
              1, # 14. Social Scientists and Related Worke
              1, # 15. Life, Physical, and Social Science
              1, # 16. Counselors, Social Workers, and Oth
              1, # 17. Religious Workers
              1, # 18. Lawyers, Judges, and related Worker
              1, # 19. Legal Support Workers
              1, # 20. Postsecondary Teachers
              1, # 21. Preschool, Primary, Secondary, and
              1, # 22. Other Teachers and Instructors
              1, # 23. Librarians, Curators, and Archivist
              1, # 24. Other Education, Training, and Libr
              1, # 25. Art and Design Workers
              1, # 26. Entertainers and Performers, Sports
              1, # 27. Media and Communication Workers
              1, # 28. Media and Communication Equipment W
              1, # 29. Health Diagnosing and Treating Prac
              3, # 30. Health Technologists and Technician
              3, # 31. Other Healthcare Practitioners and
              3, # 32. Nursing, Psychiatric, and Home Heal
              3, # 33. Occupational Therapy and Physical T
              3, # 34. Other Healthcare Support Occupation
              3, # 35. Supervisors of Protective Service W
              3, # 36. Fire Fighting and Prevention Worker
              3, # 37. Law Enforcement Workers
              3, # 38. Other Protective Service Workers
              3, # 39. Supervisors of Food Preparation and
              3, # 40. Cooks and Food Preparation Workers
              3, # 41. Food and Beverage Serving Workers
              3, # 42. Other Food Preparation and Serving
              3, # 43. Supervisors of Building and Grounds
              3, # 44. Building Cleaning and Pest Control
              3, # 45. Grounds Maintenance Workers
              3, # 46. Supervisor of Personal Care and Ser
              3, # 47. Animal Care and Service Workers
              3, # 48. Entertainment Attendants and Relate
              3, # 49. Funeral Service Workers
              3, # 50. Personal Appearance Workers
              3, # 51. Baggage Porters, Bellhops, and Conc
              3, # 52. Tour and Travel Guides
              3, # 53. Other Personal Care and Service Wor
              2, # 54. Supervisors of Sales Workers
              2, # 55. Retail Sales Workers
              2, # 56. Sales Representatives, Services
              2, # 57. Sales Representatives, Wholesale an
              2, # 58. Other Sales and Related Workers
              2, # 59. Supervisors of Office and Administr
              2, # 60. Communications Equipment Operators
              2, # 61. Financial Clerks
              2, # 62. Information and Record Clerks
              2, # 63. Matertial Recordingm Scheduling, Di
              2, # 64. Secretaries and Administrative Assi
              2, # 65. Other Office and Administrative Sup
              5, # 66. Supervisors of Farming, Fishing, an
              5, # 67. Agricultural Workers
              5, # 68. Fishing and Hunting Workers
              5, # 69. Forest, Conservation, and Logging W
              4, # 70. Supervisors of Construction and Ext
              4, # 71. Construction Trades Workers
              4, # 72. Helpers, Construction Trades
              4, # 73. Other Construction and Related Work
              4, # 74. Extraction Workers
              4, # 75. Supervisors of Installation, Mainte
              4, # 76. Electrical and Electronic Equipment
              4, # 77. Vehicle and Mobile Equipment Mechan
              4, # 78. Other Installation, Maintenance, an
              4, # 79. Supervisors of Production Workers
              4, # 80. Assemblers and Fabricators
              4, # 81. Food Processing Workers
              4, # 82. Metal Workers and Plastic Workers
              4, # 83. Printing Workers
              4, # 84. Textile, Apparel, and Furnishing Wo
              4, # 85. Woodworkers
              4, # 86. Plant and System Operators
              4, # 87. Other Production Occupations
              3, # 88. Supervisors of Transportation and M
              3, # 89. Air Transportation Workers
              3, # 90. Motor Vehicle Operators
              3, # 91. Rail Transportation Workers
              3, # 92. Water Transportation Workers
              3, # 93. Other Transportation Workers
              3, # 94. Material Moving Workers
              1, # 95. Military Officer Special and Tactic
              1, # 96. First-Line Enlisted Military Supervisors
              1) # 97. Military Enlisted Tactical Operatio
jobindexid <- seq(0,97,1)
length(jobindex)==length(jobindexid)

# 2008 Occupation
table(!is.na(occ08$COCode),!is.na(occ08$POCode))
# Substitute with Past Job if Current Job is Not Available
occ08$OCode <- occ08$COCode
occ08$OCode[is.na(occ08$COCode)] <- occ08$POCode[is.na(occ08$COCode)]
table(occ08$OCode, useNA="always")
# Attach Occupation Codes to t2008 Data
length(occ08$ID); length(t2008$V080001)
t2008$jobcode <- occ08$OCode[match(t2008$V080001,occ08$ID)]
t2008$jobcode[is.na(t2008$jobcode)] <- 0
table(t2008$jobcode, useNA="always")
# Categorize industry codes into 5 categories
jobindex08 <- jobindex[jobindexid %in% names(table(t2008$jobcode))]
length(table(t2008$jobcode))-length(jobindex08) # Should be 2
t2008$jobtype <- convindex(t2008$jobcode, jobindex08, c(996,997))
table(t2008$jobtype, useNA="always")
length(t2008$jobtype)-length(o2008$id) # Same Length
# Add to Occupatio Dataset
o2008$job <- t2008$jobcode
o2008$jobcat <- t2008$jobtype


# 2016 Occupation
table(t2016$V161282b, useNA="always") # Past
table(t2016$V161291b, useNA="always") # Current
# Substitute with Past Job if Current Job is Not Available
t2016$jobcode <- t2016$V161291b 
t2016$jobcode[t2016$V161291b %in% c(-1,98,99)] <- t2016$V161282b[t2016$V161291b %in% c(-1,98,99)]
t2016$jobcode[t2016$jobcode %in% c(-1)] <- 0
table(t2016$jobcode, useNA="always")
# Categorize industry codes into 5 categories
jobindex16 <- jobindex[jobindexid %in% names(table(t2016$jobcode))]
length(table(t2016$jobcode))-length(jobindex16) # Should be 2
t2016$jobtype <- convindex(t2016$jobcode, jobindex16, c(98,99))
table(t2016$jobtype, useNA="always")
length(t2016$jobtype)-length(o2016$id) # Same Length
# Add to Occupation Dataset
o2016$job <- t2016$jobcode
o2016$jobcat <- t2016$jobtype


#' New Combined Dataset
od <- rbind(o2008,o2012,o2016)

od$occ <- as.character(od$occ)
od$occ[grep("(<RF>|-1 Inapplicable|^$)",od$occ)] <- NA
od$occ <- gsub("\\[REDACTED( |, )","",od$occ, perl=TRUE)
od$occ <- gsub("\\[REFDACTED ","",od$occ, perl=TRUE)
od$occ <- gsub("\\[REDACT ","",od$occ, perl=TRUE)
od$occ <- gsub("{REDACTED ","",od$occ, perl=TRUE)
od$occ <- gsub("\\]","",od$occ, perl=TRUE)
od$occ <- gsub("\\[","",od$occ, perl=TRUE)
od$occ <- gsub("NAME( |.|,|$)","",od$occ, perl=TRUE)
od$occ <- gsub("COMPANY( |.|,|$)","",od$occ, perl=TRUE)
od$occ <- gsub("STORE( |.|,|$)","",od$occ, perl=TRUE)
od$occ <- gsub("COUNTY( |.|,|$)","",od$occ, perl=TRUE)
od$occ <- gsub("DETAIL( |.|,|$)","",od$occ, perl=TRUE)
od$occ <- gsub("DETAILS( |.|,|$)","",od$occ, perl=TRUE)
od$occ <- gsub("//"," ",od$occ)
od$occ <- gsub("/pi/"," ",od$occ)
od$occ[grep("^[0-9]*$",od$occ, perl=TRUE)] <- ""
od$occ[grep("(^i dont know$|^i don't know$)",od$occ, ignore.case=TRUE)] <- ""
od$occ[grep("<DK>",od$occ, perl=TRUE)] <- ""

od$worknow <- 1
od$worknow[od$yr %in% c(2008,2012) & is.na(od$occ) & is.na(od$ind)] <- 0
od$worknow[od$yr %in% c(2016) & od$jobcat==0] <- 0
od$worknow[od$yr %in% c(2016) & is.na(od$jobcat)] <- NA
table(od$worknow, useNA="always")

#odw <- od[which(od$worknow==1),-which(names(od)=="worknow")]

# Save Data
write.csv(od, paste0(projdir,"/codes_anes/processing/occlearn.csv"), row.names = FALSE)
#write.csv(odw, "../data/occlearn_worknow.csv", row.names = FALSE)


