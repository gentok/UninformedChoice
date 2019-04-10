#################################################################################
## File Name: CCES_pvi_mapping.R                                               ##
## Date: 03 Mar 2018                                                           ##
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
library(rprojroot);
library(questionr);
library(stringr)
library(scales)

## Set Working Directory (Automatically or Manually) ##
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)); #setwd("../") #In RStudio
projdir <- find_root(has_file("README.md")); projdir; setwd(projdir) #In Atom
#setwd("C:/GoogleDrive/Projects/Uninformed_Choice")
plotdir <- "papers/figures"

## packages
library(ggplot2)
library(ggmap)
library(maps)
#library(mapdata)
# Install Required Map Data
#devtools::install_github("UrbanInstitute/urbnmapr")

# Load Data
pvi08p <- readRDS("data/PVI08_president.rds")
#pvi08h <- readRDS("data/PVI08_house.rds")
pvi16p <- readRDS("data/PVI16_president.rds")
#pvi16h <- readRDS("data/PVI16_house.rds")

# Add Variables
pvi08p$region <- tolower(pvi08p$State)
pvi08p$allregion <- str_squish(tolower(paste(pvi08p$State, pvi08p$County_Original)))
pvi16p$region <- tolower(pvi16p$State)
pvi16p$allregion <- str_squish(tolower(paste(pvi16p$State, pvi16p$County_Original)))

## Additional Data
cq0loc <- "C:/GoogleDrive/Data/CQ_VEC/cqvec_president_nation.rds"
# cq1loc <- "C:/GoogleDrive/Data/CQ_VEC/cqvec_president_state.rds"
# cq2loc <- "C:/GoogleDrive/Data/CQ_VEC/cqvec_president_county.rds"

## National Level Data
cq0 <- readRDS(cq0loc)
cq000 <- cq0[which(cq0$year %in% c(2000)),]
cq004 <- cq0[which(cq0$year %in% c(2004)),]
cq008 <- cq0[which(cq0$year %in% c(2008)),]
cq012 <- cq0[which(cq0$year %in% c(2012)),]

# cq1 <- readRDS(cq1loc)
# cq100 <- cq1[which(cq1$year %in% c(2000)),]
# cq104 <- cq1[which(cq1$year %in% c(2004)),]
# cq108 <- cq1[which(cq1$year %in% c(2008)),]
# cq112 <- cq1[which(cq1$year %in% c(2012)),]
# 
# cq2 <- readRDS(cq2loc)
# cq2$Area <- str_squish(cq2$Area)
# cq2$allregion <- paste(tolower(cq2$State), tolower(cq2$Area))
# cq200 <- cq2[which(cq2$year %in% c(2000)),]
# cq204 <- cq2[which(cq2$year %in% c(2004)),]
# cq208 <- cq2[which(cq2$year %in% c(2008)),]
# cq212 <- cq2[which(cq2$year %in% c(2012)),]

## Theme Setting

bgcolor <- rgb(245, 245, 245,maxColorValue = 255)

themehere <- theme_classic() + 
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) + 
  theme(panel.background = element_rect(fill=bgcolor, colour=bgcolor),
        plot.background = element_rect(fill=bgcolor, colour=bgcolor),
        legend.background = element_rect(fill=bgcolor, colour=bgcolor),
        legend.position = "bottom")
  
## Nation Level Map ###########################################################

#usa <- map_data("usa") 
usa <- urbnmapr::states
usa$pvi08 <- 
  (cq000$RepVotesMajorPercentAll + 
  cq004$RepVotesMajorPercentAll)/2 - 50
usa$pvi16 <- 
  (cq008$RepVotesMajorPercentAll + 
     cq012$RepVotesMajorPercentAll)/2 -50
usa$pvi08[1]
usa$pvi16[1]

us08 <- 
  ggplot() + geom_polygon(data = usa, 
                        aes(x=long, y = lat, group = group), 
                        fill="red", color=NULL) + 
  coord_fixed(1.3) + themehere
us08 

us16 <- 
  ggplot() + geom_polygon(data = usa, 
                          aes(x=long, y = lat, group = group), 
                          fill="blue") + 
  coord_fixed(1.3) + themehere
us16 

## State Level Map ###########################################################

#states <- map_data("state")
states <- urbnmapr::states
states$region <- tolower(states$state_name)
head(states)

# Merging PVI Data with Mapping Data
pvi08p_s <- data.frame(region = unique(pvi08p$region),
                       pvi = unique(pvi08p$statePVI))
pvi16p_s <- data.frame(region = unique(pvi16p$region),
                       pvi = unique(pvi16p$statePVI))
states$pvi08p <- pvi08p_s$pvi[match(states$region, pvi08p_s$region)]
states$pvi16p <- pvi16p_s$pvi[match(states$region, pvi16p_s$region)]

## Mapping

genthls <- function(dt) {
  thls <- list()
  # 0% Threshold in 0-1 Scale
  thls$th080 <- (scales::rescale(sort(dt$pvi08p))[which(sort(dt$pvi08p)>=0)][1] + 
                   scales::rescale(sort(dt$pvi08p, decreasing=TRUE))[which(sort(dt$pvi08p, decreasing=TRUE)<=0)][1])/2
  thls$th160 <- (scales::rescale(sort(dt$pvi16p))[which(sort(dt$pvi16p)>=0)][1] + 
                   scales::rescale(sort(dt$pvi16p, decreasing=TRUE))[which(sort(dt$pvi16p, decreasing=TRUE)<=0)][1])/2
  # 5% Threshold in 0-1 Scale 
  thls$th08h5 <- scales::rescale(sort(dt$pvi08p))[which(sort(dt$pvi08p)>=5)][1]
  thls$th08l5 <- scales::rescale(sort(dt$pvi08p, decreasing=TRUE))[which(sort(dt$pvi08p, decreasing=TRUE)<=-5)][1]
  thls$th16h5 <- scales::rescale(sort(dt$pvi16p))[which(sort(dt$pvi16p)>=5)][1]
  thls$th16l5 <- scales::rescale(sort(dt$pvi16p, decreasing=TRUE))[which(sort(dt$pvi16p, decreasing=TRUE)<=-5)][1]
  # 10% Threshold in 0-1 Scale 
  thls$th08h10 <- scales::rescale(sort(dt$pvi08p))[which(sort(dt$pvi08p)>=10)][1]
  thls$th08l10 <- scales::rescale(sort(dt$pvi08p, decreasing=TRUE))[which(sort(dt$pvi08p, decreasing=TRUE)<=-10)][1]
  thls$th16h10 <- scales::rescale(sort(dt$pvi16p))[which(sort(dt$pvi16p)>=10)][1]
  thls$th16l10 <- scales::rescale(sort(dt$pvi16p, decreasing=TRUE))[which(sort(dt$pvi16p, decreasing=TRUE)<=-10)][1]
  # 15% Threshold in 0-1 Scale 
  thls$th08h15 <- scales::rescale(sort(dt$pvi08p))[which(sort(dt$pvi08p)>=15)][1]
  thls$th08l15 <- scales::rescale(sort(dt$pvi08p, decreasing=TRUE))[which(sort(dt$pvi08p, decreasing=TRUE)<=-15)][1]
  thls$th16h15 <- scales::rescale(sort(dt$pvi16p))[which(sort(dt$pvi16p)>=15)][1]
  thls$th16l15 <- scales::rescale(sort(dt$pvi16p, decreasing=TRUE))[which(sort(dt$pvi16p, decreasing=TRUE)<=-15)][1]
  return(thls)
}

thls_s <- genthls(states)

# Plot 2008
state08 <- 
  ggplot(data = states) + 
  geom_polygon(aes_string(x = "long", y = "lat", fill = "pvi08p", group = "group"), 
               color = bgcolor) +
  scale_fill_gradientn(name="", colors=c("blue4","blue3","blue2","blue",
                                         "white",
                                         "red","red2","red3","red4"),
                       values = c(0,thls_s$th08l15,thls_s$th08l10,thls_s$th08l5,
                                  thls_s$th080,
                                  thls_s$th08h5,thls_s$th08h10,thls_s$th08h15,1)) + 
  coord_fixed(1.3) + themehere
state08

ggsave("./papers/pictures/state08.pdf", width=15, height=10.05)

# Plot 2016
state16 <- 
  ggplot(data = states) + 
  geom_polygon(aes_string(x = "long", y = "lat", fill = "pvi16p", group = "group"), 
               color = bgcolor) +
  scale_fill_gradientn(name="", colors=c("blue4","blue3","blue2","blue",
                                         "white",
                                         "red","red2","red3","red4"),
                       values = c(0,thls_s$th16l15,thls_s$th16l10,thls_s$th16l5,
                                  thls_s$th160,
                                  thls_s$th16h5,thls_s$th16h10,thls_s$th16h15,1)) + 
  coord_fixed(1.3) + themehere
state16

ggsave("./papers/pictures/state16.pdf", width=15, height=10.05)

## County Level Map ########################################################

#counties <- map_data("county")
counties <- urbnmapr::counties
counties$region <- tolower(counties$state_name)
counties$subregion <- tolower(counties$county_name)

# Adjust Names
counties$subregion <- sub(" county$","",counties$subregion)
counties$subregion <- sub(" (city and |)borough$","",counties$subregion)
counties$subregion <- sub(" census area$","",counties$subregion)
counties$subregion <- sub(" municipality$","",counties$subregion)
counties$subregion <- sub(" parish$","",counties$subregion)
counties$subregion[counties$region=="virginia"] <- 
  sub(" city$","",counties$subregion[counties$region=="virginia"])
counties$subregion[counties$region=="virginia"] <- 
  sub("^charles$","charles city",counties$subregion[counties$region=="virginia"])
counties$subregion[counties$region=="virginia"] <- 
  sub("^james$","james city",counties$subregion[counties$region=="virginia"])
counties$subregion <- sub("'s$", "s", counties$subregion)
counties$subregion <- sub("lasalle", "la salle", counties$subregion)
counties$subregion <- sub("laporte", "la porte", counties$subregion)
counties$subregion <- sub("lamoure", "la moure", counties$subregion)
#counties$subregion <- gsub("lakota", "la kota", counties$subregion)
counties$subregion <- sub("dewitt", "de witt", counties$subregion)
counties$subregion <- sub("dupage", "du page", counties$subregion)
counties$subregion[counties$region=="mississippi"] <- 
  sub("desoto","de soto",counties$subregion[counties$region=="mississippi"])
# Change in Name
counties$subregion <- sub("kusilvak", "wade hampton", counties$subregion)
counties$subregion <- sub("oglala lakota", "shannon", counties$subregion)
# Often included in different county
counties$subregion <- sub("kalawao", "maui", counties$subregion)
# Deprecated
# counties$subregion <- gsub("ste ", "ste. ", counties$subregion)
# counties$subregion <- gsub("st ", "st. ", counties$subregion)
# counties$subregion <- gsub("east. ", "east ", counties$subregion)
# counties$subregion <- gsub("west. ", "west ", counties$subregion)
# counties$subregion <- gsub("yellowstone national", "yellowstone", counties$subregion)
# counties$subregion <- gsub("obrien", "o\'brien", counties$subregion)
# counties$subregion <- gsub("de kalb", "dekalb", counties$subregion)
# counties$subregion[counties$region=="florida"] <- 
#   gsub("de soto", "desoto", counties$subregion[counties$region=="florida"])

# Combine State and Region
counties$allregion <- paste(counties$region, counties$subregion)

# Check Match
table(unique(counties$allregion) %in% unique(pvi08p$allregion))
table(unique(counties$allregion) %in% unique(pvi16p$allregion))
# Check Unmatched Locations
unique(counties$allregion[!counties$allregion %in% pvi08p$allregion])
unique(counties$allregion[!counties$allregion %in% pvi16p$allregion])
# District of Columbia has no Subregions in map data
unique(pvi08p$allregion[grep("district of columbia", pvi08p$allregion)])
unique(counties$allregion[grep("district of columbia", counties$allregion)])

# Merging PVI Data with Mapping Data
counties$pvi08p <- pvi08p$countyPVI[match(counties$allregion, pvi08p$allregion)]
counties$pvi08p[grep("district of columbia", counties$allregion)] <- 0
counties$pvi16p <- pvi16p$countyPVI[match(counties$allregion, pvi16p$allregion)]
counties$pvi16p[grep("district of columbia", counties$allregion)] <- 0
summary(counties$pvi08p)
summary(counties$pvi16p)

## Mapping

# county thresholds
thls <- genthls(counties)

# Plot 2008
county08 <- 
  ggplot(data = counties) + 
  geom_polygon(aes_string(x = "long", y = "lat", fill = "pvi08p", group = "group"), 
               color = bgcolor) +
  scale_fill_gradientn(name="", colors=c("blue4","blue3","blue2","blue",
                                         "white",
                                         "red","red2","red3","red4"),
                       values = c(0,thls$th08l15,thls$th08l10,thls$th08l5,
                                  thls$th080,
                                  thls$th08h5,thls$th08h10,thls$th08h15,1)) + 
  coord_fixed(1.3) + themehere
county08

ggsave("./papers/pictures/county08.pdf", width=15, height=10.05)

# Plot 2016
county16 <- 
  ggplot(data = counties) + 
  geom_polygon(aes_string(x = "long", y = "lat", fill = "pvi16p", group = "group"), 
               color = bgcolor) +
  scale_fill_gradientn(name="", colors=c("blue4","blue3","blue2","blue",
                                         "white",
                                         "red","red2","red3","red4"),
                       values = c(0,thls$th16l15,thls$th16l10,thls$th16l5,
                                  thls$th160,
                                  thls$th16h5,thls$th16h10,thls$th16h15,1)) + 
  coord_fixed(1.3) + themehere
county16

ggsave("./papers/pictures/county16.pdf", width=15, height=10.05)

## california thresholds
thls_ca <- genthls(counties[counties$region=="california",])

## california 08
county08_ca <- 
  ggplot(data = counties[counties$region=="california",]) + 
  geom_polygon(aes_string(x = "long", y = "lat", fill = "pvi08p", group = "group"), 
               color = bgcolor) +
  scale_fill_gradientn(name="", colors=c("blue4","blue3","blue2","blue",
                                         "white",
                                         "red","red2","red3","red4"),
                       values = c(0,thls_ca$th08l15,thls_ca$th08l10,thls_ca$th08l5,
                                  thls_ca$th080,
                                  thls_ca$th08h5,thls_ca$th08h10,thls_ca$th08h15,1)) + 
  coord_fixed(1.3) + themehere
county08_ca

ggsave("./papers/pictures/county08_ca.pdf", width=6, height=7.9)

## california 16
county16_ca <- 
  ggplot(data = counties[counties$region=="california",]) + 
  geom_polygon(aes_string(x = "long", y = "lat", fill = "pvi16p", group = "group"), 
               color = bgcolor) +
  scale_fill_gradientn(name="", colors=c("blue4","blue3","blue2","blue",
                                         "white",
                                         "red","red2","red3","red4"),
                       values = c(0,thls_ca$th16l15,thls_ca$th16l10,thls_ca$th16l5,
                                  thls_ca$th160,
                                  thls_ca$th16h5,thls_ca$th16h10,thls_ca$th16h15,1)) + 
  coord_fixed(1.3) + themehere
county16_ca

ggsave("./papers/pictures/county16_ca.pdf", width=6, height=7.9)

## texas thresholds
thls_tx <- genthls(counties[counties$region=="texas",])

## texas 08
county08_tx <- 
  ggplot(data = counties[counties$region=="texas",]) + 
  geom_polygon(aes_string(x = "long", y = "lat", fill = "pvi08p", group = "group"), 
               color = bgcolor) +
  scale_fill_gradientn(name="", colors=c("blue4","blue3","blue2","blue",
                                         "white",
                                         "red","red2","red3","red4"),
                       values = c(0,thls_tx$th08l15,thls_tx$th08l10,thls_tx$th08l5,
                                  thls_tx$th080,
                                  thls_tx$th08h5,thls_tx$th08h10,thls_tx$th08h15,1)) + 
  coord_fixed(1.3) + themehere
county08_tx

ggsave("./papers/pictures/county08_tx.pdf", width=6, height=7.07)

## texas 16
county16_tx <- 
  ggplot(data = counties[counties$region=="texas",]) + 
  geom_polygon(aes_string(x = "long", y = "lat", fill = "pvi16p", group = "group"), 
               color = bgcolor) +
  scale_fill_gradientn(name="", colors=c("blue4","blue3","blue2","blue",
                                         "white",
                                         "red","red2","red3","red4"),
                       values = c(0,thls_tx$th16l15,thls_tx$th16l10,thls_tx$th16l5,
                                  thls_tx$th160,
                                  thls_tx$th16h5,thls_tx$th16h10,thls_tx$th16h15,1)) + 
  coord_fixed(1.3) + themehere
county16_tx

ggsave("./papers/pictures/county16_tx.pdf", width=6, height=7.07)

## pennsylvania thresholds
thls_pa <- genthls(counties[counties$region=="pennsylvania",])

## pennsylvania 08
county08_pa <- 
  ggplot(data = counties[counties$region=="pennsylvania",]) + 
  geom_polygon(aes_string(x = "long", y = "lat", fill = "pvi08p", group = "group"), 
               color = bgcolor) +
  scale_fill_gradientn(name="", colors=c("blue4","blue3","blue2","blue",
                                         "white",
                                         "red","red2","red3","red4"),
                       values = c(0,thls_pa$th08l15,thls_pa$th08l10,thls_pa$th08l5,
                                  thls_pa$th080,
                                  thls_pa$th08h5,thls_pa$th08h10,thls_pa$th08h15,1)) + 
  coord_fixed(1.3) + themehere
county08_pa

ggsave("./papers/pictures/county08_pa.pdf", width=8, height=5.4)

## pennsylvania 16
county16_pa <- 
  ggplot(data = counties[counties$region=="pennsylvania",]) + 
  geom_polygon(aes_string(x = "long", y = "lat", fill = "pvi16p", group = "group"), 
               color = bgcolor) +
  scale_fill_gradientn(name="", colors=c("blue4","blue3","blue2","blue",
                                         "white",
                                         "red","red2","red3","red4"),
                       values = c(0,thls_pa$th16l15,thls_pa$th16l10,thls_pa$th16l5,
                                  thls_pa$th160,
                                  thls_pa$th16h5,thls_pa$th16h10,thls_pa$th16h15,1)) + 
  coord_fixed(1.3) + themehere
county16_pa

ggsave("./papers/pictures/county16_pa.pdf", width=8, height=5.4)

## florida thresholds
thls_fl <- genthls(counties[counties$region=="florida",])

## florida 08
county08_fl <- 
  ggplot(data = counties[counties$region=="florida",]) + 
  geom_polygon(aes_string(x = "long", y = "lat", fill = "pvi08p", group = "group"), 
               color = bgcolor) +
  scale_fill_gradientn(name="", colors=c("blue4","blue3","blue2","blue",
                                         "white",
                                         "red","red2","red3","red4"),
                       values = c(0,thls_fl$th08l15,thls_fl$th08l10,thls_fl$th08l5,
                                  thls_fl$th080,
                                  thls_fl$th08h5,thls_fl$th08h10,thls_fl$th08h15,1)) + 
  coord_fixed(1.3) + themehere
county08_fl

ggsave("./papers/pictures/county08_fl.pdf", width=6, height=7.4)

## florida 16
county16_fl <- 
  ggplot(data = counties[counties$region=="florida",]) + 
  geom_polygon(aes_string(x = "long", y = "lat", fill = "pvi16p", group = "group"), 
               color = bgcolor) +
  scale_fill_gradientn(name="", colors=c("blue4","blue3","blue2","blue",
                                         "white",
                                         "red","red2","red3","red4"),
                       values = c(0,thls_fl$th16l15,thls_fl$th16l10,thls_fl$th16l5,
                                  thls_fl$th160,
                                  thls_fl$th16h5,thls_fl$th16h10,thls_fl$th16h15,1)) + 
  coord_fixed(1.3) + themehere
county16_fl

ggsave("./papers/pictures/county16_fl.pdf", width=6, height=7.4)

## ohio thresholds
thls_oh <- genthls(counties[counties$region=="ohio",])

## ohio 08
county08_oh <- 
  ggplot(data = counties[counties$region=="ohio",]) + 
  geom_polygon(aes_string(x = "long", y = "lat", fill = "pvi08p", group = "group"), 
               color = bgcolor) +
  scale_fill_gradientn(name="", colors=c("blue4","blue3","blue2","blue",
                                         "white",
                                         "red","red2","red3","red4"),
                       values = c(0,thls_oh$th08l15,thls_oh$th08l10,thls_oh$th08l5,
                                  thls_oh$th080,
                                  thls_oh$th08h5,thls_oh$th08h10,thls_oh$th08h15,1)) + 
  coord_fixed(1.3) + themehere
county08_oh

ggsave("./papers/pictures/county08_oh.pdf", width=6, height=7.2)

## ohio 16
county16_oh <- 
  ggplot(data = counties[counties$region=="ohio",]) + 
  geom_polygon(aes_string(x = "long", y = "lat", fill = "pvi16p", group = "group"), 
               color = bgcolor) +
  scale_fill_gradientn(name="", colors=c("blue4","blue3","blue2","blue",
                                         "white",
                                         "red","red2","red3","red4"),
                       values = c(0,thls_oh$th16l15,thls_oh$th16l10,thls_oh$th16l5,
                                  thls_oh$th160,
                                  thls_oh$th16h5,thls_oh$th16h10,thls_oh$th16h15,1)) + 
  coord_fixed(1.3) + themehere
county16_oh

ggsave("./papers/pictures/county16_oh.pdf", width=6, height=7.2)

###############
## Save Data ##
###############

save(states,
     counties,
     #us08,us16,
     thls_s,state08,state16,
     thls,county08,county16,
     thls_ca,county08_ca,county16_ca,
     thls_tx,county08_tx,county16_tx,
     thls_pa,county08_pa,county16_pa,
     thls_fl,county08_fl,county16_fl,
     thls_oh,county08_oh,county16_oh,
     file="./outputs/pvi_mapping.RData")