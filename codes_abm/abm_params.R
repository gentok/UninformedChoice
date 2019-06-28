#' ---
#' title: "Get ABM Parameters"
#' author: "Gento Kato"
#' date: "June 26, 2019"
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

pv <- readRDS("data/param_voting.rds")
ps <- readRDS("data/param_society.rds")

library(xtable)

pvx <- pv
pvx[4,] <- pvx[4,]*100 
pvx[6,] <- pvx[6,]*100 
pvx[7,] <- pvx[7,]*100 
pvx <- round(pvx,3)
rownames(pvx) <- c("Ideological Advantage",
                   "Retrospective Evaluation",
                   "Party Identity",
                   "National PVI",
                   "Incumbent (R)",
                   "State PVI","County PVI")
pvx <- xtable(pvx[,-2], digits=3, 
              caption="Voting Function Parameters",
              label = "voteparams")

print(pvx, caption.placement="top", 
      table.placement="h!!!",
      file = "outputs/abm_voteparams.tex", compress = FALSE)

psx <- cbind(colMeans(ps[,-1]),apply(ps[,-1], 2, function(k) sd(k)),
             t(apply(ps[,-1], 2, function(k) quantile(k, probs=c(0.025,0.975)))))
colnames(psx) <- c("Mean","SD","Lower 95% CI","Upper 95% CI")
rownames(psx) <- c("Retrospective Evaluation (Mean)",
                   "Rep. Ideology Perception (Mean)",
                   "Dem. Ideology Perception (Mean)",
                   "Ideology Distance from Rep. (SD)",
                   "Ideology Distance from Dem. (SD)",
                   "Self Ideology (SD)",
                   "Rep. Member Self Ideology (SD)",
                   "Dem. Member Self Ideology (SD)",
                   "Self Ideology (Mean)",
                   "Rep. Member Self Ideology (Mean)",
                   "Dem. Member Self Ideology (Mean)",
                   "Knowledge (Mean)",
                   "Knowledge (SD)")
psx <- psx[c(1,2,3,4,5,9,6,10,7,11,8,12,13),]
psx <- xtable(psx, digits=3, 
              caption="Society Parameters",
              label="socparams")
print(psx, caption.placement="top", 
      table.placement="h!!!",
      file = "outputs/abm_socparams.tex", compress = FALSE)