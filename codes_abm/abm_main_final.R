#' ---
#' title: "Run ABM Model"
#' author: "Gento Kato"
#' date: "June 25, 2019"
#' ---
#' 
#' ## Preparation 
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
#source(paste(projdir, "codes_abm/abm_functions3.R", sep="/"))
# devtools::install_github("gentok/contextvoting")
require(contextvoting)

#'
#' ## Create Samples of Segregated Societies
#'

set.seed(5)
soc_low <- oneset(alike_threshold=0.2,
                  drawplot=TRUE,
                  run_type="segregation")

# Check Segregation by Region
soc_low$region_stat$maj <- ifelse(soc_low$region_stat$g1>=soc_low$region_stat$g2,
                                  soc_low$region_stat$g1,
                                  soc_low$region_stat$g2)
soc_low$region_stat$majpr <- soc_low$region_stat$maj/soc_low$region_stat$total
mean(soc_low$region_stat$majpr)

# Check Segregation by SubRegion
soc_low$subregion_stat$maj <- ifelse(soc_low$subregion_stat$g1>=soc_low$subregion_stat$g2,
                                  soc_low$subregion_stat$g1,
                                  soc_low$subregion_stat$g2)
soc_low$subregion_stat$majpr <- soc_low$subregion_stat$maj/soc_low$subregion_stat$total
mean(soc_low$subregion_stat$majpr)


set.seed(3)
soc_mod <- oneset(alike_threshold=0.5,
                  drawplot=TRUE,
                  run_type="segregation")

# Check Segregation by Region
soc_mod$region_stat$maj <- ifelse(soc_mod$region_stat$g1>=soc_mod$region_stat$g2,
                                  soc_mod$region_stat$g1,
                                  soc_mod$region_stat$g2)
soc_mod$region_stat$majpr <- soc_mod$region_stat$maj/soc_mod$region_stat$total
mean(soc_mod$region_stat$majpr)

# Check Segregation by Sub-Region
soc_mod$subregion_stat$maj <- ifelse(soc_mod$subregion_stat$g1>=soc_mod$subregion_stat$g2,
                                  soc_mod$subregion_stat$g1,
                                  soc_mod$subregion_stat$g2)
soc_mod$subregion_stat$majpr <- soc_mod$subregion_stat$maj/soc_mod$subregion_stat$total
mean(soc_mod$subregion_stat$majpr)

set.seed(4)
soc_high <- oneset(alike_threshold=0.7,
                  drawplot=TRUE,
                  run_type="segregation")

# Check Segregation by Region
soc_high$region_stat$maj <- ifelse(soc_high$region_stat$g1>=soc_high$region_stat$g2,
                                  soc_high$region_stat$g1,
                                  soc_high$region_stat$g2)
soc_high$region_stat$majpr <- soc_high$region_stat$maj/soc_high$region_stat$total
mean(soc_high$region_stat$majpr)

# Check Segregation by Sub-region
soc_high$subregion_stat$maj <- ifelse(soc_high$subregion_stat$g1>=soc_high$subregion_stat$g2,
                                  soc_high$subregion_stat$g1,
                                  soc_high$subregion_stat$g2)
soc_high$subregion_stat$majpr <- soc_high$subregion_stat$maj/soc_high$subregion_stat$total
mean(soc_high$subregion_stat$majpr)

#'
#' ## Test on Those Societies
#'

socx_low <- oneset(signalbias_sd = 1,
                   signalbias_mean = 0,
                   signalbias_ingroup = 0,
                   partycapacity_noise = 0.5,
                   partystartcapacity_noise = 0.5,
                   knowmean1 = 0.5,
                   knowmean2 = 0.8,
                   drawplot=TRUE,
                   run_type = "election",
                   segregated_soc = soc_low)

socx_mod <- oneset(signalbias_sd = 1,
                   signalbias_mean = 0,
                   signalbias_ingroup = 0,
                   partycapacity_noise = 0.5,
                   partystartcapacity_noise = 0.5,
                   knowmean1 = 0.5,
                   knowmean2 = 0.8,
                   drawplot=TRUE,
                   run_type = "election",
                   segregated_soc = soc_mod)

socx_high <- oneset(signalbias_sd = 1,
                    signalbias_mean = 1,
                    signalbias_ingroup = 0,
                    partycapacity_noise = 0.5,
                    partystartcapacity_noise = 0.5,
                    knowmean1 = 0.5,
                    knowmean2 = 0.8,
                    drawplot=TRUE,
                    run_type = "election",
                    segregated_soc = soc_high)

#'
#' ## Prepare Parameters
#'

require(simsalapar)

# Prepare Parameters
prepparams <- function(leneach,nsim=10) {
  varlist(
    n.sim = list(type="N", value=nsim),
    alike_threshold = list(type="grid", value=seq(0.2,0.7,length=2)), #seq(0.3,0.6,0.1) #runif(1,0.3,0.6)
    signalbias_sd = list(type="grid", value=seq(0,1,length=2)), #runif(1,0.5,2)
    signalbias_mean = list(type="grid", value=seq(0,1,length=2)), #runif(1,0.5,2)
    signalbias_ingroup = list(type="frozen", value=0), #runif(1,0,1)
    #signalbias_ingroup = list(type="grid", value=seq(0,1,length=leneach)), #runif(1,0,1)
    knowmean1 = list(type="grid", value=seq(0.45,0.65,length=leneach))
  )
}

# One Set of Simulation
onesim <- function(alike_threshold, 
                   signalbias_sd, 
                   signalbias_mean,
                   signalbias_ingroup, 
                   knowmean1) {
  
  #seedval <- sample(-2^15:2^15, 1)
  
  x = oneset(alike_threshold = alike_threshold,
         signalbias_sd = signalbias_sd,
         signalbias_mean = signalbias_mean,
         signalbias_ingroup = signalbias_ingroup,
         knowmean1 = knowmean1,
         knowmean2 = 1.3 - knowmean1,
         drawplot=FALSE, seedval = NULL)
  ttohappy <- length(x$happy_tracker)
  names(ttohappy) <- "ttohappy"
  happypr <- tail(x$happy_tracker,1)
  names(happypr) <- "happypr"
  reg_redper <- x$region_stat$g2/x$region_stat$total
  names(reg_redper) <- paste0("reg", seq(1,9,1), "_redper")
  reg_all_majper <- mean(ifelse(reg_redper>=0.5,reg_redper,1-reg_redper))
  names(reg_all_majper) <- "reg_all_majper"
  subreg_redper <- x$subregion_stat$g2/x$subregion_stat$total
  names(subreg_redper) <- paste0("subreg", seq(1,81,1), "_redper")
  subreg_all_majper <- mean(ifelse(subreg_redper>=0.5,subreg_redper,1-subreg_redper))
  names(subreg_all_majper) <- "subreg_all_majper"
  kn_blue <- mean(x$knowledge$p[x$ideology$x==-1])
  kn_red <- mean(x$knowledge$p[x$ideology$x==1])
  kn_all <- mean(x$knowledge$p)
  kn <- c(kn_blue, kn_red, kn_all)
  names(kn) <- c("kn_blue","kn_red","kn_all")
  plus1 <- unlist(x$quality$informed)
  names(plus1) <- paste0("inf_", names(plus1))
  plus2 <- unlist(x$quality$noidinformed)
  names(plus2) <- paste0("noid_", names(plus2))
  a <- c(ttohappy,happypr,reg_all_majper,reg_redper,
         subreg_all_majper,subreg_redper,kn,plus1,plus2)
  return(a)
}
# Test
onesim(0.3,0,0,0.5,0.5)

# Total Number of Simulation
neach <- 5; nsim <- 100
params <- prepparams(neach,nsim)
get.n.sim(params)*dim(mkGrid(params))[1]
set.seed(23)
(seedset <- sample(-2^15:2^15,nsim))

#'
#' ## Run Simulation
#' 

#simres <- doLapply(vList=params, doOne=onesim, drawplot=FALSE)

#+ eval = FALSE
system.time(
simres <- doForeach(vList=params, 
                    doOne=onesim,
                    seed = seedset,
                    exports=c("oneset","initialsoc","get_neighbors",
                              "collect_region","collect_subregion",
                              "happysoc","happyregion","happysubregion",
                              "moveunhappy","setideology",
                              "updateideology","setknowledge",
                              "setparty","updateparty",
                              "getsignal","getregion","getcontext",
                              "letsvote","prefsoc")
                              )
)

# Erapsed Time (nsim=100)
# getDoParWorkers(): 6
# user    system   elapsed 
# 80.388    36.246 20290.572 

# Check
# dsim <- sapply(simres[1:2500], function(k) k$value)
# dsim <- as.data.frame(t(dsim))
# dim(dsim)
# head(dsim)

#'
#' ## Save Results
#' 

saveRDS(simres, "codes_abm/processing/abm_res_final.rds")

