#################################################################################
## File Name: CCES_analysis0_functions.R                                       ##               ##
## Date: 21 Feb 2018                                                           ##
## Author: Gento Kato                                                          ##
## Project: Uninformed Choice                                                  ##
## Purpose: Functions to Analyze Data                                          ##
#################################################################################

library(lme4)
library(merTools)
library(interplot)
library(estvis)
library(pbapply)
require(MCMCpack)
require(mcmcplots)
require(texreg)

#################################################
## Functions and Formulas to Simplify Analyses ##
#################################################


# Social Context
fpsoc <- formula(vpres ~ knidx + 
                  knidx*pvi_state + 
                  knidx*pvi_county
                )

# Individual Preference
fpind <- formula(  ~ . + 
                   knidx*idedistsq_prepadv + 
                   knidx*pid + 
                   knidx*evecon
                )

# Control Variable
fctl <- formula(  ~ . + 
                   knidx*female + 
                   knidx*age + 
                   knidx*agesq +
                   knidx*black + 
                   knidx*latino + 
                   knidx*asian + 
                   knidx*other +
                   knidx*income + 
                   knidx*edu + 
                   knidx*bornagain
                )

## Level Variables
flv <- formula(~.+ (1|state) + (1|county))

# For Standard Analysis
fp1 <- fpsoc
fp2 <- update(fpsoc, fpind)
fp3 <- update(update(fpsoc,fpind),fctl)

# For Multi-level Analysis
fp1x <- update(fp1, flv)
fp2x <- update(fp2, flv)
fp3x <- update(fp3, flv)

# Variable Names
vn3 <- c("(Intercept)",
         "State PVI (By 10%)",
         "County PVI (By 10%)",
         "Ideological Advantage",
         "Partisanship",
         "Retrospective Economic Evaluation",
         "Female",
         "Age",
         "Age Squared",
         "Black",
         "Latino",
         "Asian",
         "Other Race",
         "Income",
         "Education",
         "Born-Again Christian")
vn1 <- vn3[1:3]
vn2 <- vn3[1:6]

# Export Interacted Values (Deprecated)
# gd_export <- function(m.org,m,m.vcov){
#   
#   r0 <- c(1,seq(3,grep("knidx:",rownames(m))[1]-1,1))
#   m0 <- m[r0,]
#   m1 <- m[-r0,]
#   m0.vcov <- m.vcov[r0,r0]
#   m1.vcov <- m.vcov[-r0,-r0]
#   mx.vcov <- m.vcov[r0,-r0]
#   
#   m1x <- m1
#   rownames(m1x) <- rownames(m0)
#   m1x[,1] <- m0[,1] + m1[,1]
#   m1x[,2] <- sqrt(diag(m0.vcov) + (1^2)*diag(m1.vcov) + 1*2*diag(mx.vcov))
#   m1x[,3] <- m1x[,1]/m1x[,2]
#   dfm <- summary(m.org)$df.residual
#   m1x[,4] <- round(pt(1-abs(m1x[,3]),dfm),3)/2
#   
#   gd0 <- data.frame(cf=m0[,1],
#                     lCI = m0[,1]-1.96*m0[,2],
#                     uCI = m0[,1]+1.96*m0[,2])
#   rownames(gd0) <- rownames(m0)
#   gd1 <- data.frame(cf=m1x[,1],
#                     lCI = m1x[,1]-1.96*m1x[,2],
#                     uCI = m1x[,1]+1.96*m1x[,2])
#   rownames(gd1) <- rownames(m1x)
#   
#   gdlist <- list(gd0,gd1)
#   names(gdlist) <- c("gd0","gd1")
#   
#   return(gdlist)
# }

# Export Data for Graphing
gd_export <- function(ml,d, type=1) {
  tmpfunc1 <- function(k,m,d) {
    a <- simu_interact(m, k, moveprof = data.frame(knidx=c(0,1)),
                       vcov.est="cluster", cluster.var = cbind(d$state,d$county))$predsum
    a$knidx <- factor(c("Uninformed","Informed"),levels=c("Uninformed","Informed"))
    a$name <- k
    return(a)
  }
  tmpfunc1x <- function(k,m,d) {
    a <- interplot(m, k, "knidx", steps=2, plot=FALSE)
    a <- a[,c(1,2,4,3)]
    colnames(a)[2:4] <- c("Mean","lowerCI","upperCI")
    a$knidx <- factor(c("Uninformed","Informed"),levels=c("Uninformed","Informed"))
    a$name <- k
    return(a)
  }
  tmpfunc2 <- function(m, d, type) {
    if (type==2) {
      avs <- all.vars(m@call$formula)
      a <- lapply(avs[-c(1,2,length(avs)-1,length(avs))], tmpfunc1x, m=m, d=d) 
    } else {
      a <- lapply(all.vars(m$formula)[-c(1,2)], tmpfunc1, m=m, d=d)
    }
    a <- do.call("rbind", a)
    a <- list(gd0=a[a$knidx=="Uninformed",],
              gd1=a[a$knidx=="Informed",])
    rownames(a[[1]]) <- a[[1]]$name
    rownames(a[[2]]) <- a[[2]]$name
    return(a)
  }
  
  if (class(ml)[1]=="list") {
    a <- pblapply(ml, tmpfunc2, d=d, type=type)
    gd0 <- data.frame(knidx = a[[1]]$gd0$knidx)
    gd0$Mean <- rowMeans(sapply(a, function(k) k$gd0$Mean))
    gd0$upperCI <- rowMeans(sapply(a, function(k) k$gd0$upperCI))
    gd0$lowerCI <- rowMeans(sapply(a, function(k) k$gd0$lowerCI))
    rownames(gd0) <- gd0$name <- a[[1]]$gd0$name
    gd1 <- data.frame(knidx = a[[1]]$gd1$knidx)
    gd1$Mean <- rowMeans(sapply(a, function(k) k$gd1$Mean))
    gd1$upperCI <- rowMeans(sapply(a, function(k) k$gd1$upperCI))
    gd1$lowerCI <- rowMeans(sapply(a, function(k) k$gd1$lowerCI))
    rownames(gd1) <- gd1$name <- a[[1]]$gd1$name
    a <- list(gd0=gd0, gd1=gd1)
  } else {
    a <- tmpfunc2(ml, type=type)
  }
  
  return(a)
  
}


gof_export <- function(ml) {
  
  require(stringr)
  
  if ("list"%in%class(ml)) {
    a <- sapply(ml, extract_gofchr)
    a <- rowMeans(sapply(str_split(gsub("AIC:|Log Likelihood:|N:","",a), "; "),as.numeric))
    a <- paste(paste0(c("AIC:","Log Likelihood:","N:"),a), collapse="; ")
  } else {
    a <- extract_gofchr(ml)
  }
  return(a)
}

func_logit <- function(d,mod){
  
  if (class(d)[1]=="mi") {
    est <- lapply(d, function(k) glm(mod,data=k,weights=w,family=stats::binomial("logit")))
  } else {
    est<-glm(mod,data=d,weights=w,family=stats::binomial("logit"))
  }
  return(est)
}


func_logitml <- function(d,mod){
  
  if (class(d)[1]=="mi") {
    est <- pblapply(d, function(k) glmer(mod,data=k,weights=d$w,nAGQ=0,family=stats::binomial("logit")))
    # class(est) <- c("merModList", class(est))
  } else {
    est <- glmer(mod,data=d,weights=d$w,nAGQ=0,family=stats::binomial("logit"))
  }
  return(est)
  
}

# Generating Simulation Data
gensimudata <- function(m,d,sq,cq,yr) {
  # Simulating Values
  simval <- data.frame(pvi=c(rep(seq(sq[1],sq[2],length=10),2),
                             rep(seq(cq[1],cq[2],length=10),2)),
                       context=factor(rep(c("State PVI","County PVI"), each=20),
                                      levels=c("State PVI","County PVI")),
                       knidx = rep(rep(c(0,1), each=10),2) )
  simeach <- function(k,m,d) {
    # Simulation Dataset
    dtemp <- d
    geo <- k["context"]
    if (geo=="State PVI") dtemp$pvi_state <- as.numeric(k["pvi"])
    if (geo=="County PVI") dtemp$pvi_county <- as.numeric(k["pvi"]) 
    dtemp$knidx <- as.numeric(k["knidx"])
    # Simulation
    simres <- simu_pred(m, dtemp, vcov.est="cluster", cluster.var = cbind(d$state,d$county))
    temp <- cbind(simres$predsum,dtemp$w)
    # Weighted Average
    rowSums(apply(temp, 1, function(k) k[1:5]*k[6]))/sum(dtemp$w)
  }
  # Combine Simulations
  dres <- as.data.frame(t(pbapply(simval, 1, simeach, m=m, d=d)))
  dres <- data.frame(dres, simval)
  dres$year <- yr
  return(dres)
}

extract.modlist <- function(model, ...) {
  
  tmp <- list()
  for (k in 1:length(model)) tmp[[k]] <- texreg::extract(model[[k]], ...)
  
  if (length(tmp[[1]]@coef.names)>0) {
    # Check if all models have same variables
    check <- all(sapply(tmp, function(k) all(tmp[[1]]@coef.names==k@coef.names)))
    if (check==FALSE) stop("Variables differed across listed model. CANNOT average.")
  } else { stop("No coefficient names.")}
  
  if (length(tmp[[1]]@coef)>0) {
    tmp[[1]]@coef <- rowMeans(sapply(tmp, function(k) k@coef))
  } else { stop("No coefficient.")}
  if (length(tmp[[1]]@gof)>0) {
    tmp[[1]]@gof <- rowMeans(sapply(tmp, function(k) k@gof))
  } else { stop("No gof statistics.")}
  
  if (length(tmp[[1]]@se)>0) tmp[[1]]@se <- rowMeans(sapply(tmp, function(k) k@se))
  if (length(tmp[[1]]@pvalues)>0) tmp[[1]]@pvalues <- rowMeans(sapply(tmp, function(k) k@pvalues))
  if (length(tmp[[1]]@ci.low)>0) tmp[[1]]@ci.low <- rowMeans(sapply(tmp, function(k) k@ci.low))
  if (length(tmp[[1]]@ci.up)>0) tmp[[1]]@ci.up <- rowMeans(sapply(tmp, function(k) k@ci.up))
  
  return(tmp[[1]])
}

setMethod("extract", signature = className("modlist","estvis"), 
          definition = extract.modlist)

robusttab <- function(ml,d) {
  eachtab <- function(m,d) {
    as.data.frame(coeftest(m, vcov.=cluster.vcov(m, cbind(d$state,d$county)))[,])
  }
  tabs <- lapply(ml, eachtab, d=d)
  tabfin <- data.frame(`est` = rowMeans(sapply(tabs, function(k) k$"Estimate")),
                       `se` = rowMeans(sapply(tabs, function(k) k$"Std. Error")),
                       `z` = rowMeans(sapply(tabs, function(k) k$"z value")),
                       `p` = rowMeans(sapply(tabs, function(k) k$"Pr(>|z|)")))
  rownames(tabfin) <- rownames(tabs[[1]])
  tabfin
}

trainIRT <- function(yrdt) {
  
  # Prepare Data for IRT
  kncol <- grep("objkn_",names(yrdt))
  cat("\n")
  cat("\n")
  cat(paste("N of Knowledge Variables:", length(kncol)))
  
  select_row <- which(complete.cases(yrdt[,kncol]))
  knowdtx <- yrdt[select_row,kncol]
  objkn_add <- (rowSums(knowdtx) - mean(rowSums(knowdtx)))/sd(rowSums(knowdtx))
  rownames(knowdtx)[which.max(objkn_add)] <- "MAX_ROW"
  rownames(knowdtx)[which.min(objkn_add)] <- "MIN_ROW"
  
  start <- Sys.time(); start
  knowx.param.samples1 <- 
    MCMCirt1d(knowdtx, theta.constraints=list(MAX_ROW="+",MIN_ROW="-"), burnin = 25000,
              mcmc = 25000, thin=5, verbose = 500, seed = 123, theta.start = NA,
              alpha.start = 1, beta.start = 1, t0 = 0, T0 = 1, ab0=0, AB0=.5,
              store.item = FALSE, store.ability = TRUE,drop.constant.items=TRUE)
  knowx.param.samples2 <- 
    MCMCirt1d(knowdtx, theta.constraints=list(MAX_ROW="+",MIN_ROW="-"), burnin = 25000,
              mcmc = 25000, thin=5, verbose = 500, seed = 124, theta.start = NA,
              alpha.start = 1, beta.start = 1, t0 = 0, T0 = 1, ab0=0, AB0=.5,
              store.item = FALSE, store.ability = TRUE,drop.constant.items=TRUE)
  knowx.param.samples <- mcmc.list(knowx.param.samples1,knowx.param.samples2)
  end <- Sys.time()
  knowx.IRTtime <- end-start
  
  cat("\n")
  cat("\n")
  print(knowx.IRTtime)
  
  ## Factual Test Measure
  knowx.ability.summary <- summary(knowx.param.samples)
  
  ## Mean Tendency
  meanKnowx <- knowx.ability.summary$statistics[,1]
  #plot(density(meanKnowx))
  
  ## Check the Relationship with simple additive knowledge Variable
  cat("\n")
  cat("\n")
  cat(paste("Correlation with Additive Index:", cor(meanKnowx,objkn_add)))
  #plot(meanKnowx,objkn_add)
  
  ## Check Convergence ##
  #plot(knowx.param.samples[,1:206])
  #autocorr.plot(knowx.param.samples[,1:206])
  #geweke.diag(knowx.param.samples[,1:206])
  #gelman.plot(knowx.param.samples[,1:206])
  knowx.geweke <- geweke.diag(knowx.param.samples)
  #plot(density(knowx.geweke[[1]]$z))
  
  # Back to Data
  yrdt$objkn_irt <- NA
  yrdt$objkn_irt[select_row] <- meanKnowx
  yrdt$objkn_add <- NA
  yrdt$objkn_add[select_row] <- objkn_add
  
  # Exporting List
  out <- list(yrdt[,c("objkn_irt","objkn_add")], knowx.geweke)
  names(out) <- c("vars","geweke")
  
  # Store Data in the Result list
  return(out)
}

