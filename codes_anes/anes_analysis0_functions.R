require(lme4)
library(estvis)
library(pbapply)

fbase <- formula(presvote ~ infoph)

fcxt <- formula( ~ . + infoph*pvi_nation + infoph*repinc2)

fpref <- formula( ~ .+ 
                infoph*iderepadv +
                infoph*pid +
                infoph*evecon)
fpref2 <- formula( ~ .+ 
                    infoph*iderepadvsq +
                    infoph*pid +
                    infoph*evecon)

fdem <- formula( ~ . + 
                female*infoph + # Female
                age*infoph + # Age
                agesq*infoph + # Age squared
                educ*infoph + # Education
                inc*infoph + # Income
                # black*infoph + # Black
                minority*infoph + # Minority (NOn-White)
                prot*infoph + # Protestant
                cath*infoph) # Catholic

fdemextra <- formula(~ . +
                       married*infoph + # Married
                       own*infoph + # Homeowner
                       hmake*infoph + # Housewife
                       ret*infoph + # Retired
                       cler*infoph + # Clerical
                       prof*infoph + # Professional
                       union*infoph + # Union household
                       urban*infoph + # Urban
                       east*infoph + # East
                       south*infoph + # South
                       west*infoph) # West

## Level Variables
flv <- formula(~. + (1|year) + (1|state) + (1|state:year))

## Only Preference and Demography
f1 <- update(update(fbase,fpref),fdem)
f1sq <- update(update(fbase,fpref2),fdem)
f2 <- update(update(update(fbase,fpref),fdem),fdemextra)
f2sq <- update(update(update(fbase,fpref2),fdem),fdemextra)

# Multi Level
f1x <- update(update(update(update(fbase,fcxt),fpref),fdem),flv)

## Standard Logit
func_logit <- function(d,mod){
  
  if (class(d)[1]%in%c("mi","list")) {
    est <- lapply(d, function(k) glm(mod,data=k,weights=w,family=stats::binomial("logit")))
  } else {
    est<-glm(mod,data=d,weights=w,family=stats::binomial("logit"))
  }
  return(est)
}

## Multi-level Model
func_logitml <- function(d,mod,weighted){
  
  if (class(d)[1]=="mi") {
    est <- pblapply(d, function(k) glmer(mod,data=k,weights=d$w,nAGQ=0,family=stats::binomial("logit")))
    # class(est) <- c("merModList", class(est))
  } else {
    if (weighted==TRUE) {
      est <- glmer(mod,data=d,weights=w,nAGQ=0,family=stats::binomial("logit"))
    } else {
      est <- glmer(mod,data=d,nAGQ=0,family=stats::binomial("logit"))
    }
  }
  return(est)
  
}

# Export Data for Graphing
gd_export <- function(ml,d, type=1) {
  tmpfunc1 <- function(k,m,d) {
    a <- simu_interact(m, k, moveprof = data.frame(infoph=c(0,1)))$predsum
    a$infoph <- factor(c("Uninformed","Informed"),levels=c("Uninformed","Informed"))
    a$name <- k
    return(a)
  }
  tmpfunc1x <- function(k,m,d) {
    a <- interplot(m, k, "infoph", steps=2, plot=FALSE)
    a <- a[,c(1,2,4,3)]
    colnames(a)[2:4] <- c("Mean","lowerCI","upperCI")
    a$infoph <- factor(c("Uninformed","Informed"),levels=c("Uninformed","Informed"))
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
    a <- list(gd0=a[a$infoph=="Uninformed",],
              gd1=a[a$infoph=="Informed",])
    rownames(a[[1]]) <- a[[1]]$name
    rownames(a[[2]]) <- a[[2]]$name
    return(a)
  }
  
  if (class(ml)[1]=="list") {
    if (class(d)[1]!="list") stop("d argument must be list!")
    a <- pblapply(seq(1,length(ml),1), function(k) tmpfunc2(m=ml[[k]], d=d[[k]], type=type))
    # gd0 <- data.frame(infoph = a[[1]]$gd0$infoph)
    # gd0$Mean <- rowMeans(sapply(a, function(k) k$gd0$Mean))
    # gd0$upperCI <- rowMeans(sapply(a, function(k) k$gd0$upperCI))
    # gd0$lowerCI <- rowMeans(sapply(a, function(k) k$gd0$lowerCI))
    # rownames(gd0) <- gd0$name <- a[[1]]$gd0$name
    # gd1 <- data.frame(infoph = a[[1]]$gd1$infoph)
    # gd1$Mean <- rowMeans(sapply(a, function(k) k$gd1$Mean))
    # gd1$upperCI <- rowMeans(sapply(a, function(k) k$gd1$upperCI))
    # gd1$lowerCI <- rowMeans(sapply(a, function(k) k$gd1$lowerCI))
    # rownames(gd1) <- gd1$name <- a[[1]]$gd1$name
    # a <- list(gd0=gd0, gd1=gd1)
  } else {
    a <- tmpfunc2(ml, type=type)
  }
  
  return(a)
  
}


## Prediction for Logit Model
predeach <- function(m,simud0,simud1, ...) {
  # uninformed
  temp0 <- simu_pred(m, profile=simud0, ...)$predsum
  temp0 <- cbind(temp0,simud0$w)
  # informed
  temp1 <- simu_pred(m, profile=simud1, ...)$predsum
  temp1 <- cbind(temp1,simud1$w)
  # difference
  tempdif <- data.frame(Mean=temp0$Mean-temp1$Mean,
                        Median=temp0$Median-temp1$Median,
                        SE=temp0$SE-temp1$SE,
                        lowerCI=temp0$lowerCI-temp1$lowerCI,
                        upperCI=temp0$upperCI-temp1$upperCI,
                        w=simud0$w)
  # Weighted Average
  temp0 <- rowSums(apply(temp0, 1, function(k) k[1:5]*k[6]))/sum(simud0$w)
  temp1 <- rowSums(apply(temp1, 1, function(k) k[1:5]*k[6]))/sum(simud1$w)
  tempdif <- rowSums(apply(tempdif, 1, function(k) k[1:5]*k[6]))/sum(simud0$w)
  temp <- as.data.frame(rbind(temp0, temp1, tempdif))
  temp$type <- c("Uninformed","Informed","Difference")
  return(temp)
}

## Run and Export Logit Results

run_logit <- function(d,f,pyears,model_name,projdir,idex=3) {
  
  res <- list()
  
  # Prepare Data
  dlist <- lapply(pyears, function(k) as.data.frame(d[d$year==k,]))
  # Run Logit
  res$m <- m1 <- func_logit(dlist, f)
  # Observations
  tmp <- sapply(m1, function(k) nobs(k))
  names(tmp) <- pyears
  res$nobs <- tmp
  # Log Likelihood
  tmp <- sapply(m1, function(k) logLik(k))
  names(tmp) <- pyears
  res$logLik <- tmp
  # AIC 
  tmp <- sapply(m1, function(k) AIC(k))
  names(tmp) <- pyears
  res$AIC <- tmp
  # BIC 
  tmp <- sapply(m1, function(k) BIC(k))
  names(tmp) <- pyears
  res$BIC <- tmp
  
  #'
  #' ## Coefficient Plot for Individual Preference variables
  #'
  
  # Export Polotting data
  res$gd <- gd <- gd_export(m1,dlist)
  gd0_iderepadv <- do.call("rbind",lapply(gd, function(k) k$gd0[1,]))
  gd0_pid <- do.call("rbind",lapply(gd, function(k) k$gd0[2,]))
  gd0_evecon <- do.call("rbind",lapply(gd, function(k) k$gd0[3,]))
  rownames(gd0_iderepadv) <- rownames(gd0_pid) <- rownames(gd0_evecon) <- pyears
  gd1_iderepadv <- do.call("rbind",lapply(gd, function(k) k$gd1[1,]))
  gd1_pid <- do.call("rbind",lapply(gd, function(k) k$gd1[2,]))
  gd1_evecon <- do.call("rbind",lapply(gd, function(k) k$gd1[3,]))
  rownames(gd1_iderepadv) <- rownames(gd1_pid) <- rownames(gd1_evecon) <- pyears
  
  # Plot
  idename <- "Ideological Advantage"
  if (idex>1) idename <- paste0("Ideological Advantage \n(By ", idex,")")
  oddsbreaks <- c(0.1,0.5,1,2,5,10)
  oddslabels <- c(".1",".5","1","2","5","10")
  p <- plot_coef(list(gd0_iderepadv[,c(1,4,5)]*idex,gd0_pid[,c(1,4,5)],gd0_evecon[,c(1,4,5)],
                      gd1_iderepadv[,c(1,4,5)]*idex,gd1_pid[,c(1,4,5)],gd1_evecon[,c(1,4,5)]),
                 overlap.names = rep(c("Uninformed","Informed"),each=3),
                 facet.names = rep(c(idename,
                                     "Partisanship",
                                     "Retrospective\n Economic Evaluation"),2),
                 facet.x.scale = "free",
                 odds = TRUE,
                 overlap.shape.index = c(16,17)) + 
    theme(plot.caption = element_text(hjust=0)) + 
    scale_y_log10(breaks=oddsbreaks, labels=oddslabels)
  
  foottxt <- paste("Note: Conditional coefficients at knowledge values of 0 (uninformed) and 1 (informed) are simulated by quasi-Bayesian Monte \nCarlo method based on Normal approximation. In each year, model is independently estimated with Logistic regression. \nDemographic controls are omitted from the figure (see Appendix).")
  p <- plot_footnote(p, foottxt, caption=TRUE)
  
  res$p <- p
  
  ggsave(paste(projdir, "/outputs/",model_name,"_anescoefplot.png", sep=""),
         p, w=6.5, h=5)

  # Other Demographic Data
  gd0_female <- do.call("rbind",lapply(gd, function(k) k$gd0[4,]))
  gd0_age <- do.call("rbind",lapply(gd, function(k) k$gd0[5,]))
  gd0_agesq <- do.call("rbind",lapply(gd, function(k) k$gd0[6,]))
  gd0_educ <- do.call("rbind",lapply(gd, function(k) k$gd0[7,]))
  gd0_inc <- do.call("rbind",lapply(gd, function(k) k$gd0[8,]))
  gd0_minority <- do.call("rbind",lapply(gd, function(k) k$gd0[9,]))
  gd0_prot <- do.call("rbind",lapply(gd, function(k) k$gd0[10,]))
  gd0_cath <- do.call("rbind",lapply(gd, function(k) k$gd0[11,]))
  #gd0_jew <- do.call("rbind",lapply(gd, function(k) k$gd0[12,]))
  rownames(gd0_female) <- rownames(gd0_age) <- rownames(gd0_agesq) <- 
    rownames(gd0_educ) <- rownames(gd0_inc) <- rownames(gd0_minority) <- 
    rownames(gd0_prot) <- rownames(gd0_cath) <- pyears #rownames(gd0_jew) <- pyears
  gd1_female <- do.call("rbind",lapply(gd, function(k) k$gd1[4,]))
  gd1_age <- do.call("rbind",lapply(gd, function(k) k$gd1[5,]))
  gd1_agesq <- do.call("rbind",lapply(gd, function(k) k$gd1[6,]))
  gd1_educ <- do.call("rbind",lapply(gd, function(k) k$gd1[7,]))
  gd1_inc <- do.call("rbind",lapply(gd, function(k) k$gd1[8,]))
  gd1_minority <- do.call("rbind",lapply(gd, function(k) k$gd1[9,]))
  gd1_prot <- do.call("rbind",lapply(gd, function(k) k$gd1[10,]))
  gd1_cath <- do.call("rbind",lapply(gd, function(k) k$gd1[11,]))
  #gd1_jew <- do.call("rbind",lapply(gd, function(k) k$gd1[12,]))
  rownames(gd1_female) <- rownames(gd1_age) <- rownames(gd1_agesq) <- 
    rownames(gd1_educ) <- rownames(gd1_inc) <- rownames(gd1_minority) <- 
    rownames(gd1_prot) <- rownames(gd1_cath) <- pyears #rownames(gd1_jew) <- pyears
  
  p <- plot_coef(list(gd0_female[,c(1,4,5)],gd0_age[,c(1,4,5)],gd0_agesq[,c(1,4,5)],
                      gd1_female[,c(1,4,5)],gd1_age[,c(1,4,5)],gd1_agesq[,c(1,4,5)]),
                 overlap.names = rep(c("Uninformed","Informed"),each=3),
                 facet.names = rep(c("Gender (Female)",
                                     "Age",
                                     "Age Squared"),2),
                 facet.x.scale = "free",
                 odds = TRUE,
                 overlap.shape.index = c(16,17)) + 
    theme(plot.caption = element_text(hjust=0)) + 
    scale_y_log10(breaks=oddsbreaks, labels=oddslabels)
  
  foottxt <- paste("Note: Conditional coefficients at knowledge values of 0 (uninformed) and 1 (informed) are simulated by quasi-Bayesian Monte \nCarlo method based on Normal approximation. In each year, model is independently estimated with Logistic regression.")
  p <- plot_footnote(p, foottxt, caption=TRUE)
  
  res$p_dem1 <- p
  
  ggsave(paste(projdir, "/outputs/",model_name,"_anescoefplot_dem1.png", sep=""),
         p, w=6.5, h=5)
  
  p <- plot_coef(list(gd0_educ[,c(1,4,5)],gd0_inc[,c(1,4,5)],gd0_minority[,c(1,4,5)],
                      gd1_educ[,c(1,4,5)],gd1_inc[,c(1,4,5)],gd1_minority[,c(1,4,5)]),
                 overlap.names = rep(c("Uninformed","Informed"),each=3),
                 facet.names = rep(c("Education",
                                     "Income",
                                     "Ethnic Minority\n(Non-White)"),2),
                 facet.x.scale = "free",
                 odds = TRUE,
                 overlap.shape.index = c(16,17)) + 
    theme(plot.caption = element_text(hjust=0)) + 
    scale_y_log10(breaks=oddsbreaks, labels=oddslabels)
  
  foottxt <- paste("Note: Conditional coefficients at knowledge values of 0 (uninformed) and 1 (informed) are simulated by quasi-Bayesian Monte \nCarlo method based on Normal approximation. In each year, model is independently estimated with Logistic regression.")
  p <- plot_footnote(p, foottxt, caption=TRUE)
  
  res$p_dem2 <- p
  
  ggsave(paste(projdir, "/outputs/",model_name,"_anescoefplot_dem2.png", sep=""),
         p, w=6.5, h=5)
  
  p <- plot_coef(list(gd0_prot[,c(1,4,5)],gd0_cath[,c(1,4,5)], #gd0_jew[,c(1,4,5)],
                      gd1_prot[,c(1,4,5)],gd1_cath[,c(1,4,5)]), #gd0_jew[,c(1,4,5)]),
                 overlap.names = rep(c("Uninformed","Informed"),each=2),
                 facet.names = rep(c("Protestant",
                                     "Catholic"#,
                                     #"Jew"
                                     ),2),
                 facet.x.scale = "free",
                 odds = TRUE,
                 overlap.shape.index = c(16,17)) + 
    theme(plot.caption = element_text(hjust=0)) + 
    scale_y_log10(breaks=oddsbreaks, labels=oddslabels)

  foottxt <- paste("Note: Conditional coefficients at knowledge values of 0 (uninformed) and 1 (informed) are simulated by quasi-Bayesian Monte \nCarlo method based on Normal approximation. In each year, model is independently estimated with Logistic regression.")
  p <- plot_footnote(p, foottxt, caption=TRUE)
  
  res$p_dem3 <- p
  
  ggsave(paste(projdir, "/outputs/",model_name,"_anescoefplot_dem3.png", sep=""),
         p, w=6.5, h=5)
  
  res$model_name <- model_name
  res$pyears <- pyears
  
  return(res)
  
}

# Make Predictions & EDV Regression

run_pred <- function(d,m,baseyr,type="response") {
  
  res <- list()
  res$baseyr <- baseyr
  res$model_name <- m$model_name
  
  m1 <- m$m
  pyears <- m$pyears

  # Prediction Data
  prepd <- d[complete.cases(d[,all.vars(m1[[7]]$formula)]),]
  prepd <- prepd[prepd$year==baseyr,]
  simud0 <- prepd
  simud0$infoph <- 0
  res$simud0 <- simud0
  simud1 <- prepd
  simud1$infoph <- 1
  res$simud1 <- simud1
  
  # Make Prediction
  predres <- pblapply(m1, predeach, simud0=simud0, simud1=simud1, type=type)
  predres <- do.call("rbind", predres)
  predres$year <- rep(pyears, each=3)
  predres$pvi_nation <- rep(sapply(pyears, function(k) d[d$year==k,]$pvi_nation[1]),each=3)
  predres$repinc <- rep(sapply(pyears, function(k) d[d$year==k,]$repinc[1]),each=3)
  predres$deminc <- rep(sapply(pyears, function(k) d[d$year==k,]$deminc[1]),each=3)
  predres$repinc2 <- rep(sapply(pyears, function(k) d[d$year==k,]$repinc2[1]),each=3)
  predres$`Incumbent Party` <- factor(ifelse(predres$repinc2==1,"Republican","Democrat"),
                                      levels=c("Republican","Democrat"))
  predres <- predres[predres$type!="Difference",]
  predres$type <- factor(predres$type, levels=c("Uninformed","Informed"))
  predres$typename <- factor(as.numeric(predres$type), levels=c("1","2"),
                             labels=c("As If Uninformed","As If Informed"))
  res$predres <- predres
  
  # Subset Prediction Data
  dp0 <- predres[predres$type=="Uninformed",]
  dp1 <- predres[predres$type=="Informed",]
  
  #'
  #' ## EDV Regression
  #' 
  m1edv0 <- edvreg(Mean ~ pvi_nation+repinc2, omegasq=dp0$SE^2, data=dp0) # + dp0$year
  summary(m1edv0)
  m1edv1 <- edvreg(Mean ~ pvi_nation+repinc2, omegasq=dp1$SE^2, data=dp1) # + dp0$year
  summary(m1edv1)
  res$edv0 <- m1edv0
  res$edv1 <- m1edv1
  
  # Prediction from EDV Regression
  m1edv0pred <- predict(m1edv0, se.fit=TRUE)
  m1edv0pred <- data.frame(est = c(m1edv0pred$fit),
                           lCI = c(m1edv0pred$fit - 1.96*m1edv0pred$se.fit),
                           uCI = c(m1edv0pred$fit + 1.96*m1edv0pred$se.fit),
                           dp0)
  res$edv0_pred <- m1edv0pred
  m1edv1pred <- predict(m1edv1, se.fit=TRUE)
  m1edv1pred <- data.frame(est = c(m1edv1pred$fit),
                           lCI = c(m1edv1pred$fit - 1.96*m1edv1pred$se.fit),
                           uCI = c(m1edv1pred$fit + 1.96*m1edv1pred$se.fit),
                           dp1)
  res$edv1_pred <- m1edv1pred

  return(res)
  
}


export_predplot <- function(res,
                            projdir,
                            model_name="inherit",
                            ylim="auto",
                            xlim=c(44,58),
                            xbreaks=c(45,50,55),
                            ylab="response") {
  
  if (model_name=="inherit") model_name <- res$model_name
  if (ylim=="auto") ylim=NULL
  ylabset <- "Average Predicted Probability\n of Republican Vote"
  if (ylab=="asis") ylabset <- "Average Predicted Logit\n of Republican Vote"
  
  res$predres$typename <- factor(gsub("As If ","", res$predres$typename),
                                 levels=c("Uninformed","Informed"))
  res$edv0_pred$typename <- factor(gsub("As If ","", res$edv0_pred$typename),
                                   levels=c("Uninformed","Informed"))
  res$edv1_pred$typename <- factor(gsub("As If ","", res$edv1_pred$typename),
                                   levels=c("Uninformed","Informed"))
  
  p <- ggplot(data=res$predres, 
              aes_string(x="pvi_nation",y="Mean")) + 
    geom_point(aes_string(shape="`Incumbent Party`",
                          color="`Incumbent Party`")) + 
    # geom_point(aes_string(shape=`Incumbent Party`,
    #                color=`Incumbent Party`)) + 
    geom_text_repel(aes_string(label="year"), vjust=1.5) + 
    geom_ribbon(data=res$edv0_pred[res$edv0_pred$repinc2==1,],
                aes_string(x="pvi_nation",ymin="lCI",ymax="uCI"),
                fill="tomato", alpha=0.4) + 
    geom_ribbon(data=res$edv0_pred[res$edv0_pred$repinc2==0,],
                aes_string(x="pvi_nation",ymin="lCI",ymax="uCI"),
                fill="steelblue", alpha=0.4) + 
    geom_line(data=res$edv1_pred[res$edv1_pred$repinc2==1,],
              aes_string(x="pvi_nation",y="est"), color="tomato") + 
    geom_line(data=res$edv1_pred[res$edv1_pred$repinc2==0,],
              aes_string(x="pvi_nation",y="est"), color="steelblue") + 
    geom_ribbon(data=res$edv1_pred[res$edv1_pred$repinc2==1,],
                aes_string(x="pvi_nation",ymin="lCI",ymax="uCI"),
                fill="tomato", alpha=0.4) + 
    geom_ribbon(data=res$edv1_pred[res$edv1_pred$repinc2==0,],
                aes_string(x="pvi_nation",ymin="lCI",ymax="uCI"),
                fill="steelblue", alpha=0.4) + 
    geom_line(data=res$edv0_pred[res$edv0_pred$repinc2==1,],
              aes_string(x="pvi_nation",y="est"), color="tomato") + 
    geom_line(data=res$edv0_pred[res$edv0_pred$repinc2==0,],
              aes_string(x="pvi_nation",y="est"), color="steelblue") + 
    scale_y_continuous(limits=ylim) +
    scale_x_continuous(limits=xlim, breaks=xbreaks) +
    scale_color_manual(name="Incumbent Party", values=c("red","blue")) +
    facet_grid(. ~ typename) + 
    ylab(ylabset) + 
    xlab("National PVI (%)") + 
    estvis.theme() + 
    theme(legend.position="bottom",
          legend.title = element_text(),
          strip.text.x=element_text(size=12, face="bold"),
          plot.caption = element_text(hjust=0))
  foottxt <- paste("Note: Point estimates are generated by weighted average of predicted probabilities based on", res$baseyr, "ANES respondents \nprofile with imputed knowledge of 0 (uninformed) or 1 (informed). Fitted lines with 95% confidence interval are estimated \nby estimated Dependent Variable (EDV) model (Lewis and Linzer 2005) to incorporate uncertainty in point estimates.")
  p <- plot_footnote(p, foottxt, caption=TRUE)
  
  res$p <- p
  
  ggsave(paste(projdir, "/outputs/",model_name,"_anespredplot.png", sep=""),
         p, w=6.5, h=5)
  
  return(res)
  
}

## Extract EDV results from Prediction Results

extEDV <- function(m) {
  
  # Extract Coefficient Table
  t0 <- as.data.frame(summary(m$edv0)$coefficients)
  t1 <- as.data.frame(summary(m$edv1)$coefficients)
  tab <- rbind(t0,t1)
  colnames(tab) <- c("est","se","tval","p")
  # Calculate Confidence Interval
  tab$lCI <- tab$est - qnorm(0.975)*tab$se
  tab$uCI <- tab$est + qnorm(0.975)*tab$se
  # Make pvi variable in 10% scale
  tab$est[c(2,5)] <- tab$est[c(2,5)]*10
  tab$se[c(2,5)] <- tab$se[c(2,5)]*10
  tab$lCI[c(2,5)] <- tab$lCI[c(2,5)]*10
  tab$uCI[c(2,5)] <- tab$uCI[c(2,5)]*10
  # Other Variables
  tab$type <- rep(c("uninformed","informed"),each=3)
  tab$year <- as.numeric(gsub("^.*_","",deparse(substitute(m))))
  tab$coef <- rep(c("constant","pvi","inc"),2)
  return(tab)
  
}

m <- m1sq_pred_1992
