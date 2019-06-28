#################################################################################
## File Name: CCES_analysis0_functions.R                                       ##               ##
## Date: 21 Feb 2018                                                           ##
## Author: Gento Kato                                                          ##
## Project: Uninformed Choice                                                  ##
## Purpose: Functions to Analyze Data                                          ##
#################################################################################

library(lme4)

#################################################
## Functions and Formulas to Simplify Analyses ##
#################################################

# No Context
m_cxt0 <- formula ( ~ . + knidx)

# State Context
m_cxt1v <- formula( ~ .+
                     knidx +
                     knidx*I(pvi_state/10)
)
m_cxt1c <- formula( ~ .+
                     knidx +
                     knidx*I(pci_state/5)
)
m_hcxt1v <- formula( ~ .+
                      knidx +
                      knidx*I(hpvi_state/10)
)
m_hcxt1c <- formula( ~ .+
                      knidx +
                      knidx*I(hpci_state/5)
)
m_hcxt1v2 <- formula( ~ .+
                       knidx +
                       knidx*I(hpvi2_state/10)
)
m_hcxt1c2 <- formula( ~ .+
                       knidx +
                       knidx*I(hpci2_state/5)
)

# County Context
m_cxt2v <- formula( ~ .+
                      knidx +
                      knidx*I(pvi_state/10) +
                      knidx*I(pvi_county/10) 
                   )
m_cxt2c <- formula( ~ .+
                      knidx +
                      knidx*I(pci_state/10) +
                      knidx*I(pci_county/5) 
)

# District Context
m_hcxt3v2 <- formula( ~ .+
                        knidx +
                        knidx*I(hpvi2_state/10) +
                        knidx*I(hpvi2_district/10)
)
m_hcxt3c2 <- formula( ~ .+
                        knidx +
                        knidx*I(hpci2_state/10) +
                        knidx*I(hpci2_district/5)
)

# Ideological Distance (Rep Advantage)
m_ide_p <- formula( ~.+
                      knidx*idedist_prepadv)
# knidx*idedist_padvmr+
# knidx*idedist_padvmd)
m_ide_s <- formula( ~.+
                      knidx*idedist_srepadv)
# knidx*idedist_sadvmr +
# knidx*idedist_sadvmd)
m_ide_h <- formula( ~ . +
                      knidx*idedist_hrepadv)
# knidx*idedist_hadvmr +
# knidx*idedist_hadvmd)

# Ideological Distance (Magnitude)
m_ide_px <- formula( ~.+ knidx*idedist_padvm)
m_ide_sx <- formula( ~.+ knidx*idedist_sadvm)
m_ide_hx <- formula( ~ . + knidx*idedist_hadvm)

# m_net <- formula( ~ . + 
#                     knidx*netReppr)

# Controls (For Choice)
m_ctl <- formula( ~.+
                    knidx*I(repstr-demstr)+
                    # knidx*repstr +
                    # knidx*demstr +
                    # knidx*polint +
                    knidx*evecon +
                    # knidx*retinc +
                    knidx*female +
                    knidx*I((age-46)/10) +
                    # knidx*white +
                    knidx*black +
                    knidx*latino +
                    knidx*asian +
                    knidx*other +
                    # knidx*married +
                    # knidx*fulltime +
                    # knidx*income +
                    knidx*I(edu-2) +
                    knidx*bornagain
                  # knidx*chatd +
                  # knidx*resid
)

## Level Variables
m_lv <- formula(~.+ (1|state) + (1|county))

gd_export <- function(m.org,m,m.vcov){
  
  r0 <- c(1,seq(3,grep("knidx:",rownames(m))[1]-1,1))
  m0 <- m[r0,]
  m1 <- m[-r0,]
  m0.vcov <- m.vcov[r0,r0]
  m1.vcov <- m.vcov[-r0,-r0]
  mx.vcov <- m.vcov[r0,-r0]
  
  m1x <- m1
  rownames(m1x) <- rownames(m0)
  m1x[,1] <- m0[,1] + m1[,1]
  m1x[,2] <- sqrt(diag(m0.vcov) + (1^2)*diag(m1.vcov) + 1*2*diag(mx.vcov))
  m1x[,3] <- m1x[,1]/m1x[,2]
  dfm <- summary(m.org)$df.residual
  m1x[,4] <- round(pt(1-abs(m1x[,3]),dfm),3)/2
  
  gd0 <- data.frame(cf=m0[,1],
                    lCI = m0[,1]-1.96*m0[,2],
                    uCI = m0[,1]+1.96*m0[,2])
  rownames(gd0) <- rownames(m0)
  gd1 <- data.frame(cf=m1x[,1],
                    lCI = m1x[,1]-1.96*m1x[,2],
                    uCI = m1x[,1]+1.96*m1x[,2])
  rownames(gd1) <- rownames(m1x)
  
  gdlist <- list(gd0,gd1)
  names(gdlist) <- c("gd0","gd1")
  
  return(gdlist)
}

vn1 <- c("(Intercept)",
         "State PVI (Republican Advantage/10%)",
         #         "County PVI Squared",
         # "County PVI (Republican: by 10%)",
         # "County PVI (Democrat: by 10%)",
         "Relative Ideological Closeness (Republican: -6:6)",
         # "Relative Ideological Closeness (Republican:0-6)",
         # "Relative Ideological Closeness (Democrat:0-6)",
         # "Leaning Party (Dem:Rep, -1:1)",
         "Partisanship Strength (Dem:Rep: -3:3)",
         # "Partisanship Strength (Republican:0-3)",
         # "Partisanship Strength (Democrat:0-3)",
         "Retrospective Economic Evaluation (-2:2)",
         # "Retrospective Income Evaluation (-2:2)",
         # "Political Interest (0-3)",
         "Female",
         "Age (by 10 years)",
         # "White",
         "Black",
         "Latino",
         "Asian",
         "Other Race",
         # "Married",
         # "Work Full-Time",
         # "Income",
         "Education (0-5)",
         "Born-Again Christian"
         # "Church Attendance (0-5)",
         # "Length of Residence (1-6)"
)

vn1v <- vn1c <- vn1v2 <- vn1c2 <- vn1
vn1c[2] <- "State PCI (Republican Increase/5%)"
vn1v2[2] <- "House State PVI (Republican Increase/5%)"
vn1c2[2] <- "House State PCI (Republican Increase/5%)"

vn2v <- vn2c <- c(vn1[1:2],NA,vn1[3:length(vn1)])
vn2v[3] <- c("County PVI (Republican Advantage/10%)") 
vn2c[2] <- c("State PCI (Republican Increase/5%)")
vn2c[3] <- c("County PCI (Republican Increase/5%)")

vn2v2 <- vn2c2 <- c(vn1[1:2],NA,vn1[3:length(vn1)])
vn2v2[2] <- c("House State PVI (Republican Advantage/10%)") 
vn2c2[2] <- c("House State PCI (Republican Increase/5%)")
vn2v2[3] <- c("House District PVI (Republican Advantage/10%)") 
vn2c2[3] <- c("House District PCI (Republican Increase/5%)")

func_logit <- function(d,mod,w="wt"){
  
  # Weight Variable
  d$wtvar <- d[,w]
  
  # Dataset
  du <- d#[which(d$voted %in% c(1)),]
  dx <- du[,c("id","state","county","wtvar",all.vars(mod))]
  dx[,all.vars(mod)[1]] <- as.numeric(factor(dx[,all.vars(mod)[1]],
                                             levels=c("Democrat","Republican")))-1
  dx <- dx[complete.cases(dx),]
  print(table(dx[,all.vars(mod)[1]]))
  
  # Analysis
  est<-glm(mod,data=dx,weights=wtvar,family=binomial("logit"))
  est$dt <- dx
  
  return(est)
  
}

func_logitx <- function(d,mod,w="wt"){
  
  # Weight Variable
  d$wtvar <- d[,w]
  
  # Dataset
  du <- d#[which(d$voted %in% c(1)),]
  dx <- du[,c("id","state","county","wtvar",all.vars(mod))]
  # dx[,all.vars(mod)[1]] <- as.numeric(factor(dx[,all.vars(mod)[1]],
  #                                            levels=c("Democrat","Republican")))-1
  dx <- dx[complete.cases(dx),]
  print(table(dx[,all.vars(mod)[1]]))
  
  # Analysis
  est<-glm(mod,data=dx,weights=wtvar,family=binomial("logit"))
  est$dt <- dx
  
  return(est)
  
}

func_logitml <- function(d,mod,w="wt",...){
  
  # Weight Variable
  d$wtvar <- d[,w]
  
  # Dataset
  du <- d#[which(d$voted %in% c(1)),]
  dx <- du[,c("id","state","county","wtvar",all.vars(mod))]
  dx[,all.vars(mod)[1]] <- as.numeric(factor(dx[,all.vars(mod)[1]],
                                             levels=c("Democrat","Republican")))-1
  dx <- dx[complete.cases(dx),]
  print(table(dx[,all.vars(mod)[1]]))
  
  # Analysis
  est <- glmer(mod,data=dx,weights=wtvar,nAGQ=1,family=stats::binomial("logit"),...)
  est$dt <- dx
  
  return(est)
  
}
