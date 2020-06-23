library(rio)
library(plyr)
library(tidyverse)
library(exact2x2)

library(devtools)
#devtools::source_url("https://raw.githubusercontent.com/ManyLabsOpenScience/manylabRs/master/R/C-3PR_ASCII%202.R")
source('~/Documents/GitHub/manylabRs/manylabRs/R/manylabRs_SOURCE.R')

options(digits = 22)

dir.in     <- "/Volumes/Samsung_T3/Manylabs2_031018/TestOutput/RESULTS_RDS"

# Create info files ----
dfKey <- get.GoogleSheet(data = "ML2masteRkey")$df
dfGlobal <- import("~/OSFdata/!!AggregatedData/ALL_study_global_include_all.xlsx")
dfKeyOri <- dfKey[dfKey$ori.study.figure2.include==1&!is.na(dfKey$unique.id),]

dfKeyGLori <- left_join(dfKeyOri, dfGlobal, by=c("study.analysis"= ".id"))
export(dfKeyGLori,"OriginalEffectSizeInfo.xlsx")

dfKeyGlob <- dfKey[dfKey$study.figure2.include==1,]
dfKeyGlobal <- left_join(dfKeyGlob, dfGlobal, by=c("study.analysis"= ".id"))
export(dfKeyGlobal,"GlobalEffectSizeInfo.xlsx")

# Create output file ----
dfKeyGLori  <- import("OriginalEffectSizeInfo.xlsx")
dfKeyGlobal <- import("GlobalEffectSizeInfo.xlsx")

testdata <- data.frame(study         = dfKeyGlobal$study.description,
                       analysis      = dfKeyGlobal$analysis.name,
                       ori_ML2analysis  = NA,
                       ori_stattype  = NA,
                       ori_N         = dfKeyGlobal$orig.stat.N,
                       ori_N_weird   = NA,
                       ori_n1_weird  = dfKeyGlobal$orig.stat.n1,
                       ori_n2_weird  = dfKeyGlobal$orig.stat.n2,
                       ori_N_lessweird = NA,
                       ML2_test      = dfKeyGlobal$test.method,
                       ML2_stattype  = dfKeyGlobal$test.type,
                       ML2_estimate  = dfKeyGlobal$test.estimate,
                       ML2_estimate1 = dfKeyGlobal$test.estimate1,
                       ML2_estimate2 = dfKeyGlobal$test.estimate2,
                       ML2_teststat  = dfKeyGlobal$test.statistic,
                       ML2_N         = dfKeyGlobal$stat.N,
                       ML2_n1         = dfKeyGlobal$stat.n1,
                       ML2_n2         = dfKeyGlobal$stat.n2,
                       ML2_df        = dfKeyGlobal$test.parameter,
                       ML2_pval      = dfKeyGlobal$test.p.value,
                       ML2_sd.1      = dfKeyGlobal$stat.cond1.sd,
                       ML2_sd.2      = dfKeyGlobal$stat.cond2.sd,
                       ML2_d         = dfKeyGlobal$ESCI.d,
                       ML2_r         = dfKeyGlobal$ESCI.r,
                       ML2_OR        = dfKeyGlobal$ESCI.OR,
                       ML2_Q         = dfKeyGlobal$ESCI.cohensQ,
                       N_ML2_pval    = NA,
                       N_ori_pval    = NA,
                       N_2.5ori_pval = NA,
                       N_50each_pval = NA, stringsAsFactors = FALSE)

# Add original effect info
idOri <- llply(dfKeyGlobal$study.id.x, function(id) dfKeyGLori$study.id.x%in%id)
testdata$ori_stattype <- laply(idOri,function(id) dfKeyGLori$orig.stat.type[id&dfKeyGLori$ori.sample.weird==1])
testdata$ori_N_weird  <- laply(idOri,function(id) dfKeyGLori$orig.stat.N[id&dfKeyGLori$ori.sample.weird==1])
testdata$ori_ML2analysis  <- laply(idOri,function(id) dfKeyGLori$study.analysis[id&dfKeyGLori$ori.sample.weird==1])
testdata$ori_N[is.na(testdata$ori_N)] <- testdata$ori_N_weird[is.na(testdata$ori_N)]

testdata$ori_n2_weird[is.na(testdata$ori_n1_weird)&is.na(testdata$ori_n2_weird)] <- ceiling(testdata$ori_N[is.na(testdata$ori_n1_weird)&is.na(testdata$ori_n2_weird)]/2)
testdata$ori_n1_weird[is.na(testdata$ori_n1_weird)] <- floor(testdata$ori_N[is.na(testdata$ori_n1_weird)]/2)


# Change some analysis-specific numbers
testdata$ori_N[testdata$analysis%in%"Norenzayan.1"] <- NA
testdata$ori_N_lessweird[is.na(testdata$ori_N)] <- dfKeyGLori$orig.stat.N[(dfKeyGLori$study.id.x%in%dfKeyGlobal$study.id.x)&(dfKeyGLori$ori.sample.weird==0)]

testdata$ML2_test[testdata$analysis%in%"Miyamoto.1"] <- "GLM t-test"
testdata$ML2_df[testdata$analysis%in%"Miyamoto.1"] <- 7194

testdata$ML2_test[testdata$analysis%in%"Savani.3a"]  <- "GLMM Wald Z-test"
testdata$ML2_stattype[testdata$analysis%in%"Savani.3a"]  <- "Z"
testdata$ML2_estimate[testdata$analysis%in%"Savani.3a"]  <- dfKeyGlobal$test.statistic[dfKeyGlobal$study.analysis%in%"Savani.3a"]
testdata$ML2_stattype[testdata$analysis%in%"Savani.3a"]  <- "Z"

dfKeyGlobal$test.estimate1[dfKeyGlobal$study.analysis%in%"Inbar.1a"]
testdata$ML2_estimate1[testdata$analysis%in%"Inbar.1a"] <- 0.12445156
testdata$ML2_estimate2[testdata$analysis%in%"Inbar.1a"] <- 0.07103915

testdata$ML2_estimate1[testdata$analysis%in%"Graham.1"] <- 0.1447813
testdata$ML2_estimate1[testdata$analysis%in%"vanLange.1"] <- -0.01687882

testdata$ML2_estimate1[testdata$analysis%in%"Schwarz.1a"] <- 0.3772108
testdata$ML2_estimate2[testdata$analysis%in%"Schwarz.1a"] <- 0.4357757

# Calculate new p-values ----
tind  <- grepl("t",testdata$ML2_stattype)&!grepl("Welch", testdata$ML2_test)
Wind  <- grepl("Welch", testdata$ML2_test)
Zind  <- grepl("Z.f",testdata$ML2_stattype)
ZindQ <- grepl("Z.f",testdata$ML2_stattype)&grepl("Inbar|Schwarz",testdata$analysis)
ORind <- which(grepl("OR",testdata$ML2_stattype))

# R&M functions ----

# For d based on two-groups
# "d" is the value of Cohen's d for the replication
# n.1 and n.2 are sample sizes for group 1 and 2, respectively
get.sig.d <- function(d, n.1, n.2)
{
  tval <- d/sqrt(1/n.1+1/n.2)
  df <- n.1+n.2-2
  pval <- 1-2*abs(pt(tval,df)-.5)
  return(data.frame(tval = tval, pval = pval))
}

# For d based on one-group
# "d" is the value of Cohen's d for the replication
# n.1 and n.2 are sample sizes for group 1 and 2, respectively
get.sig.d1 <- function(d, n)
{
  tval <- d/sqrt(n)
  df <- n-1
  pval <- 1-2*abs(pt(tval,df)-.5)
  return(data.frame(tval = tval, pval = pval))
}

# For Welch t-test
# "d" is the value of Cohen's d for the replication
# n.1 and n.2 are sample sizes for group 1 and 2, respectively
# sd.1 and sd.2 are standard deviations for group 1 and 2, respectively
get.sig.welch <- function(d, n.1, n.2, sd.1, sd.2)
{
  tval <- d/sqrt(1/n.1+1/n.2)
  df.1 <- n.1-1
  df.2 <- n.2-1
  df <- (sd.1^2/n.1+sd.2^2/n.2)^2/(sd.1^4/(n.1^2*df.1)+sd.2^4/(n.2^2*df.2))
  pval <- 1-2*abs(pt(tval,df)-.5)
  return(data.frame(tval = tval, pval = pval))
}

# For r based on continuous variables
# "r" is the value of r in the replication
# n is the sample size

get.sig.r <- function(r, n)
{
  z <- 0.5*(log((1+r)/(1-r)))
  zval <- z*sqrt(n-3)
  pval <- 1-2*abs(pnorm(zval)-0.5)
  return(data.frame(zval = zval, pval = pval))
}

# Cohen's q
get.sig.2r <- function(z, n.1, n.2)
{
  zval <- z/sqrt(1/(n.1-3) + 1/(n.2-3))
  pval <- 1-2*abs(pnorm(zval)-0.5)
  return(data.frame(zval = zval, pval = pval))
}

# For z-test proportion
# "x" is the value of the observed proportion in the replication
# pi is the proportion under the null hypothesis
# n is the sample size
get.sig.z <- function(x, pi, n)
{
  se <- sqrt(pi*(1-pi)/n)
  zval <- (pi-x)/se
  #pval <- 1-2*abs(pnorm(zval)-0.5)
  # 'greater'
  if(zval<0){
    pval <- pnorm(zval)
  } else {
    pval <- 1-pnorm(zval)
  }

  return(data.frame(zval = zval, pval = pval))
}

# For or based on dichotomous variables
# "orr" is the value of or in the replication
# n11,n12,n22,n22 are the sample sizes of original study

get.sig.or <- function(or, n11, n12, n21, n22)
{
  zval <- log(or)/sqrt(1/n11 + 1/n12 + 1/n21 + 1/n22)
  pval <- 1-2*abs(pnorm(zval)-0.5)
  return(data.frame(zval = zval, pval = pval))
}

# t-tests: Use the observed t, but change the df
testdata$N_ML2_pval[tind]    <- get.sig.d(d = testdata$ML2_d[tind], n.1 = testdata$ML2_n1[tind],
                                          n.2 = testdata$ML2_n2[tind])$pval

testdata$N_ori_pval[tind]    <- get.sig.d(d = testdata$ML2_d[tind], n.1 = testdata$ori_n1_weird[tind],
                                          n.2 = testdata$ori_n2_weird[tind])$pval

testdata$N_2.5ori_pval[tind] <- get.sig.d(d = testdata$ML2_d[tind],
                                          n.1 = (((testdata$ori_n1_weird[tind]+testdata$ori_n2_weird[tind])*2.5)/(testdata$ori_n1_weird[tind]+testdata$ori_n2_weird[tind]))*testdata$ori_n1_weird[tind],
                                          n.2 = (((testdata$ori_n1_weird[tind]+testdata$ori_n2_weird[tind])*2.5)/(testdata$ori_n1_weird[tind]+testdata$ori_n2_weird[tind]))*testdata$ori_n2_weird[tind])$pval

testdata$N_50each_pval[tind] <- get.sig.d(d = testdata$ML2_d[tind], n.1 = 50, n.2 = 50)$pval

# Welch-tests
testdata$N_ML2_pval[Wind]    <- get.sig.welch(d = testdata$ML2_d[Wind], n.1 = testdata$ML2_n1[Wind],n.2 = testdata$ML2_n2[Wind],
                                              sd.1 =testdata$ML2_sd.1[Wind], sd.2 =testdata$ML2_sd.1[Wind])$pval

testdata$N_ori_pval[Wind]    <- get.sig.welch(d = testdata$ML2_d[Wind], n.1 = testdata$ori_n1_weird[Wind], n.2 = testdata$ori_n2_weird[Wind],
                                              sd.1 =testdata$ML2_sd.1[Wind], sd.2 =testdata$ML2_sd.1[Wind])$pval

testdata$N_2.5ori_pval[Wind] <- get.sig.welch(d = testdata$ML2_d[Wind], n.1 = (((testdata$ori_n1_weird[Wind]+testdata$ori_n2_weird[Wind])*2.5)/(testdata$ori_n1_weird[Wind]+testdata$ori_n2_weird[Wind]))*testdata$ori_n1_weird[Wind],
                                              n.2 = (((testdata$ori_n1_weird[Wind]+testdata$ori_n2_weird[Wind])*2.5)/(testdata$ori_n1_weird[Wind]+testdata$ori_n2_weird[Wind]))*testdata$ori_n2_weird[Wind], sd.1 =testdata$ML2_sd.1[Wind], sd.2 =testdata$ML2_sd.1[Wind])$pval

testdata$N_50each_pval[Wind] <- get.sig.welch(d = testdata$ML2_d[Wind], n.1 = 50, n.2 = 50,
                                              sd.1 =testdata$ML2_sd.1[Wind], sd.2 =testdata$ML2_sd.1[Wind])$pval


tind1 <- testdata$analysis%in%"Gati.2"
testdata$N_ML2_pval[tind1]    <- get.sig.d1(d = testdata$ML2_d[tind1], n = testdata$ML2_N[tind1])$pval
testdata$N_ori_pval[tind1]    <- get.sig.d1(d = testdata$ML2_d[tind1], n = testdata$ori_N[tind1])$pval
testdata$N_2.5ori_pval[tind1] <- get.sig.d1(d = testdata$ML2_d[tind1], n = testdata$ori_N[tind1]*2.5)$pval
testdata$N_50each_pval[tind1] <- get.sig.d1(d = testdata$ML2_d[tind1], n = 50)$pval


# 1 sample r-to-fisherZ-tests: Values are recalculated using same correlations, but different N
testdata$N_ML2_pval[Zind]    <- get.sig.r(r = testdata$ML2_r[Zind],n = testdata$ML2_N[Zind])$pval
testdata$N_ori_pval[Zind]    <- get.sig.r(r = testdata$ML2_r[Zind],n = testdata$ori_N[Zind])$pval
testdata$N_2.5ori_pval[Zind] <- get.sig.r(r = testdata$ML2_r[Zind],n = testdata$ori_N[Zind]*2.5)$pval
testdata$N_50each_pval[Zind] <- get.sig.r(r = testdata$ML2_r[Zind],n = 50)$pval

# 2 sample r-to-fisherZ-tests: Values are recalculated using same cohen's Q as z-score, but different N
testdata$N_ML2_pval[ZindQ]    <- get.sig.2r(z = testdata$ML2_Q[ZindQ],n.1 = testdata$ML2_n1[ZindQ], n.2 = testdata$ML2_n2[ZindQ])$pval
testdata$N_ori_pval[ZindQ]    <- get.sig.2r(z = testdata$ML2_Q[ZindQ],n.1 = testdata$ori_n1_weird[ZindQ], n.2 = testdata$ori_n2_weird[ZindQ])$pval
testdata$N_2.5ori_pval[ZindQ] <- get.sig.2r(z = testdata$ML2_Q[ZindQ],n.1 = (((testdata$ori_n1_weird[ZindQ]+testdata$ori_n2_weird[ZindQ])*2.5)/(testdata$ori_n1_weird[ZindQ]+testdata$ori_n2_weird[ZindQ]))*testdata$ori_n1_weird[ZindQ], n.2 = (((testdata$ori_n1_weird[ZindQ]+testdata$ori_n2_weird[ZindQ])*2.5)/(testdata$ori_n1_weird[ZindQ]+testdata$ori_n2_weird[ZindQ]))*testdata$ori_n2_weird[ZindQ])$pval
testdata$N_50each_pval[ZindQ] <- get.sig.2r(z = testdata$ML2_Q[ZindQ],n.1 = 50, n.2 = 50)$pval

# Shafir Z-test: This is a proportion Z-test
testdata$N_ML2_pval[testdata$analysis%in%"Shafir.1"]    <- get.sig.z(x=0.4657149, n=7901, pi = 0.5)$pval
testdata$N_ori_pval[testdata$analysis%in%"Shafir.1"]    <- get.sig.z(x=0.4657149, n=170, pi = 0.5)$pval
testdata$N_2.5ori_pval[testdata$analysis%in%"Shafir.1"] <- get.sig.z(x=0.4657149, n=2.5*170, pi = 0.5)$pval
testdata$N_50each_pval[testdata$analysis%in%"Shafir.1"] <- get.sig.z(x=0.4657149, n=50, pi = 0.5)$pval

# Fisher exact tests: Just multiply the table with counts by a ratio... this keeps OR about the same, inaccuracies are due to rounding to integers
ORtables <- llply(ORind,function(id){
  matrix(data = c(dfKeyGlobal$stat.cond1.count[id],
                  dfKeyGlobal$stat.cond2.count[id],
                  dfKeyGlobal$stat.cond3.count[id],
                  dfKeyGlobal$stat.cond4.count[id]), ncol=2)})

shafir.ori   <-  as.table(matrix(c(31,54,38,47),nrow = 2, ncol = 2, dimnames = list(c("parent A", "parent B"), c("Award","Deny"))))
tverski.ori  <- as.table(matrix(c(69,24,25,63),nrow = 2, ncol = 2, dimnames = list(c("Go","Don't go"),c("Cheap", "Costly"))))
savani.ori   <- as.table(matrix(c(60,45,68,45),nrow = 2, ncol = 2, dimnames = list(c("Indians", "Americans"), c("Personal","Interpersonal"))))
rottenstreich.ori <- as.table(matrix(c(6,14,13,7), nrow = 2, ncol = 2, dimnames = list(c("Kiss", "Cash"), c("Certain","Uncertain"))))
hauser1.ori <- as.table(matrix(c(1177,146,146,1177), nrow = 2, ncol = 2, dimnames = list(c("Yes","No"),c("Foreseen side eff.", "Greater good"))))
hauser4.ori <- as.table(matrix(c(731,575,940,366), nrow = 2, ncol = 2, dimnames = list(c("Yes","No"),c("Intended harm", "Foreseen harm"))))


ORoriTables <- list(rottenstreich.ori,hauser1.ori,tverski.ori,hauser4.ori)

testdata$N_ML2_pval[ORind]    <- laply(ORind, function(id){get.sig.or(or  = testdata$ML2_OR[id],
                                                                      n11 = dfKeyGlobal$stat.cond1.count[id],
                                                                      n12 = dfKeyGlobal$stat.cond3.count[id],
                                                                      n21 = dfKeyGlobal$stat.cond2.count[id],
                                                                      n22 = dfKeyGlobal$stat.cond4.count[id])$pval})

#
testdata$N_ori_pval[ORind] <- laply(1:4, function(id){get.sig.or(or  = testdata$ML2_OR[ORind[id]],
                                                                      n11 = ORoriTables[[id]][1,1],
                                                                      n12 = ORoriTables[[id]][1,2],
                                                                      n21 = ORoriTables[[id]][2,1],
                                                                      n22 = ORoriTables[[id]][2,2])$pval})

testdata$N_2.5ori_pval[ORind] <-   laply(1:4, function(id){get.sig.or(or  = testdata$ML2_OR[ORind[id]],
                                                                           n11 = ORoriTables[[id]][1,1]*2.5,
                                                                           n12 = ORoriTables[[id]][1,2]*2.5,
                                                                           n21 = ORoriTables[[id]][2,1]*2.5,
                                                                           n22 = ORoriTables[[id]][2,2]*2.5)$pval})
testdata$N_50each_pval[ORind] <-  NA #laply(seq_along(ORind), function(id){exact2x2::exact2x2(x = ORtables[[id]]*ratio$G50)$p.value})

export(testdata,"table5data_RM.csv")
export(testdata,"table5data_RM.xlsx")

testdata_comp <- import("table5data_RM.xlsx")

testdata$ML2_pval-testdata$N_ML2_pval

testdata_comp <- testdata_comp %>% select(ends_with("pval"))
RM <- testdata %>% select(ends_with("pval"))

diffs <- RM - testdata_comp
diffs$analysis <- testdata$analysis
export(diffs,"table5_pval_diffs.xlsx")


# # t-tests: Use the observed t, but change the df
# testdata$N_ML2_pval[tind]    <- 2*(1-pt(q = abs(testdata$ML2_teststat[tind]), df = testdata$ML2_df[tind]))
# testdata$N_ori_pval[tind]    <- 2*(1-pt(q = abs(testdata$ML2_teststat[tind]), df = testdata$ori_N_weird[tind]-2))
# testdata$N_2.5ML2_pval[tind] <- 2*(1-pt(q = abs(testdata$ML2_teststat[tind]), df = testdata$ML2_df[tind]*2.5))
# testdata$N_50each_pval[tind] <- 2*(1-pt(q = abs(testdata$ML2_teststat[tind]), df = 98))
#
# # r-to-fisherZ-tests: Values are recalculated using same correlations, but different N
# testdata$N_ML2_pval[Zind]    <- laply(which(Zind), function(id){cor_test_fisherZ(r1 = testdata$ML2_estimate1[id], r2 = testdata$ML2_estimate2[id], n1 = floor(testdata$ML2_N[id]/2), n2 = ceiling(testdata$ML2_N[id]/2))$p.value})
# testdata$N_ori_pval[Zind]    <- laply(which(Zind), function(id){cor_test_fisherZ(r1 = testdata$ML2_estimate1[id], r2 = testdata$ML2_estimate2[id], n1 = floor(testdata$ori_N[id]/2), n2 = ceiling(testdata$ori_N[id]/2))$p.value})
# testdata$N_2.5ML2_pval[Zind] <- laply(which(Zind), function(id){cor_test_fisherZ(r1 = testdata$ML2_estimate1[id], r2 = testdata$ML2_estimate2[id], n1 = floor((testdata$ML2_N[id]*2.5)/2), n2 = ceiling((testdata$ML2_N[id]*2.5)/2))$p.value})
# testdata$N_50each_pval[Zind] <- laply(which(Zind), function(id){cor_test_fisherZ(r1 = testdata$ML2_estimate1[id], r2 = testdata$ML2_estimate2[id], n1 = 50, n2 = 50)$p.value})
#
# # Shafir Z-test: This is a proportion Z-test
# testdata$N_ML2_pval[testdata$analysis%in%"Shafir.1"]    <- z.test(x=0.4657149, N=7901, proportion = TRUE, pi = 0.5, alternative = "one.sided")$p.value
# testdata$N_ori_pval[testdata$analysis%in%"Shafir.1"]    <- z.test(x=0.4657149, N=170, proportion = TRUE, pi = 0.5, alternative = "one.sided")$p.value
# testdata$N_2.5ML2_pval[testdata$analysis%in%"Shafir.1"] <- z.test(x=0.4657149, N=2.5*7901, proportion = TRUE, pi = 0.5, alternative = "one.sided")$p.value
# testdata$N_50each_pval[testdata$analysis%in%"Shafir.1"] <- z.test(x=0.4657149, N=100, proportion = TRUE, pi = 0.5, alternative = "one.sided")$p.value
#
# # Savani Z-test: We use the OR as effect size, but here we have to use the Wald Z-test from the GLMM output in order to evaluate different N. We'll pretend it is a regular sample based Z-score with SE = 0.02903.
#
# # df<-readRDS("~/Dropbox/Manylabs2/TestOutput/RESULTS_RDS/all/ML2_results_global_all.rds")
# # output.sav3a <- df$aggregated$Savani.3a$test.ConsoleOutput
# # cat(paste(output.sav3a))
# # Fixed effects:
# #                                Estimate Std. Error z value Pr(>|z|)
# # (Intercept)                     1.40785    0.02125  66.252  < 2e-16 ***
# #   Importance                   -0.05146    0.01050  -4.900 9.59e-07 ***
# #   ConditionPersonal            -0.33492    0.02903 -11.538  < 2e-16 ***   << Savani.3a
# #   Importance:ConditionPersonal -0.06097    0.01366  -4.464 8.04e-06 ***
#
# SE <- 0.02903
# Z  <- -11.538
# N  <- 6010
# testdata$N_ML2_pval[testdata$analysis%in%"Savani.3a"]    <- 2*(1-pnorm(abs(Z),sd = 1))
# testdata$N_ori_pval[testdata$analysis%in%"Savani.3a"]    <- 2*(1-pnorm(abs(Z),sd = SE*sqrt(90)))
# testdata$N_2.5ML2_pval[testdata$analysis%in%"Savani.3a"] <- 2*(1-pnorm(abs(Z),sd = SE*sqrt(N*2.5)))
# testdata$N_50each_pval[testdata$analysis%in%"Savani.3a"] <- 2*(1-pnorm(abs(Z),sd = SE*sqrt(100)))
#
# # Fisher exact tests: Just multiply the table with counts by a ratio... this keeps OR about the same, inaccuracies are due to rounding to integers
# ORtables <- llply(ORind,function(id){
#   matrix(data = c(dfKeyGlobal$stat.cond1.count[id],
#                   dfKeyGlobal$stat.cond2.count[id],
#                   dfKeyGlobal$stat.cond3.count[id],
#                   dfKeyGlobal$stat.cond4.count[id]), ncol=2)
#   })
#
# ratio <- list(ML2=rep(1,length(ORind)), ori=testdata$ori_N[ORind]/testdata$ML2_N[ORind],ML225=rep(2.5,length(ORind)),G50 = 100/testdata$ML2_N[ORind])
#
# testdata$N_ML2_pval[ORind]    <- laply(seq_along(ORind), function(id){exact2x2::exact2x2(x = ORtables[[id]]*ratio$ML2)$p.value})
# testdata$N_ori_pval[ORind]     <- laply(seq_along(ORind), function(id){exact2x2::exact2x2(x = ORtables[[id]]*ratio$ori)$p.value})
# testdata$N_2.5ML2_pval[ORind] <-  laply(seq_along(ORind), function(id){exact2x2::exact2x2(x = ORtables[[id]]*ratio$ML225)$p.value})
# testdata$N_50each_pval[ORind] <-  laply(seq_along(ORind), function(id){exact2x2::exact2x2(x = ORtables[[id]]*ratio$G50)$p.value})
#
# export(testdata,"table5data.csv")
# export(testdata,"table5data.xlsx")

