
#DESCRIPTIVES AND ANALYSIS

#############################################################################

rm(list=ls())

#setwd(path) in path.R
source(".path.R")

#install and load packages
#source("packages.R")
source("library.R")

load("datfinal.RDA")

#Descriptives
#############################################################################
#############################################################################
#need to check analysis part of script for consistency (esp variable names)

#distribution of time to failure by state

dist <- subset(datfin, event==1)
dist$yeargroup <- as.factor(dist$firstyear)
levels(dist$state)
hist(dist$time)

ggplot(
  dist, 
  aes(x = dist$time, y = dist$state, fill=dist$state)
) +
  geom_density_ridges_gradient(
    aes(fill = ..x..), scale = 2.5, size = 0.3
  ) +
  scale_fill_gradientn(
    colours = c("#0D0887FF", "#CC4678FF", "#F0F921FF"),
    name = "Duration"
  )+
  theme_ridges() +
  labs(title = 'Density of Time to Event', x = "Time", y = "States")


ggplot(dist, aes(x = time, y = state, fill = state)) + 
  geom_density_ridges(scale = 2.5) + theme_minimal() +
  scale_fill_cyclical(values = c("blue", "green")) +
  labs(title = 'Density of Time to Event', x = "Time", y = "States")

rm(dist)


# Draftmans Plot

pairplotcont <- datfin[, c("hhinc", "maxedu", "time")]
pairplotdisc <- datfin[, c("gender", "event", "married", "region")]

pairs(pairplot, main = "Survival Data", pch = 21, bg = c("red", "blue")[unclass(datfin$region)])

ggpairs(pairplot, lower = list(continuous = "points", combo = "box", discrete="facetbar"))

ggpairs(pairplotdisc, lower = list(discrete="box"))

ggpairs(pairplotcont, lower = list(continuous = "smooth_loess"), diag = list(continuous = "density"))

rm(pairplotcont, pairplotdisc)

# Visualization of missing values in wide dataset #
# In case of income, a lot of zero values not defined as NA
datfin$hhinc[datfin$hhinc <= 0] <- NA

#few missings in final data set => no surprise b/c of generated variables


sapply(datfin, function(x) sum(is.na(x)))

aggr_plot <- aggr(datfin, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE,
                  labels=names(datfin), cex.axis=.7, gap=3, 
                  ylab=c("Histogram of missing data","Pattern"))
rm(aggr_plot)

gg_miss_fct(x = datfin, fct = firstyear)

#View(datfin)


##################################################################################
#################### ANALYSIS ####################################################
##################################################################################

# KM by region ####################################################################

wide.fit <- survfit(Surv(time, event, type="right") ~ region, data=datfin)

kmcurve <- ggsurvplot(wide.fit, conf.int=T,
                      legend.labs=c("West", "East"), legend.title="Region",  
                      title="Kaplan-Meier Curve for Survival in Rent", 
                      censor=F,
                      palette = "strata",
                      risk.table = T,
                      pval=TRUE,
                      risk.table.height=.25,
                      ylim=c(0,1),
                      xlim=c(0,30),
                      surv.median.line="hv",
                      linetype=c(1,1),
                      size = 0.5)
print(kmcurve)

rm(kmcurve, wide.fit)


# KM by migback ####################################################################

wide.fit <- survfit(Surv(time, event, type="right") ~ migback, data=datfin)
kmcurve <- ggsurvplot(wide.fit, conf.int=T,
                      legend.labs=c("No", "Yes"), legend.title="Migration Background",  
                      title="Kaplan-Meier Curve for Survival in Rent", 
                      censor=F,
                      palette = "strata",
                      risk.table = T,
                      pval=TRUE,
                      risk.table.height=.25,
                      ylim=c(0,1),
                      xlim=c(0,30),
                      surv.median.line="hv",
                      linetype=c(1,1),
                      size = 0.5)
print(kmcurve)

rm(kmcurve, wide.fit)


# KM by highinc/lowinc ###########################################################

medinc <- median(as.numeric(datfin$hhinc), na.rm=TRUE)
datfin <- mutate(datfin, highinc = ifelse(datfin$hhinc > medinc, 1, 0))
summary(datfin$highinc)
#define survival object and fit KM estimator
inc.fit <- survfit(Surv(time, event, type="right") ~ highinc, data=datfin)
km.inc <- ggsurvplot(inc.fit, conf.int=T,
                     legend.labs=c("Low", "High"), legend.title="HH Inc.",  
                     title="Kaplan-Meier Curve for Survival in Rent", 
                     censor=F,
                     palette = "strata",
                     risk.table = T,
                     pval=TRUE,
                     risk.table.height=.25,
                     ylim=c(0,1),
                     xlim=c(0,30),
                     surv.median.line="hv",
                     linetype=c(1,1),
                     size = 0.5)
print(km.inc)

rm(km.inc, inc.fit, medinc)

# KM by higedu/lowedu ###########################################################

mededu <- median(datfin$maxedu, na.rm=TRUE)
datfin <- mutate(datfin, highedu = ifelse(datfin$maxedu > mededu, 1, 0))
summary(datfin$highedu)
#define survival object and fit KM estimator
edu.fit <- survfit(Surv(time, event, type="right") ~ highedu, data=datfin)
km.edu <- ggsurvplot(edu.fit, conf.int=T,
                     legend.labs=c("Low", "High"), legend.title="Years Educ.",  
                     title="Kaplan-Meier Curve for Survival in Rent", 
                     censor=F,
                     palette = "strata",
                     risk.table = T,
                     pval=TRUE,
                     risk.table.height=.25,
                     ylim=c(0,1),
                     xlim=c(0,30),
                     surv.median.line="hv",
                     linetype=c(1,1),
                     size = 0.5)
print(km.edu)

rm(km.edu, edu.fit, mededu)

# KM by cohorts 84-87 and 94-97 #########################################################

datfin <- mutate(datfin, cohort8494 = ifelse (datfin$firstyear<=1987, 1,ifelse(datfin$firstyear>=1994 & datfin$firstyear<=1997, 2, NA)))
summary(datfin$cohort8494)
table(datfin$cohort8494)
#define survival object and fit KM estimator
coh.fit <- survfit(Surv(time, event, type="right") ~ cohort8494, data=datfin)
km.coh <- ggsurvplot(coh.fit, conf.int=T,
                     legend.labs=c("84-87", "94-97"), legend.title="Strata",  
                     title="Kaplan-Meier Curve for Survival in Rent", 
                     censor=F,
                     palette = "strata",
                     risk.table = T,
                     pval=TRUE,
                     risk.table.height=.25,
                     ylim=c(0,1),
                     xlim=c(0,30),
                     surv.median.line="hv",
                     linetype=c(1,1),
                     size = 0.5)
print(km.coh)

rm(km.coh, coh.fit)



#cohorts 84-87 and 04-07

datfin <- mutate(datfin, cohort8404 = ifelse
                (datfin$firstyear>=1984 & datfin$firstyear<=1987, 1,
                  ifelse(datfin$firstyear>=2004 & datfin$firstyear<=2007, 2, NA)))
summary(datfin$cohort8404)
table(datfin$cohort8404)
#define survival object and fit KM estimator
coh.fit2 <- survfit(Surv(time, event, type="right") ~ cohort8404, data=datfin)
km.coh2 <- ggsurvplot(coh.fit2, conf.int=T,
                      legend.labs=c("84-87", "94-97"), legend.title="Strata",  
                      title="Kaplan-Meier Curve for Survival in Rent", 
                      censor=F,
                      palette = "strata",
                      risk.table = T,
                      pval=TRUE,
                      risk.table.height=.25,
                      ylim=c(0,1),
                      xlim=c(0,30),
                      surv.median.line="hv",
                      linetype=c(1,1),
                      size = 0.5)
print(km.coh2)

rm(km.coh2, coh.fit2)




# cox proportional hazard regression #################################################

#survival/rms to estimate models, survminer package for plots and diagnostics

#define survival object
coxsurv <- Surv(datfin$time, datfin$event, type="right")
plot(coxsurv)

#Cox PH model

#using survival package
cox.ph <- coxph(coxsurv ~ maxedu + hhinc + gender + region + married, data=datfin)

#using rms package
cox.ph2 <- cph(coxsurv ~ maxedu + hhinc + gender + region + strat(married), data=datfin,
               na.rm=FALSE, y=TRUE, x=TRUE)

print(cox.ph)
print(cox.ph2)

#Schoenfeld test of cox ph assumption
coxtest <- cox.zph(cox.ph)
coxtest
#Schoenfeld graphical test of cox ph assumption
ggcoxzph(coxtest)

#adjusted (for covariates) survival curves from cox model

#by highinc
medinc <- median(datfin$hhinc, na.rm=TRUE)
datfin <- mutate(datfin, highinc = ifelse(datfin$hhinc > medinc, 2, 1))
summary(datfin$highinc)
rm(medinc)

ggadjustedcurves(cox.ph, data = datfin, variable = "highinc")


#by firstyear

datfin$firstyear <- as.integer(datfin$firstyear)
cu <- c(1984, 1990, 2000, 2010, 2015)
datfin$yearcut <- cut(datfin$firstyear, cu, dig.lab = min(nchar(cu)))
ggadjustedcurves(cox.ph, data = datfin, variable="yearcut")
rm(cu)

rm(cox.ph, cox.ph2, coxtest, coxsurv)

######!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##########
#All of the following are simple KM curves using the long format for survival objects
#   => long format should not be used anymore, wide is now available (above) from $$datfin

load("datalong.RDA")

#create minimal dataset to try out survival functions
minimal <- data[, c("hid", "syear", "tstart", "tstop", "time", "failure",
                    "failureflag", "l11102", "firstyear")]

minimal$tstart <- minimal$tstart - 2
minimal$tstop <- minimal$tstop - 2
#define survival object in long format version (interval censored)
min.fit <- survfit(Surv(tstart, tstop, failure) ~ l11102,
                   data=minimal)
summary(min.fit)

#define survival curve object
kmcurve <- ggsurvplot(min.fit, conf.int=T,
                      legend.labs=c("West", "East"), legend.title="Region",  
                      title="Kaplan-Meier Curve for Survival in Rent", 
                      censor=F,
                      palette = "strata",
                      risk.table = T,
                      risk.table.height=.25,
                      ylim=c(0.25,1),
                      xlim=c(0,30),
                      surv.median.line="h",
                      linetype=c(1,1),
                      size = 0.5)
#control line width through geom_step(size = 2)
#print and save survival curve
print(kmcurve)
ggsave(file = "kmcurve.pdf", print(kmcurve))

summary(min.fit)



glist <- list(
  ggsurvplot(min.fit, risk.table = F, main = "Survival Probability"),
  ggsurvplot(min.fit, fun = "event",  main = "Cumulative Proportion"),
  ggsurvplot(min.fit, fun = "cumhaz", main = "Cumulative Hazard")
)
arrange_ggsurvplots(glist, print = TRUE, ncol = 3, nrow = 1)


# cohort analysis#####################################################################

#define cohorts in long format version

#cohort comprising all 25-yo-renters observed from 1984 to 1986
minimal$c8486 <- 0
minimal$c8486[minimal$firstyear<=1986] <- 1
summary(minimal$c8486)

#cohort comprising all 25-yo-renters observed from 1994 to 1996
minimal$c9496 <- 0
minimal$c9496[minimal$firstyear<=1996 & minimal$firstyear>=1994] <- 1
summary(minimal$c9496)

#create cohort grouping variable
minimal$cohort <- NA
minimal$cohort[minimal$c8486 == 1] <- 1
minimal$cohort[minimal$c9496 == 1] <- 2
table(minimal$cohort)

#define survival object and strata
min.fit.coh <- survfit(Surv(tstart, tstop, failure) ~ cohort,
                       data=minimal)
summary(min.fit.coh)


kmcurve.coh <- ggsurvplot(min.fit.coh, conf.int=T,
                          legend.labs=c("84-86", "94-96"), legend.title="Cohort",  
                          title="Kaplan-Meier Curve for Survival in Rent", 
                          censor=F,
                          palette = "strata",
                          risk.table = T,
                          risk.table.height=.25,
                          ylim=c(0.2,1),
                          xlim=c(0,30),
                          surv.median.line="h",
                          linetype=c(1,1),
                          size = 0.5)
print(kmcurve.coh)

summary(min.fit.coh)

# aggregated graphs

#generate graphs, then aggregate them

survprob <- ggsurvplot(min.fit.coh, conf.int=T,
                       legend.labs=c("84-86", "94-96"), legend.title="Cohort",  
                       title="Kaplan-Meier Survival Probability", 
                       censor=F,
                       palette = "strata",
                       risk.table = F,
                       risk.table.height=.25,
                       ylim=c(0.2,1),
                       xlim=c(0,30),
                       surv.median.line="h",
                       linetype=c(1,1),
                       size = 0.5)
#print(survprob)

cumprop <- ggsurvplot(min.fit.coh, conf.int=T,
                      legend.labs=c("84-86", "94-96"), legend.title="Cohort",  
                      title="Kaplan-Meier Cumulative Proportion", 
                      main="Cumulative Proportion",
                      censor=F,
                      fun = "event",
                      palette = "strata",
                      risk.table = F,
                      risk.table.height=.25,
                      ylim=c(0,1),
                      xlim=c(0,30),
                      surv.median.line="h",
                      linetype=c(1,1),
                      size = 0.5)
#print(cumprop)


cumulhaz <- ggsurvplot(min.fit.coh, conf.int=T,
                       legend.labs=c("84-86", "94-96"), legend.title="Cohort",  
                       title="Kaplan-Meier Cumulative Hazard", 
                       main="Cumulative Hazard",
                       censor=F,
                       fun = "cumhaz",
                       palette = "strata",
                       risk.table = F,
                       risk.table.height=.25,
                       ylim=c(0,1.5),
                       xlim=c(0,30),
                       linetype=c(1,1),
                       size = 0.5)



glist <- list(survprob, cumprop, cumulhaz)
arrange_ggsurvplots(glist, print = TRUE, ncol = 3, nrow = 1)



