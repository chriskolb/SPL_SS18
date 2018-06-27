
#DESCRIPTIVES AND ANALYSIS

#############################################################################

rm(list=ls())

# setwd(path) in path.R
source(".path.R")

# install and load packages
# source("packages.R")
source("library.R")

load("datfinal.RDA")

##################################################################################
##################################################################################
#################### Descriptives ################################################
##################################################################################
##################################################################################

# 2 do:
# graphical representation of data set (see rpubs paper) (done)
# descriptives table (done)
# survival time density plot (done)
# missing data plots before and after imputation
# distributions and densities (by state plots, draftmans plot, countour/3d)

##################################################################################
# summary table ##################################################################
##################################################################################

#recode factors as numeric for them to be included in table

sum.dat <- dat

sum.dat$gender <- as.integer(as.character(sum.dat$gender))
sum.dat$married <- as.integer(as.character(sum.dat$married))
sum.dat$ever_div <- as.integer(as.character(sum.dat$ever_div))
sum.dat$region <- as.integer(as.character(sum.dat$region))
sum.dat$rural <- as.integer(as.character(sum.dat$rural))
sum.dat$migback <- as.integer(as.character(sum.dat$migback))

sum.dat <- select(sum.dat,
                  one_of(c("time", "event", "hhinc", "maxedu", "birthyear",
                           "gender", "region", "rural", "married", "ever_div",
                           "migback")) )

stargazer(sum.dat, type="latex",
          summary.stat=c("mean", "sd", "min",
                         "median", "max", "n"),
          title = "Summary Statistics",
          covariate.labels = c("Time to Event", "Event", "HH Income",
                               "Educ. (years)", "Birthyear", "Gender",
                               "East Germany", "Rural", "Married", 
                               "Ever Divorced", "Migr. Background"),
          digits=2,
          summary.logical = TRUE)
# need to add column w/ variable names (hhinc, maxedu etc.) manually in latex doc

rm(sum.dat)


##################################################################################
# data structure visualization ###################################################
##################################################################################


# permutate persID to get rid of time dependencies in $time
dat.str <- dat
dat.str$pnr <- sample(1:nrow(dat), nrow(dat), replace=F)

  ggplot(dat.str, aes(x = pnr, y = time)) +
    geom_linerange(aes(ymin = 0, ymax = time), size=0.3) +
    geom_point(aes(shape = as.factor(event), color = as.factor(event)), stroke = 0.5, cex = 1) +
    scale_shape_manual(values = c(3,4)) + guides(shape = F, color = F) +
    labs(y = "Time (years)", x = "Subject ID") + coord_flip() + theme_classic()

# plot too crowded, look only at subsample of 500
  
dat.str1 <- subset(dat.str, pnr<500)

ggplot(dat.str1, aes(x = pnr, y = time)) +
  geom_linerange(aes(ymin = 0, ymax = time), size=0.65) +
  geom_point(aes(shape = as.factor(event), color = as.factor(event)), stroke = 1, cex = 2) +
  scale_color_manual(values = c("steelblue1", "red2"))+
  scale_shape_manual(values = c(4,2)) + guides(shape = F, color = F) +
  labs(y = "Time (years)", x = "Subject ID") + coord_flip() + theme_classic()


##################################################################################
# density plot of survival time ##################################################
##################################################################################

dens.dat <- subset(dat, event==1)

ggplot(dens.dat, aes(x=time)) + 
  geom_histogram(binwidth = 0.2, aes(fill = ..count..) ) + theme_classic() +
  labs(y = "Count", x = "Time (years)")

hist(dens.dat$time, breaks = 15, freq = F, xlab = 'Time', ylim = c(0, 0.2), ylab = 'Relative Frequency', main = 'Histogram of Survival Times')
lines(density(dens.dat$time, na.rm = T, from = 0, to = 30))

hist(dens.dat$time, # histogram
     col="Dodger Blue", # column color
     border="black",
     prob = TRUE, # show densities instead of frequencies
     xlab = "Time",
     ylab = "Relative Frequency",
     xlim = c(0, 30), 
     ylim = c(0, 0.28),
     breaks=30,
     main = "Survival Time Density")
lines(density(dens.dat$time, from = 0, to = 30), # density plot
      lwd = 2, # thickness of line
      col = "red")


##################################################################################
##################################################################################



# distribution of time to failure by state

dist <- subset(dat, event==1)
dist$yeargroup <- as.factor(dist$firstyear)
levels(dist$state)
hist(dist$time)

ggplot(
  dist, 
  aes(x = dist$time, y = dist$state, fill=dist$state)
) +
  geom_density_ridges_gradient(
    aes(fill = ..x..), scale = 3, size = 0.3
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

# distribution of hhinc by State

dist <- subset(dat, hhinc<90000)
dist$yeargroup <- as.factor(dist$firstyear)
levels(dist$state)
hist(dist$hhinc)

ggplot(
  dist, 
  aes(x = dist$hhinc, y = dist$state, fill=dist$region)
) +
  geom_density_ridges_gradient(
    aes(fill = ..x..), scale = 2.5, size = 0.3
  ) +
  scale_fill_gradientn(
    colours = c("#0D0887FF", "#CC4678FF", "#F0F921FF"),
    name = "Duration"
  )+
  theme_ridges() +
  labs(title = 'Density of Household Income ', x = "Household Income", y = "States")


ggplot(dist, aes(x = hhinc, y = state, fill = state)) + 
  geom_density_ridges(scale = 2.5) + theme_minimal() +
  scale_fill_cyclical(values = c("blue", "green")) +
  labs(title = 'Density of Household Income', x = "Household Income", y = "States")

rm(dist)



# Distribution of educational attainment by State


dist <- subset(dat, event==1)
dist$yeargroup <- as.factor(dist$firstyear)
levels(dist$state)
hist(dist$maxedu)

ggplot(
  dist, 
  aes(x = dist$maxeu, y = dist$state, fill=dist$state)
) +
  geom_density_ridges_gradient(
    aes(fill = ..x..), scale = 2.5, size = 0.3
  ) +
  scale_fill_gradientn(
    colours = c("#0D0887FF", "#CC4678FF", "#F0F921FF"),
    name = "Duration"
  )+
  theme_ridges() +
  labs(title = 'Density of Years of Education', x = "Education", y = "States")


ggplot(dist, aes(x = maxedu, y = state, fill = state)) + 
  geom_density_ridges(scale = 2.5) + theme_minimal() +
  scale_fill_cyclical(values = c("blue", "green")) +
  labs(title = 'Density of Years of Education', x = "education", y = "States")

rm(dist)




# area + contour 2d density
ggplot(dat, aes(x=maxedu, y=hhinc) ) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", colour="white",
                  n=100, h=NULL)



# Draftmans Plot

pairplotcont <- dat[, c("hhinc", "maxedu", "time")]
pairplotdisc <- dat[, c("gender", "event", "married", "region")]

pairplot <- dat[, c("time", "event", "hhinc", "maxedu", "migback", "rural")]

pairs(pairplotcont, main = "Survival Data", pch = 21, bg = c("red", "blue")[unclass(dat$region)])

ggpairs(pairplot, lower = list(continuous = "points", combo = "box", discrete="facetbar"))

ggpairs(pairplotdisc, lower = list(discrete="box"))

ggpairs(pairplotcont, lower = list(continuous = "smooth_loess"), diag = list(continuous = "density"))


rm(pairplotcont, pairplotdisc)

# Visualization of missing values in wide dataset #
# In case of income, a lot of zero values not defined as NA
dat$hhinc[dat$hhinc < 0] <- NA

#few missings in final data set => no surprise b/c of generated variables


sapply(dat, function(x) sum(is.na(x)))

aggr_plot <- aggr(dat, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE,
                  labels=names(dat), cex.axis=.7, gap=3, 
                  ylab=c("Histogram of missing data","Pattern"))
rm(aggr_plot)

gg_miss_fct(x = dat, fct = firstyear)

#View(dat)

##################################################################################
##################################################################################
#################### ANALYSIS ####################################################
##################################################################################
##################################################################################


# Things to do:
# Classic nonparametric estimators:
# KM, Nelson-Aalen/Fleming-Harrington comparison for overall group
# CDF, Survival function, cumulative hazard function for both estimators
# KM by strata (most interesting ones migback, highinc etc.)
# Cox PH regression:
# results table, plot overall, ggadjust plots, schoenfeld test plot, hazard ratios, other
# Comparison Parametric Models and Cox PH / KM:
# Coefficient table for different distributions and Cox PH in comparison, AICs table (done)
# Plot comparing Cox PH, KM, and parametric models (done)
# Mordern nonparametric estimator: random survival forest:
# survival function plots, plots for different strata, concordance index between models,
# prediction error curve, partial dependence surface, VIMP (variable importance)
# ggRandomForests

# KM by region ####################################################################

wide.fit <- survfit(Surv(time, event, type="right") ~ region, data=dat)

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

wide.fit <- survfit(Surv(time, event, type="right") ~ migback, data=dat)
kmcurve <- ggsurvplot(wide.fit, conf.int=T,
                      legend.labs=c("No", "Yes"), legend.title="Migr. Back.",  
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

medinc <- median(as.numeric(dat$hhinc), na.rm=TRUE)
dat <- mutate(dat, highinc = ifelse(dat$hhinc > medinc, 1, 0))
summary(dat$highinc)
#define survival object and fit KM estimator
inc.fit <- survfit(Surv(time, event, type="right") ~ highinc, data=dat)
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

mededu <- median(dat$maxedu, na.rm=TRUE)
dat <- mutate(dat, highedu = ifelse(dat$maxedu > mededu, 1, 0))
summary(dat$highedu)
#define survival object and fit KM estimator
edu.fit <- survfit(Surv(time, event, type="right") ~ highedu, data=dat)
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

dat <- mutate(dat, cohort8494 = ifelse (dat$firstyear<=1987, 1,ifelse(dat$firstyear>=1994 & dat$firstyear<=1997, 2, NA)))
summary(dat$cohort8494)
table(dat$cohort8494)
#define survival object and fit KM estimator
coh.fit <- survfit(Surv(time, event, type="right") ~ cohort8494, data=dat)
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

dat <- mutate(dat, cohort8404 = ifelse
                (dat$firstyear>=1984 & dat$firstyear<=1987, 1,
                  ifelse(dat$firstyear>=2004 & dat$firstyear<=2007, 2, NA)))
summary(dat$cohort8404)
table(dat$cohort8404)
#define survival object and fit KM estimator
coh.fit2 <- survfit(Surv(time, event, type="right") ~ cohort8404, data=dat)
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
coxsurv <- Surv(dat$time, dat$event, type="right")
plot(coxsurv)

#Cox PH model

#using survival package
cox.ph <- coxph(coxsurv ~ hhinc + rural + maxedu + region + migback + married , data=dat)

cox.ph <- coxph(coxsurv ~ hhinc + rural + maxedu + region + migback , data=dat)
print(cox.ph)

#using rms package
cox.ph2 <- cph(ccoxsurv ~ hhinc + rural + maxedu + region + migback + married , data=dat,
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
medinc <- median(dat$hhinc, na.rm=TRUE)
dat <- mutate(dat, highinc = ifelse(dat$hhinc > medinc, 2, 1))
summary(dat$highinc)
rm(medinc)

ggadjustedcurves(cox.ph, data = dat, variable = "highinc")


#by firstyear

dat$firstyear <- as.integer(dat$firstyear)
cu <- c(1984, 1990, 2000, 2010, 2015)
dat$yearcut <- cut(dat$firstyear, cu, dig.lab = min(nchar(cu)))
ggadjustedcurves(cox.ph, data = dat, variable="yearcut")
rm(cu)

rm(cox.ph, cox.ph2, coxtest, coxsurv)


##################################################################################################
# KM, Cox PH and Parametric distributions plot ###################################################
##################################################################################################

# define survival object
coxparm <- Surv(dat$time, dat$event, type="right")

# define model formula
parmform <- as.formula("coxparm ~ hhinc + rural + maxedu + region + migback + married + ever_div")

# Kaplan-Meier estimator
kapm <- survfit(coxparm ~ 1, data=dat)

# fortify puts survival table into from kapm object in data frame
kap.dat <- fortify(kapm)

# Cox PH model
cox.ph <- coxph(formula=parmform, data=dat)
summary(cox.ph)

# fortify cox model output
cox.dat <- fortify(survfit(cox.ph, conf.int = F))

# Flexible splines (Royston and Parmar 2002)
flex.spline <- flexsurvspline(coxparm ~ 1, data = dat, k = 2, scale = "odds")

# Weibull distribution
weibull <- flexsurvreg(formula = parmform, data = dat, dist = "weibull")

# Exponential distribution
expo <- flexsurvreg(formula = parmform, data = dat, dist = "exp")

# Log-Logistic distribution
loglog <- flexsurvreg(formula = parmform, data = dat, dist = "llogis")

# Log-normal distribution
lnormal <- flexsurvreg(formula = parmform, data = dat, dist = "lnorm")


# plot all curves together

# graph approach 1 (from rpubs paper)
# plot for Cox PH model is (by default) at average of covariates
grid.arrange(
  ggplot(data.frame(summary(expo)), aes(x = time)) + 
    geom_line(aes(y = est, col = "Exponential")) +
    geom_line(data = data.frame(summary(weibull)), aes(y = est, col = "Weibull")) +
    geom_line(data = data.frame(summary(loglog)), aes(y = est, col = "Log-Logistic")) +
    geom_line(data = data.frame(summary(lnormal)), aes(y = est, col = "Log-Normal")) +
    geom_line(data = data.frame(summary(flex.spline)), aes(y = est, col = "Flexible Splines")) +
    geom_step(data = kap.dat, aes(x=time, y=surv, colour = "Kaplan-Meier"), size = 0.37)+
    geom_step(data = cox.dat, aes(x=time, y=surv, colour = "Cox PH"), size = 0.37)+
    labs(x = "Time (years)", y = "Survival Probability", col = "Models") + theme_classic(),
  ggplot(data.frame(summary(expo, type = "hazard")), aes(x = time)) + 
    geom_line(aes(y = est, col = "Exponential")) +
    geom_line(data = data.frame(summary(weibull, type = "hazard")), aes(y = est, col = "Weibull")) +
    geom_line(data = data.frame(summary(loglog, type = "hazard")), aes(y = est, col = "Log-Logistic")) +
    geom_line(data = data.frame(summary(lnormal, type = "hazard")), aes(y = est, col = "Log-Normal")) +
    geom_line(data = data.frame(summary(flex.spline, type = "hazard")), aes(y = est, col = "Flexible Splines")) +
    labs(x = "Time (years)", y = "Hazard Function", col = "Models") + theme_classic(),
  ncol = 2
)

# only survival curves as single plot
ggplot(data.frame(summary(expo)), aes(x = time)) + 
  geom_line(aes(y = est, col = "Exponential")) +
  geom_line(data = data.frame(summary(weibull)), aes(y = est, col = "Weibull")) +
  geom_line(data = data.frame(summary(loglog)), aes(y = est, col = "Log-Logistic")) +
  geom_line(data = data.frame(summary(lnormal)), aes(y = est, col = "Log-Normal")) +
  geom_line(data = data.frame(summary(flex.spline)), aes(y = est, col = "Flexible Splines")) +
  geom_step(data = kap.dat, aes(x=time, y=surv, colour = "Kaplan-Meier"), size = 0.37)+
  geom_step(data = cox.dat, aes(x=time, y=surv, colour = "Cox PH"), size = 0.37)+
  labs(x = "Time (years)", y = "Survival Probability", col = "Models") + theme_classic()

# only hazard functions as single plot
ggplot(data.frame(summary(expo, type = "hazard")), aes(x = time)) + 
  geom_line(aes(y = est, col = "Exponential")) +
  geom_line(data = data.frame(summary(weibull, type = "hazard")), aes(y = est, col = "Weibull")) +
  geom_line(data = data.frame(summary(loglog, type = "hazard")), aes(y = est, col = "Log-Logistic")) +
  geom_line(data = data.frame(summary(lnormal, type = "hazard")), aes(y = est, col = "Log-Normal")) +
  geom_line(data = data.frame(summary(flex.spline, type = "hazard")), aes(y = est, col = "Flexible Splines")) +
  labs(x = "Time (years)", y = "Hazard Function", col = "Models") + theme_classic()



##################################################################################################
# Cox PH and Parametric distributions tables #####################################################
##################################################################################################

# stargazer only compatible with survreg, not flexsurvreg

# define survival object
coxparm <- Surv(dat$time, dat$event, type="right")

# define model formula
parmform <- as.formula("coxparm ~ hhinc + rural + maxedu + region + migback + married + ever_div")

# Cox PH model
cox.ph.tab <- coxph(formula=parmform, data=dat)
summary(cox.ph.tab)

# Weibull distribution
weibull.tab <- survreg(formula = parmform, data = dat, dist = "weibull")

# Exponential distribution
expo.tab <- survreg(formula = parmform, data = dat, dist = "exp")

# Log-Logistic distribution
loglog.tab <- survreg(formula = parmform, data = dat, dist = "loglogistic")

# Log-normal distribution
lnormal.tab <- survreg(formula = parmform, data = dat, dist = "lognormal")


# Results table (nees more options maybe)
# should maybe be odds ratios/relative risk/sth instead of coefficients
stargazer(cox.ph.tab, weibull.tab, expo.tab, loglog.tab, lnormal.tab, align=F)


# AIC for parametric models (model selection)

AICs <- matrix(data = NA, nrow = 4, ncol = 1)
AICs[1, 1] <- weibull$AIC
AICs[2, 1] <- expo$AIC
AICs[3, 1] <- loglog$AIC
AICs[4, 1] <- lnormal$AIC
rownames(AICs) <- c("Weibull", "Exponential", "Log-Logistic", "Log-Normal")
colnames(AICs) <- "AIC"
AICs
#Log-normal has best fit


# Random Survival Forests #################################################
#preliminary version

fitform <- Surv(time, event) ~ hhinc + rural + maxedu + region + migback

set.seed(0692)
rsf <- rfsrc(fitform, data = dat, forest = TRUE, ntree = 500, importance = TRUE)
print(rsf)


#ggRFsrc <- plot(gg_rfsrc(rsf), alpha = 0.2)
#show(ggRFsrc)

plot(gg_rfsrc(rsf, by = "region"))

plot(gg_rfsrc(rsf, by = "migback"))



#################################################################################################
# OLD Analysis in long data set #################################################################
#################################################################################################

#All of the following are simple KM curves using the long format for survival objects
#   => long format should not be used anymore, wide is now available (above) from $$dat


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




