
#DESCRIPTIVES AND ANALYSIS

###################################################################################

rm(list=ls())

# setwd(path) in path.R
source(".path.R")

# install and load packages
source("packages.R")

source("functions.R")

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
# missing data plots before and after imputation (done)
# distributions and densities (by state plots, draftmans plot, countour/3d)

##################################################################################
# summary table ##################################################################
##################################################################################

#recode factors as numeric for them to be included in table

sum.dat <- dat

sum.dat$gender <- as.integer(as.character(sum.dat$gender))
sum.dat$married <- as.integer(as.character(sum.dat$married))
sum.dat$ever_div <- as.integer(as.character(sum.dat$ever_div))
sum.dat$region <- as.integer(sum.dat$region))
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


# permutate persID to get rid of pnr (=syear) dependencies in surv. time
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
  geom_linerange(aes(ymin = 0, ymax = time), size=0.5) +
  geom_point(aes(shape = as.factor(event), color = as.factor(event)), stroke = 1.3, cex = 2) +
  scale_shape_manual(values = c(1,4))  + labs(color="Event") + guides(shape=F) +
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
  aes(x = dist$maxedu, y = dist$state, fill=dist$state)
) +
  geom_density_ridges_gradient(
    aes(fill = ..x..), scale = 2.5, size = 0.3
  ) +
  scale_fill_gradientn(
    colours = c("#0D0887FF", "#CC4678FF", "#F0F921FF"),
    name = "Duration"
  )+
  theme_ridges() +
  labs(title = 'Density of Years of eduation', x = "Education", y = "States")


ggplot(dist, aes(x = maxedu, y = state, fill = state)) + 
  geom_density_ridges(scale = 2.5) + theme_minimal() +
  scale_fill_cyclical(values = c("blue", "green")) +
  labs(title = 'Density of Years of Education', x = "Education", y = "States")

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
# CDF, Survival function, cumulative hazard function for KM and Nelson/Aalen/Fleming-Harrington (done)
# KM by strata (most interesting ones migback, highinc etc.) (done)

# Cox PH regression:
# results table, plot overall, ggadjust plots, schoenfeld test plot, hazard ratios, other

# Comparison Parametric Models and Cox PH / KM:
# Coefficient table for different distributions and Cox PH in comparison, AICs table (done)
# Plot comparing Cox PH, KM, and parametric models (done)

# Mordern nonparametric estimator: random survival forest:
# survival function plots, plots for different strata, concordance index between models,
# prediction error curve, partial dependence surface, VIMP (variable importance)
# ggRandomForests


##################################################################################################
# Comparison Kaplan-Meier & Nelson-Aalen/Fleming-Harrington ######################################
##################################################################################################

# Kaplan-Meier estimator
km.fit <- survfit(Surv(time,event, type="right") ~ 1, data=dat, type="kaplan-meier")
# Fleming-Harrington estimator
fh.fit <- survfit(Surv(time,event, type="right") ~ 1, data=dat, type="fleming-harrington")
kmfh.all <- list(km.fit, fh.fit)

#Survival Function
surv.all <- nonparametricKurves()
#Cumulative Event Function: f(y)=1-y
#cumprop.all <- nonparametricKurves("event")
#Cumulative Hazard Function
cumhaz.all <- nonparametricKurves("cumhaz")

# put all plots in one graph

kmfh.glist <- list(surv.all, cumhaz.all)

arrange_ggsurvplots(kmfh.glist, print = TRUE, ncol = 2, nrow = 1)

##################################################################################################
# KM by strata ###################################################################################
##################################################################################################

# KM by gender ###################################################################################
# 0 = male, 1 = female
wide.fit <- survfit(Surv(time, event, type="right") ~ gender, data=dat)

km.sex <- kmGroupKurves(c("Male", "Female"), "Gender")

rm(wide.fit)

# KM by Metropolitan Area ###################################################################################
# 0 = Urban, 1 = Rural
wide.fit <- survfit(Surv(time, event, type="right") ~ rural, data=dat)

km.urban <- kmGroupKurves(c("Urban", "Rural"), "Metropolitan Area")

rm(wide.fit)

# KM by married divorced ###################################################################################

wide.fit <- survfit(Surv(time, event, type="right") ~ ever_div, data=dat)

km.div <- kmGroupKurves(c("No", "Yes"), "Ever Divorced")


wide.fit <- survfit(Surv(time, event, type="right") ~ married, data=dat)

km.marr <- kmGroupKurves(c("No", "Yes"), "Married")


rm(wide.fit)

# KM by region ###################################################################################

wide.fit <- survfit(Surv(time, event, type="right") ~ region, data=dat)

km.reg <- kmGroupKurves(c("West", "East"), "Region")

rm(wide.fit)


# KM by migback ####################################################################

wide.fit <- survfit(Surv(time, event, type="right") ~ migback, data=dat)

km.mig <- kmGroupKurves(c("No", "Yes"), "Migr.Back.")

rm(wide.fit)


# KM by highinc/lowinc ###########################################################

medinc <- median(as.numeric(dat$hhinc), na.rm=TRUE)
dat.inc <- mutate(dat, highinc = ifelse(dat$hhinc > medinc, 1, 0))
summary(dat.inc$highinc)
#define survival object and fit KM estimator
wide.fit <- survfit(Surv(time, event, type="right") ~ highinc, data=dat.inc)

km.inc <- kmGroupKurves(c("Low", "High"),"HH Inc.")

rm(wide.fit, medinc, dat.inc)


# KM by educ (ISCED 97) ###########################################################

#define survival object and fit KM estimator
wide.fit <- survfit(Surv(time, event, type="right") ~ educ, data=dat)

km.edu <- kmGroupKurves(c("Elementary", "Medium", "Higher voc.", "High"),"Education", line = c(1,1,1,1), conf=F)

rm(wide.fit)

# KM by cohorts 84-87 and 94-97 #########################################################

dat <- mutate(dat, cohort8494 = ifelse (dat$firstyear<=1987, 1,ifelse(dat$firstyear>=1994 & dat$firstyear<=1997, 2, NA)))
summary(dat$cohort8494)
table(dat$cohort8494)
#define survival object and fit KM estimator
wide.fit <- survfit(Surv(time, event, type="right") ~ cohort8494, data=dat)

km.coh <- kmGroupKurves(c("84-87", "94-97"),"Cohorts")

rm(wide.fit)


# arrange plots ############################################################

km.glist1 <- list(km.inc, km.mig)

km.plot1 <- arrange_ggsurvplots(km.glist1, ncol = 2, nrow = 1, print = FALSE,
                    risk.table.height = 0.25,
                    surv.plot.height = 1)


km.glist2 <- list(km.reg, km.urban)

km.plot2 <- arrange_ggsurvplots(km.glist2, ncol = 2, nrow = 1, print = FALSE,
                    risk.table.height = 0.25,
                    surv.plot.height = 1)

km.glist3 <- list(km.marr,km.div)
km.plot3 <- arrange_ggsurvplots(km.glist3, ncol = 2, nrow = 1, print = FALSE,
                                risk.table.height = 0.25,
                                surv.plot.height = 1)

# print KM by strata plots ##################################################

print(km.plot1)
print(km.plot2)
print(km.plot3)
print(km.edu)


##################################################################################################
# Cox Proportional Hazards Regression ############################################################
##################################################################################################

#survival package to estimate models, survminer package for plots and diagnostics
# define survival object

coxsurv <- Surv(dat$time, dat$event, type="right")

# define formula

coxform <- as.formula("coxsurv ~ hhinc + rural + maxedu + region + migback + married + ever_div")

# estimate Cox regression

cox.ph <- coxph(coxform, data=dat)
summary(cox.ph)

# Cox PH model table

stargazer(cox.ph)

displayCoxPH(cox.ph, cap = "", dig.coef = 3, dig.p = 2)

# table in overleaf is constructed from both outputs


# Forest plot of results
dat <- within(dat,{
    rural <- factor(rural, labels = c("urban", "rural"))
    region <- factor(region, labels = c("west", "east"))
    migback <- factor(migback, labels = c("no migr. backgr.", "migr. backgr."))
    married <- factor(married, labels = c("not married", "married"))
    ever_div <- factor(ever_div, labels = c("never divorced", "ever divorced"))
  })
cox.ph <- coxph(coxform, data=dat)

ggforest(cox.ph)


# adjusted survival curves from cox model
# not needed later
# by highinc
medinc <- median(dat$hhinc, na.rm=TRUE)
dat <- mutate(dat, highinc = ifelse(dat$hhinc > medinc, 2, 1))
summary(dat$highinc)
rm(medinc)

ggadjustedcurves(cox.ph, data = dat, variable = "highinc")

# by education level (ISCED97)

ggadjustedcurves(cox.ph, data = dat, variable = "educ")


##### Model Diagnostics #####
### (Schoenfeld test, influential observations) ###


# Schoenfeld test of cox ph assumption

coxtest <- cox.zph(cox.ph, transform = "km")
coxtest
stargazer(coxtest$table, out = "schoenfeld.tex")

# Schoenfeld graphical test of cox ph assumption

# scaled Schoenfeld plots for only selected variables (have to be continuous)
ggcoxzph(coxtest, resid = T, point.alpha = 0.4, var=c("hhinc","maxedu"), caption = "Schoenfeld residual plot of HH Income and Years of Education")

ggcoxzph(coxtest)


###--------- Testing for influential Observations ----------####

# use Delta-Beta residuals to detect influential observations

ggcoxdiagnostics(cox.ph, type = "dfbeta", ox.scale= "observation.id")


rm(cox.ph, cox.ph, coxtest, coxsurv, coxform)


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


