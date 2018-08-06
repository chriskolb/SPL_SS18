
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

load("datfinal.RDA")

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
  migback <- factor(migback, labels = c("No", "Yes"))
  married <- factor(married, labels = c("No", "Yes"))
  ever_div <- factor(ever_div, labels = c("No", "Yes"))
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
ggcoxzph(coxtest, resid = T, point.col="lightcoral", point.alpha = 0.4, var=c("hhinc","maxedu"))

ggcoxzph(coxtest, point.alpha = 0.4, point.col="lightcoral")


###--------- Testing for influential Observations ----------####

# use Delta-Beta residuals to detect influential observations scaled by standard errors of coefficients

ggcoxdiagnostics(cox.ph, type = "dfbetas", ox.scale= "observation.id", hline.col = "darkgreen", hline.alpha = 0.5, point.alpha = 0.4, point.col = "lightcoral", sline.alpha = 0.4, sline.col = "dodgerblue")


rm(cox.ph, coxtest, coxsurv, coxform)


##################################################################################################
# KM, Cox PH and Parametric distributions plot ###################################################
##################################################################################################

# define survival object
coxparm <- Surv(dat$time, dat$event, type="right")

# define model formula
parmform <- as.formula("coxparm ~ hhinc + rural + maxedu + region + migback + married + ever_div")

# Kaplan-Meier estimator
kapm <- survfit(coxparm ~ 1, data=dat)

# fortify as well as tidy from the broom package
# puts survival table from kapm object into a data frame
kap.dat <- fortify(kapm)
#kap.dat2 <- tidy(kapm)

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

