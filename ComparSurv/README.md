
[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)
## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **ComparSurv** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of QuantLet : ComparSurv

Published in : SPL

Description : 'Estimated survival function for four classical parametric distributions 
	       (Exponential model,  log-logistic model, log-normal model and Weibull model), 
	       flexible parametric splines, Kaplan-Meier estimator and Cox regression'

Keywords : 'survival analysis, non-parametric estimation, parametric distribution, 
            Cox Regression, Kaplan Meier, R'

Author : Alice Drube, Konstantin Göbler, Chris Kolb, Richard v. Maydell

```

![Picture1](ComparisonSurvival.png)

### R Code

```R


rm(list = ls())

# set working directory setwd('C:/...') 
# setwd('~/...') # linux/mac os
# setwd('/Users/...') # windows

# install and load packages
libraries = c("survival", "rms", "survminer", "dplyr", "readr", "flexsurv",
  "ggfortify", "ggplot2")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

# load dataset
load("datfinal.RDA")

# define survival object for Cox regression
coxsurv = Surv(dat$time, dat$event, type = "right")
# define formula
coxform = as.formula("coxsurv ~ hhinc + rural + maxedu + region + migback +
  married + ever_div")
# Cox PH regression
cox.ph = coxph(coxform, data = dat)


# define survival object for parametric models and KM estimator
coxparm = Surv(dat$time, dat$event, type = "right")
# define model formula
parmform = as.formula("coxparm ~ hhinc + rural + maxedu + region + migback + 
  married + ever_div")
  
# Exponential distribution
expo = flexsurvreg(formula = parmform, data = dat, dist = "exp")
# Weibull distribution
weibull = flexsurvreg(formula = parmform, data = dat, dist = "weibull")
# Log-Logistic distribution
loglog = flexsurvreg(formula = parmform, data = dat, dist = "llogis")
# Log-normal distribution
lnormal = flexsurvreg(formula = parmform, data = dat, dist = "lnorm")
# Flexible splines (Royston and Parmar 2002)
flex.spline = flexsurvspline(coxparm ~ 1, data = dat, k = 2, scale = "odds")
# Kaplan-Meier estimator
kapm = survfit(coxparm ~ 1, data = dat)
# puts survival table from kapm object into a data frame
kap.dat = fortify(kapm)
# fortify cox model output
cox.dat = fortify(survfit(cox.ph, conf.int = F))



ggplot(data.frame(summary(expo)), aes(x = time)) + 
  geom_line(aes(y = est, col = "Exponential")) + 
  geom_line(data = data.frame(summary(weibull)), 
  aes(y = est, col = "Weibull")) + 
  geom_line(data = data.frame(summary(loglog)), 
  aes(y = est, col = "Log-Logistic")) + 
  geom_line(data = data.frame(summary(lnormal)), 
  aes(y = est, col = "Log-Normal")) + 
  geom_line(data = data.frame(summary(flex.spline)), 
  aes(y = est, col = "Flexible Splines")) + 
  geom_step(data = kap.dat, 
  aes(x = time, y = surv, colour = "Kaplan-Meier"), size = 0.37) + 
  geom_step(data = cox.dat, 
  aes(x = time, y = surv, colour = "Cox PH"), size = 0.37) + 
  labs(x = "Time (years)", y = "Survival Probability", col = "Models") + 
  theme_classic()
  ```
