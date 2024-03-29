
[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **ComparHaz** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)
```yaml

Name of QuantLet : ComparHaz

Published in : SPL

Description : 'Compare estimated hazard function for five different models: Exponential model, 
	       Model with flexible splines, log-logistic model, log-normal model and Weibull model. 
               The graph shows the transitioning into homeownership'
	      
Keywords : 'survival analysis, parametric estimation, Weibull, Log-normal, Log-logistic,
            Exponential, flexible splines, hazard rate, R'

Author : Alice Drube, Konstantin Göbler, Chris Kolb, Richard v. Maydell
```
![Picture1](ComparHaz.png)
### R Code
```R

rm(list = ls())

# set working directory setwd('C:/...') setwd('~/...') # linux/mac os
# setwd('/Users/...') # windows

# load packages
libraries = c("survival", "rms", "survminer", "dplyr", "readr", "flexsurv", 
  "ggfortify", "ggplot2")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

# load dataset
load("datfinal.RDA")

# define survival object
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

# only hazard functions as single plot
ggplot(data.frame(summary(expo, type = "hazard")), aes(x = time)) + 
  geom_line(aes(y = est, col = "Exponential")) + 
  geom_line(data = data.frame(summary(weibull, type = "hazard")), 
  aes(y = est, col = "Weibull")) + 
  geom_line(data = data.frame(summary(loglog, 
  type = "hazard")), aes(y = est, col = "Log-Logistic")) + 
  geom_line(data = data.frame(summary(lnormal, 
  type = "hazard")), aes(y = est, col = "Log-Normal")) + 
  geom_line(data = data.frame(summary(flex.spline, 
  type = "hazard")), aes(y = est, col = "Flexible Splines")) + 
  labs(x = "Time (years)", 
  y = "Hazard Function", col = "Models") + theme_classic()
```
