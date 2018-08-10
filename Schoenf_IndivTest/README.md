[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)
## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **Schoenf_IndivTest** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)
```yaml

Name of QuantLet : Schoenf_IndivTest

Published in : SPL

Description : 'Graphical diagnostic check for the proportional hazards (PH) assumption. We can test the 
               null hypothesis of PH by plotting scaled Schoenfeld residuals against time,
	       residuals show zero slope under the Null'

Keywords : 'survival analysis, semi-parametric estimation, Cox model, proportional hazards,
            testing, Schoenfeld, scaled Schoenfeld residuals, diagnostics, R'

Author : Alice Drube, Konstantin Göbler, Chris Kolb, Richard v. Maydell

```

![Picture1](schoenfeld2.png)

![Picture1](schoenfeldall.png)

### R Code

```R


rm(list = ls())

# set working directory setwd('C:/...') 
# setwd('~/...') # linux/mac os
# setwd('/Users/...') # windows

# load packages
libraries = c("survival", "rms", "survminer", "stargazer", "reporttools", "dplyr", 
   "readr", "flexsurv", "ggfortify", "ggplot2")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
   install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)


load("datfinal.RDA")


###############################################################################
########### Cox Proportional Hazards Regression################################
###############################################################################

# survival package to estimate models, survminer package for plots and
# diagnostics

# define survival object
coxsurv = Surv(dat$time, dat$event, type = "right")

# define formula
coxform = as.formula("coxsurv ~ hhinc + rural + maxedu + region + migback + 
   married + ever_div")

# estimate Cox regression
cox.ph = coxph(coxform, data = dat)
summary(cox.ph)

###############################################################################
########### Cox Model Diagnostics##############################################
###############################################################################

# Schoenfeld test statistics as .tex table
coxtest = cox.zph(cox.ph, transform = "km")
coxtest
stargazer(coxtest$table, out = "schoenfeld.tex")

#### Schoenfeld graphical test of cox ph assumption ####

# scaled Schoenfeld plots for two selected variables
ggcoxzph(coxtest, resid = T, point.col = "lightcoral", point.alpha = 0.4, 
    var = c("hhinc", "maxedu"))

# scaled Schoenfeld plots for all variables
ggcoxzph(coxtest, point.alpha = 0.4, point.col = "lightcoral")


```
