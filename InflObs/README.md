[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)


```yaml

Name of QuantLet : 

Published in : SPL

Description : 
	      
              


Keywords : 'survival analysis, non-parametric estimation, semi-parametric estimation,
	    hazard rate, Kaplan Meier'

Author : Alice Drube, Konstantin Göbler, Chris Kolb, Richard v. Maydell

```

![Picture1](Delta_Beta.png)

### R Code
```R
rm(list = ls())

# set working directory setwd('C:/...') 
# setwd('~/...') # linux/mac os
# setwd('/Users/...') # windows

# install and load packages
rm(list = ls())


libraries = c("survival", "rms", "survminer", "stargazer", "reporttools", 
              "dplyr", "readr", "flexsurv", "ggfortify", "ggplot2")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
    install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)


load("datfinal.RDA")


# define survival object
coxsurv = Surv(dat$time, dat$event, type = "right")

# define formula
coxform = as.formula("coxsurv ~ hhinc + rural + maxedu + region + migback + 
                     married + ever_div")

# estimate Cox regression
cox.ph = coxph(coxform, data = dat)
summary(cox.ph)

# Cox PH model table
stargazer(cox.ph)
displayCoxPH(cox.ph, cap = "", dig.coef = 3, dig.p = 2)

# table in overleaf is constructed from both outputs


# Forest plot of results
dat = within(dat, {
    rural = factor(rural, labels = c("urban", "rural"))
    region = factor(region, labels = c("west", "east"))
    migback = factor(migback, labels = c("No", "Yes"))
    married = factor(married, labels = c("No", "Yes"))
    ever_div = factor(ever_div, labels = c("No", "Yes"))
})
cox.ph = coxph(coxform, data = dat)

ggforest(cox.ph)

### testing for influential Observations ####

# use Delta-Beta residuals to detect influential observations scaled by standard
# errors of coefficients
ggcoxdiagnostics(cox.ph, type = "dfbetas", ox.scale = "observation.id", 
                 hline.col = "darkgreen", hline.alpha = 0.5, point.alpha = 0.4,
                 point.col = "lightcoral", sline.alpha = 0.4, 
                 sline.col = "dodgerblue")
```