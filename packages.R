#install packages
install.packages("bigmemory",dependencies=TRUE)
install.packages("biganalytics",dependencies=TRUE)
install.packages("dplyr",dependencies=TRUE)
install.packages("plm")
install.packages("varhandle")
install.packages("survival")
install.packages("rms")
install.packages("survminer")
install.packages("Hmisc")
install.packages("pastecs")
install.packages("rms")
install.packages("car",dependencies=TRUE)
install.packages("rio", dependencies = TRUE)
install.packages("pastecs") #summary stats
install.packages("Hmisc") #summary stats 
install.packages(c("rgl", "car"))#scatter3d()
install.packages("haven")
install.packages("VIM")
install.packages("naniar")
install.packages("ggridges")
install.packages("GGally")
install.packages("rio") #loading data


library(rio) #reading data into R
library(foreign) 
library(car) # scatter3d()
library(Hmisc) # describe(mydata)
library(varhandle)
library(pastecs) #stat.desc(mydata)
library(rms)
library(survminer)
library(plm)
library(data.table)
library(bigmemory)
library(Matrix)
library(stats)
library(survival)
library(bitops)
library(haven)
library(plyr) #plyr muss vor dplyr
library(tidyr)
library(ggplot2)
library(dplyr) # das hier immer als letztes du Nase
