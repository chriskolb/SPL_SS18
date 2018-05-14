#install.packages("bigmemory",dependencies=TRUE)
#install.packages("biganalytics",dependencies=TRUE)

#install.packages("dplyr",dependencies=TRUE)



library("data.table", lib.loc="~/R/win-library/3.4")
library("dplyr", lib.loc="~/R/win-library/3.4")
library(bigmemory)
#memory.limit()
library("Matrix", lib.loc="C:/Program Files/R/R-3.4.4/library")
library("stats", lib.loc="C:/Program Files/R/R-3.4.4/library")
#library("survival", lib.loc="C:/Program Files/R/R-3.4.4/library")h
library("bitops", lib.loc="~/R/win-library/3.4")
library(haven)
hl <- read_dta("Statistik/Semester 2/SPL/SOEP-LONG_v32.1/hl.dta")
View(hl)
library(data.table)
hl = as.data.table(hl)
summary(hl$hlc0007)
#Haushaltsnettoeinkommen deskriptive Analyse
summary(hl$hlc0005)
ifelse(hl$hlc0005>0,print(hl$hlc0005),NA)
#Median und Minimum für gewisse Jahre 
#library("plyr", lib.loc="~/R/win-library/3.4")
#library(dplyr)

data = data.frame(hl)
hlc0005 = as.integer(hl$hlc0005)
# hl$hlc0005 = data 
#  group_by(syear)
#  summarise(hl$hlc0005)
library(data.table)
library(ggplot2)
hl = as.data.table(hl) 
sumyear = hl[, list(Average = mean(hlc0005), Minimum =  min(hlc0005)), by = "syear"]
sumyear

ggplot(sumyear, aes(syear, Average)) + geom_point()
ggplot(sumyear, aes(syear,  Average)) + geom_line()
print("Chris ist ein HS")
