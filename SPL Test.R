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
#erbschaft ist hlc0032 ,  ALG2, Sozi0014algeld im letzten Jahr, bezogen hlc0052 
# Anzahl Kinder hlc0043, hlk0044
#Eigenheimsf�rderung hlc0075
#Grundbesitz hlc0099
# WICHTIG : Besitz der Wohnung : hlf0001, hlf0002, hlf0004, hlf0008, 
# Gute Variable um Zeitpunkt zuordnen zu k�nnen : Schon vor einem Jahr Eigent�mer :  hlf0014 und  hlf0015 : Seit diesem Jahr neuerdings EIgent�mer
# Wohnfl�che : hlf0019, hlf0020
# Modernisierung hlf0046
# Mitekosten hlf0095,
# Einzugsjahr hlf0101

#Kaufjahr des Eigentumsgi
ifelse(hl$hlf0015 == 1, kaufjahr <- hl$syear, kaufjahr <- NA)

ifelse(hl$hlf0014 == 1, kaufjahr <- hl$syear-1, kaufjahr <- NA)
library(foreach)
foreach(hl$cid == 1: 3499900, kauf <- min(kaufjahr))

