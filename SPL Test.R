#Nötige Packages

#install.packages("bigmemory",dependencies=TRUE)
#install.packages("biganalytics",dependencies=TRUE)
#install.packages("dplyr",dependencies=TRUE)



library("data.table", lib.loc="~/R/win-library/3.4")
library("dplyr", lib.loc="~/R/win-library/3.4")
library(bigmemory)
library("Matrix", lib.loc="C:/Program Files/R/R-3.4.4/library")
library("stats", lib.loc="C:/Program Files/R/R-3.4.4/library")
#library("survival", lib.loc="C:/Program Files/R/R-3.4.4/library")h
library("bitops", lib.loc="~/R/win-library/3.4")
library(haven)
library(data.table)
library(ggplot2)
#library("plyr", lib.loc="~/R/win-library/3.4")
#library(dplyr)
#memory.limit()
#--------------------------------------------------------
#Datensatz HH Daten


hl <- read_dta("GitHub/SPL_SS18/SOEP-LONG_v32.1/hl.dta")
#View(hl)

#Definition als Datenmatrix
hl = as.data.table(hl)
summary(hl$hlc0007)



#--------------------------------------------------------
#Interessante Variablen 

#erbschaft ist hlc0032 ,  ALG2, Sozi4algeld im letzten Jahr , bezogen hlc0052 
# Anzahl Kinder hlc0043, hlk0044
#Eigenheimsförderung hlc0075
#Grundbesitz hlc0099
# WICHTIG : Besitz der Wohnung : hlf0001, hlf0002, hlf0004, hlf0008, 
# Gute Variable um Zeitpunkt zuordnen zu können : Schon vor einem Jahr Eigentümer :  hlf0014 und  hlf0015 : Seit diesem Jahr neuerdings EIgentümer
# Wohnfläche : hlf0019, hlf0020
# Modernisierung hlf0046
# Mitekosten hlf0095,
# Einzugsjahr hlf0101



#--------------------------------------------------------
#Haushaltsnettoeinkommen deskriptive Analyse

summary(hl$hlc0005)


hlc0005 = as.integer(hl$hlc0005)

#Wie soll mit Missing values umgegangen werden ?

#Variable ohne Missings
#hlc0005 <- ifelse(hlc0005>=0,hlc0005, hlc0005 == NA)
hlc0005_wom = hlc0005[!(hlc0005 < 0)]

hlc0005_wom
mean(hlc0005_wom)
mean(hlc0005[!(hlc0005 < 0)])

#Warum geht das so nicht ? Gruppierung nicht mehr möglich
sumyeartest = hl[, list(Average = mean(hlc0005_wom), Minimum =  min(hlc0005_wom)), by = "syear"]
sumyeartest

#So gehts
sumincome = hl[, list(meanincome = mean(hlc0005[!(hlc0005 < 0)]), Minimum =  min(hlc0005[!(hlc0005 < 0)])), by = "syear"]
sumincome



#Grafische Veranschaulichung
ggplot(sumincome, aes(syear, meanincome)) + geom_point()
ggplot(sumincome, aes(syear,  meanincome)) + geom_line()


#Kaufjahr des Eigentums muss noch überarbeitet werden

kaufjahr = ifelse(hl$hlf0015 == 1, kaufjahr <- hl$syear, kaufjahr <- NA)

mean(kaufjahr, na.rm = TRUE)

ifelse(hl$hlf0014 == 1, kaufjahr <- hl$syear-1, kaufjahr <- NA)
library(foreach)
foreach(hl$cid == 1: 3499900, kauf <- min(kaufjahr))

#Mietkosten hlf0074 oder auch geschätzte Mietkosten hlf0095


summiete = hl[, list(meanmiete = mean(hlf0074[!(hlf0074 < 0)]), Minimum =  min(hlf0074[!(hlf0074 < 0)])), by = "syear"]
summiete
summiete$meanmiete

ggplot(sumincome, aes(syear,  meanincome)) + geom_line()
ggplot(summiete, aes(syear,  meanmiete, color= meanmiete)) + geom_line()
class(ratio)

#Ratio between Miete und Einkommen starke schwankungen erkennbar vorallem bei Reunion und Euro Einführung

rate= summiete$meanmiete / sumincome$meanincome
rate

plot(rate,  bg = "chartreuse3", pch = 21, cex = 1, main = "Ratio Miete Income")
