#Nötige Packages
path <- "C:/Users/Richard/Documents/Statistik/Semester 2/Datenanalyse/Analyse/Daten"
setwd(path)
#install.packages("bigmemory",dependencies=TRUE)
#install.packages("biganalytics",dependencies=TRUE)
#install.packages("dplyr",dependencies=TRUE)
install.packages("plm")
install.packages("varhandle")

rm(list=ls())
library(varhandle)
library(plm)
library("data.table", lib.loc="~/R/win-library/3.4")
library("dplyr", lib.loc="~/R/win-library/3.4")
library(bigmemory)
library("Matrix", lib.loc="C:/Program Files/R/R-3.4.4/library")
library("stats", lib.loc="C:/Program Files/R/R-3.4.4/library")
library("survival", lib.loc="~/R/win-library/3.4")
library("bitops", lib.loc="~/R/win-library/3.4")
library(haven)
library(data.table)
library(ggplot2)
library("plyr", lib.loc="~/R/win-library/3.4")
library(dplyr)
memory.limit()
#Datensatz HH Daten

pequiv <- read_dta("Statistik/Semester 2/Datenanalyse/Analyse/Daten/pequiv.dta")

#Definition als Datenmatrix
pequiv = as.data.table(pequiv)



#--------------------------------------------------------
#Interessante Variablen 

# syear , d1110211 ist gender, d11101 ist age, d11104 ist Beziehungsstand, d11108 ist Bildung, e11101 ist Arbeitsstunden, e11105 ist Beruf, i11110 ist Earning,  
#l11101 ist state of residence, ijob1 ist Lohn, 
#Krankheiten : m11105-m11112, d11112ll ist Rasse 




#--------------------------------------------------------
#Haushaltsnettoeinkommen deskriptive Analyse



#Wie soll mit Missing values umgegangen werden ?

#Variable ohne Missings
#i11110 <- ifelse(i11110>=0,hlc0005, i11110 == NA)
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
print(kaufjahr)
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

#--------------------------------------------------------------------------------------------------------------------
rm(list=ls())


#Datensatz HH Daten----------------------------------------------------------

pequiv <- read_dta("pequiv.dta")

#Definition als Datenmatrix
pequiv = as.data.table(pequiv)
pequivnew <- subset(pequiv, d11101>24 | d11101<66)
pequivnew <- subset(pequiv, d11105 == 1)
pequivnew = as.data.table(pequivnew)

#View(pequiv)

save(pequivnew, file="pequivnew.RDA")

#rm(list=ls())
#load("pequivnew.Rda")
#View(pequivnew)


# Datensatz hgen---------------------------------------------------------------------------------------------

hgen <- read_dta("hgen.dta")

merged.file = merge(pequivnew, hgen, by.pequivnew = c("syear","hid"), by.hgen = c("syear", "hid"))
View(merged.file)

#Dissolve die HH, das heisst das ein Individuum höchstens einem HH zugeordnet wird 
sort(merged.file$pid)
setDT(merged.file)[,if(.N ==1) .SD,by=pid]


# Erstellen eines Zeitindexes
merged.file$timevar <- merged.file$d11101 -25

merged.file$timevar
#Nur Individuen die jünger sind als 27

minage.file <- aggregate(merged.file$d11101 ~ merged.file$hid, merged.file , function(x) min(x))
names(minage.file)[names(minage.file) == "merged.file$d11101"] <- "minage"
names(minage.file)[names(minage.file) == "merged.file$hid"] <- "hid"
merged.file = merge(minage.file, merged.file, by.minage.file = c("hid"), by.merged.file = c("hid"))
merged.file <- subset(merged.file, merged.file$minage  < 28)

#View(merged.file)


#Zum Panel Format umformulieren-----------------------------------
pldata <- pdata.frame(merged.file, index =c("hid", "syear"))

# Haushaltsbesitzer und lagged variable 
lagowner <-  lag(pldata$hgowner)
#print(lagowner)

hgowner <- pldata$hgowner
#View(hgowner)
#print(hgowner)


# Wechsel des HH Besitzers
pldata <- mutate(pldata, change1 = ifelse(c(hgowner == 1) & c(lagowner == 2), 1, 0))
pldata <- mutate(pldata, change2 = ifelse(c(hgowner == 1) & c(lagowner ==3), 1, 0))
pldata <- mutate(pldata, change3 = ifelse(c(hgowner == 1) & c(lagowner ==4), 1, 0))
pldata <- mutate(pldata, change4 = ifelse(c(hgowner == 1) & c(lagowner ==5), 1, 0))
pldata <- mutate(pldata, changetot = ifelse(c(pldata$change1)== 1 | c(pldata$change2) == 1 | c(pldata$change3) == 1 | c(pldata$change4) == 1, 1, NA))
pldata <- mutate(pldata, changetot = ifelse(c(hgowner) >= 2 & c(hgowner) <= 5 & c(lagowner) >= 2 & c(lagowner) <= 5, 0, changetot))
pldata <- mutate(pldata, changetot = ifelse(c(hgowner)==1 & c(lagowner) ==1, 0, changetot))

#test <- data.frame(hgowner,lagowner, pldata$change1, pldata$change2, pldata$change3, pldata$change4, pldata$changetot)
#View(test)
#class(pldata)


pldata <- mutate(pldata, changetot = ifelse(c(hgowner)>=2 & c(hgowner) <= 5 & c(lagowner) ==1, -2, changetot))
pldata <- mutate(pldata, changetot = ifelse(c(hgowner)==-1 & c(lagowner) ==1, -3, changetot))


#Erläuterung changetot
#-3: keine Angabe in hgowner oder L.hgowner
#-2: Wechsel von Eigentümer zu einem Mieterhältnis (2-5)
#-1: first wave
#0: entweder kein Wechsel, oder Wechsel innerhalb von versch. Mietverhältnissen
#1: Wechsel von einem Mietsverhältnis (2-5) zu Eigentum (1)

pldata <- mutate(pldata, failure = ifelse(c(changetot== 0), 0, ifelse(c(changetot)==1, 1, NA)))

test <- data.frame(pldata$change1, pldata$change2, pldata$change3, pldata$change4, pldata$changetot, pldata$failure)
View(test)


pldata <- pldata[order(pldata$syear, pldata$hid),]


#pldata$syear[[1]][1]


#Erstes Jahr der Beobachtung eines HH
pldata2 <- aggregate(as.numeric(as.character(syear)) ~ hid, pldata, function(x) min(x))
View(pldata2)
pldata = merge(pldata, pldata2, by.pldata = c("hid"), by.pldata2 = c("hid"))
names(pldata)[names(pldata) == "as.numeric(as.character(syear))"] <- "firstyear"

#Anzahl der beobachteten Jahre-------------------------------------------
#print(pldata$syear)
pldata3 <- aggregate(syear ~ hid, pldata, function(x) as.numeric(as.character(length(as.factor(x)))))
View(pldata3)
names(pldata3)[names(pldata3) == "syear"] <- "noofyears"
pldata = merge(pldata, pldata3, by.pldata = c("hid"), by.pldata3 = c("hid"))

#Anzahl der Eigentümerwechsel--------------------------------------------------------

pldata4 <- aggregate(changetot ~ hid, pldata, function(x) max(x))
names(pldata4)
names(pldata4)[names(pldata4) == "changetot"] <- "changeflag"
pldata = merge(pldata, pldata4, by.pldata = c("hid"), by.pldata4 = c("hid"))
names(pldata)


#generate birthyear and cohorts
pldata$syear = as.numeric(as.character(pldata$syear))
#class(pldata$syear)
pldata$birthyear <- pldata$syear -pldata$d11101

pldata <- mutate(pldata, cohort11 = ifelse(c(syear == 1985) & c(d11101 >= 24) & c(d11101) <= 27, 1, 0))
pldata <- mutate(pldata, cohort21 = ifelse(c(syear == 1990) & c(d11101 >= 24) & c(d11101) <= 27, 1, 0))
pldata <- mutate(pldata, cohort31 = ifelse(c(syear == 1995) & c(d11101 >= 24) & c(d11101) <= 27, 1, 0))


pldata <- mutate(pldata, cohort = ifelse(cohort11 == 1, 1, NA))
pldata <- mutate(pldata, cohort = ifelse(cohort21 == 1, 2, cohort))
pldata <- mutate(pldata, cohort = ifelse(cohort31 == 1, 3, cohort))

pldata <- mutate(pldata, cohort11 = ifelse((cohort > 0) & (cohort <= 3) & (cohort11 != 1), 0, cohort11))
pldata <- mutate(pldata, cohort21 = ifelse((cohort > 0) & (cohort <= 3) & (cohort21 != 1), 0, cohort21))
pldata <- mutate(pldata, cohort31 = ifelse((cohort > 0) & (cohort <= 3) & (cohort31 != 1), 0, cohort31))

save(pldata, file="Datensatz.RDA")
#_________________ANALYSE_____________________________-

#load("pequivnew.Rda")
View(pldata)

time <- pldata$timevar[pldata$timevar > 0]
event <- ifelse(pldata$changetot == 1, 1, 0)
X <- cbind(pldata$birthyear)
group <- pldata$cohort

kmsurvival <- survfit(Surv(time,event) ~ group)
summary(kmsurvival)
plot(kmsurvival, xlab = "Time", ylab = "Survival Probabiility", xlim=c(0,30))


