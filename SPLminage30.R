#############################################################################################
####################  Set-up ################################################################
#############################################################################################

#set path
path <- "C:\\Users\\Chris\\Dropbox\\HU Berlin\\Statistik\\2. Semester\\SPL\\Daten"
setwd(path)

#install packages
#install.packages("bigmemory",dependencies=TRUE)
#install.packages("biganalytics",dependencies=TRUE)
#install.packages("dplyr",dependencies=TRUE)
#install.packages("plm")
#nstall.packages("varhandle")
#install.packages("survival")
#install.packages("rms")
#install.packages("survminer")
#install.packages("pastecs") #summary stats
#install.packages("Hmisc") #summary stats 
#install.packages(c("rgl", "car"))#scatter3d()
#install.packages("haven")

# load packages 
library(foreign) 
library(car) # scatter3d()
library(Hmisc) # describe(mydata)
library(varhandle)
library(pastecs) #stat.desc(mydata)
library(rms)
library(survminer)
library(plm)
library(data.table)
library(dplyr)
library(bigmemory)
library(Matrix)
library(stats)
library(survival)
library(bitops)
library(haven)
library(data.table)
library(ggplot2)
library(plyr)
library(dplyr)
library(haven)
library(tidyr)
#memory.limit()
rm(list=ls())




#############################################################################################
####################  Data cleaning #########################################################
#############################################################################################

#information on individual (household head) characteristics from pequiv.dta

pequiv <- read_dta("pequiv.dta")

#declare as data table
pequiv = as.data.table(pequiv)

#restrict dataset to individuals aged 24 to 65
pequivnew <- subset(pequiv, d11101>23 & d11101<66)

#restrict dataset to household heads due to availability of hgowner only on household level

pequivnew <- subset(pequivnew, d11105 == 1)
#pequivnew = as.data.table(pequivnew)


pequivvariables <- c("pid" , "hid" , "syear" , "d11102ll" , "d11101" , "d11104" , "d11108" , "d11109" ,  "e11106" , "i11101" , "i11103" , "w11102" , "w11103" , "y11101" , "l11101" , "l11102" , "iself" , "ijob1" , "ijob2")

pequivsmall <- select(pequivnew, one_of(pequivvariables))

save(pequivsmall, file="pequivsmall.RDA")

rm(pequiv, pequivnew, pequivvariables)

#information on household renting/ownership status from hgen.dta

hgen <- read_dta("hgen.dta")

hgenvariables <- c("cid" , "hid" , "syear", "hgacquis" , "hgowner" , "hgmoveyr", "hgrent")

hgensmall <- select(hgen, one_of(hgenvariables))

save(hgensmall, file="hgensmall.RDA")
#View(hgensmall)

rm(hgen, hgenvariables)



# Merge Files ########################################################################

#need to remove attributes of identifying variables or otherwise dplyr::inner_join does not work

attributes(pequivsmall$syear) <- NULL
attributes(pequivsmall$hid) <- NULL
attributes(hgensmall$syear) <- NULL
attributes(hgensmall$hid) <- NULL

#unname(pequivsmall$syear, force = TRUE)
#unname(pequivsmall$hid, force = TRUE)
#unname(hgensmall$syear, force = TRUE)
#unname(hgensmall$hid, force = TRUE)



data = inner_join(pequivsmall, hgensmall, by = c("hid", "syear"))


#different merging approach:
#secondmerge = merge(pequivsmall, hgensmall, by.pequivsmall = c("syear","hid"), by.hgensmall = c("syear", "hid"), all = FALSE)
#View(secondmerge)

rm(hgensmall, pequivsmall)


# Data cleaning on merged data set #######################################################

#Mark dissolved households (more than one PID per HID)

#compute max(pid)-min(pid) for each household
#if difference = 0 => household remains undissolved throughout observation period
dissolvedata <- aggregate(data$pid ~ data$hid, data , function(x) (max(x)-min(x)))
names(dissolvedata)[names(dissolvedata) == "data$pid"] <- "dissolved"
names(dissolvedata)[names(dissolvedata) == "data$hid"] <- "hid"
summary(dissolvedata$dissolved)
#View(dissolveddata)

attributes(dissolvedata$hid) <- NULL
attributes(data$hid) <- NULL
data = inner_join(data, dissolvedata, by = "hid")

#table(data$dissolved)
#View(data[,c("hid", "pid", "syear", "dissolved")])

#remove dissolved households from analysis
#obs go from 247k to 217k, which corresponds to what we expect
data <- subset(data, dissolved == 0)

data$dissolved.x <- NULL
data$dissolved.x.x <- NULL
data$dissolved.y <- NULL
data$dissolved.y.y <- NULL
rm(dissolvedata)


#mark households who acquired home ownership through inheritance/endowment

data$inherit <- 0
data$inherit[data$hgacquis == 2] <- 1
table(data$inherit)

#remove observations where inherit=1
#reduces data from 64k to 62k
data <- subset(data, inherit == 0)

#age at first observation (minage)

minage.dat <- aggregate(data$d11101 ~ data$hid, data , function(x) min(x))

#rename variables in minage.dat so that merge later works
names(minage.dat)[names(minage.dat) == "data$d11101"] <- "minage"
names(minage.dat)[names(minage.dat) == "data$hid"] <- "hid"

#merge minage variable to dataset
#print befehl muss rein (oder irgendein anderer)  weil merge ansonsten nicht funktioniert
#befehle nacheinander ausführen funktioniert, aber ganzen minage code auf einmal nicht 
print("join minage to data")
data = left_join(data, minage.dat, by = "hid")
summary(data$minage)
rm(minage.dat)
#lapply(list(x, y, z), summary) <- summarize from stata

head(data[,c("hid", "syear", "d11101", "minage")])

#minage==25 reduces data from ~215k to 27k !

#keep only individuals that were surveyed starting before or at age 30
#reduces dataset from ~215k to 64k
data <- subset(data, data$minage  <= 30)
#View(data)
save(data, file="data.RDA")
rm(list=ls())



# create indicators and time variables ########################################



load(file = "data.RDA")
#pldata <- pdata.frame(data, index =c("hid", "syear"))
#View(pldata)
#save(pldata, file="pldata.RDA")


# hgowner lagged variable 
setDT(data)[, laghgowner:= shift(hgowner), hid]
summary(data$laghgowner)
#NA for 8k out of 62k


# change variable indicates type of change in homeownership from last year to current year

#indicator for renting in current period
data$rent <- 0
data$rent[data$hgowner>=2 & data$hgowner<=5] <-1
summary(data$rent)

#indicator for home ownership in current period
data$owner <- 0 
data$owner[data$hgowner == 1] <- 1
summary(data$owner)

#L.rent
data$lrent <- 0
data$lrent[data$laghgowner>=2 & data$laghgowner<=5] <- 1
data$lrent[is.na(data$laghgowner)] <- NA
summary(data$lrent)

#L.owner
data$lowner <- 0
data$lowner[data$laghgowner == 1] <- 1
data$lowner[is.na(data$laghgowner)] <- NA
summary(data$lowner)

#create change variable
#coding: -2 = owning to renter, -1 = first obs of individual, 0 = no change, 1 = renter to owner
data$change <- NA
data$change[(data$lowner == 1 & data$owner == 1) | (data$lrent == 1 & data$rent == 1)] <- 0
data$change[data$lrent == 1 & data$owner == 1] <- 1
data$change[is.na(data$lrent)] <- -1
data$change[data$lowner == 1 & data$rent == 1] <- -2
summary(data$change)
table(data$change)
unique(data$change)

#create failure flag
#failure marks change from renter to owner during syear-1 to syear
data$failure <- 0
data$failure[data$change == 1] <- 1
summary(data$failure)


#create first year of observation variable
fyears <- aggregate(syear ~ hid, data, function(x) min(x))
names(fyears)[names(fyears) == "syear"] <- "firstyear"
data = left_join(data, fyears, by = "hid")
rm(fyears)
table(data$firstyear)



#create number of observations per household (numbobs)  variable
numobs.dat <- aggregate(syear ~ hid, data, function(x) length(x))
summary(numobs.dat$syear)
names(numobs.dat)[names(numobs.dat) == "syear"] <- "numobs"
data = left_join(data, numobs.dat, by = "hid")
rm(numobs.dat)
summary(data$numobs)



#mark all households where failure occurs at some point
failure.dat <- aggregate(change ~ hid, data, function(x) max(x))
names(failure.dat)[names(failure.dat) == "change"] <- "failureflag"
data = left_join(data, failure.dat, by = "hid")
data$failureflag[data$failureflag != 1] <- 0
summary(data$failureflag)
table(data$failureflag)
rm(failure.dat)


#mark all households where ownerhip failure (change= -1) occurs at some point
failure2.dat <- aggregate(change ~ hid, data, function(x) min(x))
names(failure2.dat)[names(failure2.dat) == "change"] <- "failure2flag"
data = left_join(data, failure2.dat, by = "hid")
#recode as dummy variable
data$failure2flag[data$failure2flag == -2] <- 1
data$failure2flag[data$failure2flag == -1] <- 0
rm(failure2.dat)

#significant portion of failing households go back to rent at some point (4k out of 22k)
table(data$failureflag, data$failure2flag)

#View(data[,c("hid", "syear", "change", "hgowner", "owner", "rent", "failure", "failureflag", "failure2flag")])

#create birthyear variable
data$birthyear <- data$syear -data$d11101
summary(data$birthyear)

#SIDE NOTE:
#Surv can take two versions, second is with tstart and tstop in long format
data$time <- data$syear - (data$firstyear-1)
data$tstart <- data$time - 1
data$tstop <- data$time

summary(data$time)
summary(data$tstart)
summary(data$tstop)
#View(data[,c("hid", "syear", "firstyear", "time", "tstart", "tstop", "failure")])

#count number of failures per hid
numev.dat <- aggregate(failure ~ hid, data, function(x) sum(x))
names(numev.dat)[names(numev.dat) == "failure"] <- "numfails"
data = left_join(data, numev.dat, by = "hid")
summary(data$numfails)
rm(numev.dat)

#subset observations to include only those before and up to first failure per hid
#example: 0 0 0 0 1 1 1 => 0 0 0 0 1 and 0 0 1 0 1 => 0 0 1
#create syear at first fail for each household where failure occurs
data <- as.data.table(data)
first.fail <- data[failure == 1, .SD[1], by = hid]
first.fail <- first.fail[, c("hid", "syear", "time", "tstart", "tstop" )]
names(first.fail)[names(first.fail) == "syear"] <- "firstfailyear"
names(first.fail)[names(first.fail) == "time"] <- "firstfailtime"
names(first.fail)[names(first.fail) == "tstart"] <- "firstfailtstart"
names(first.fail)[names(first.fail) == "tstop"] <- "firstfailtstop"
data = left_join(data, first.fail, by = "hid")
#households where no failure occurs are assigned NA
rm(first.fail)

#subset data  to only include syear<=firstfailyear and censored units
data1 <- subset(data, syear <= firstfailyear)
data2 <- subset(data, is.na(firstfailyear))
data <- rbind(data1, data2)
rm(data1, data2)

#subset data to only include units that start out as renters, not as owners
firstown.dat <- subset(data, syear == firstyear & owner==1)
firstown.dat$firstownflag <- 1
firstown.dat <- firstown.dat[, c("hid", "firstownflag")]
data = left_join(data, firstown.dat, by = "hid")
data <- subset(data, is.na(firstownflag))
rm(firstown.dat)

View(data[, c("hid", "syear", "hgowner" , "rent", "owner", "change", "failure", "firstyear" , "failureflag", "failure2flag",  "time", "firstfailyear") ])


save(data, file="datalong.RDA")

##################################################################################
#################### ANALYSIS ####################################################
##################################################################################

#TWO ways to create survival objects, as wide (time) or long (tstart, tstop)
#If long version is used, data is automatically assumed interval censored instead or right-censored
#Maybe need to convert to wide format to declare Surv(time,event)

#create minimal dataset to try out survival functions
minimal <- data[, c("hid", "syear", "tstart", "tstop", "time", "failure", "failureflag", "d11102ll")]

#define survival object in long format version (interval censored)
min.fit <- survfit(Surv(minimal$tstart, minimal$tstop, minimal$failure) ~ d11102ll,
                   data=minimal)
#summary(min.fit)

#define survival curve object
kmcurve <- ggsurvplot(min.fit, conf.int=F,
                      legend.labs=c("West", "East"), legend.title="Region",  
                      palette=c("dodgerblue2", "orchid2"), 
                      title="Kaplan-Meier Curve for Survival in Rent", 
                      censor=F,
                      risk.table = T,
                      risk.table.height=.3,
                      ylim=c(0.0,1),
                      xlim=c(0,30),
                      surv.median.line="hv",
                      linetype=c(1,1))
#print and save survival curve
print(kmcurve)
ggsave(file = "kmcurve.pdf", print(kmcurve))

#summary(min.fit)



glist <- list(
  ggsurvplot(min.fit, risk.table = F, main = "Survival Probability"),
  ggsurvplot(min.fit, fun = "event",  main = "Cumulative Proportion"),
  ggsurvplot(min.fit, fun = "cumhaz", main = "Cumulative Hazard")
)
arrange_ggsurvplots(glist, print = TRUE, ncol = 3, nrow = 1)




###############################################################################
######## Richies Kram noch: vll paar nice bausteine für cohortenindikatoren####
###############################################################################

pldata <- mutate(pldata, cohort11 = ifelse((c(syear == 1985) & c(d11101 < 30)), 1, 0))
cohortdata1 <- aggregate(cohort11 ~ hid, pldata, function(x) max(x))
names(cohortdata1)[names(cohortdata1) == "cohort11"] <- "cohort1"
pldata = merge(pldata, cohortdata1, by.pldata = c("hid"), by.cohortdata1 = c("hid"), all = T)




pldata <- mutate(pldata, cohort21 = ifelse((c(syear == 1990) & c(d11101 == 25)), 1, 0))
cohortdata2 <- aggregate(cohort21 ~ hid, pldata, function(x) max(x))
names(cohortdata2)[names(cohortdata2) == "cohort21"] <- "cohort2"
pldata = merge(pldata, cohortdata2, by.pldata = c("hid"), by.cohortdata2 = c("hid"), all = T)


pldata <- mutate(pldata, cohort31 = ifelse((c(syear == 1995) & c(d11101 == 25)), 1, 0))
cohortdata3 <- aggregate(cohort31 ~ hid, pldata, function(x) max(x))
names(cohortdata3)[names(cohortdata3) == "cohort31"] <- "cohort3"
pldata = merge(pldata, cohortdata3, by.pldata = c("hid"), by.cohortdata3 = c("hid"), all = T)


#test <- data.frame(pldata$hid, pldata$syear, pldata$firstyear, pldata$d11101, pldata$cohort11, pldata$cohort1, pldata$cohort21, pldata$timevar)
#test <- test[order(pldata$hid, pldata$syear),]
#View(test)


pldata <- mutate(pldata, cohort = ifelse(cohort1 == 1, 1, NA))
pldata <- mutate(pldata, cohort = ifelse(cohort2 == 1, 2, cohort))
pldata <- mutate(pldata, cohort = ifelse(cohort3 == 1, 3, cohort))

pldata <- mutate(pldata, cohort1 = ifelse(((cohort > 0) & (cohort <= 3) & (cohort != 1)), 0, cohort1))
pldata <- mutate(pldata, cohort2 = ifelse(((cohort > 0) & (cohort <= 3) & (cohort != 1)), 0, cohort2))
pldata <- mutate(pldata, cohort3 = ifelse(((cohort > 0) & (cohort <= 3) & (cohort != 1)), 0, cohort3))

test <- data.frame(pldata$hid, pldata$cohort1, pldata$cohort2, pldata$cohort3, pldata$cohort, pldata$syear, pldata$minage, pldata$firstyear)
test <- test[order(pldata$hid, pldata$syear),]
#View(test)