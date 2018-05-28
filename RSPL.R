
source(".path.R")
source("packages.R")


#memory.limit()
#rm(list=ls())


#############################################################################################
####################  Data cleaning #########################################################
#############################################################################################

#subsetting pequiv takes too long on my pc
#I commented out this part and load pequivsmall right away

#information on individual (household head) characteristics from pequiv.dta

pequiv <- read.csv(paste(path, "pequiv.csv", sep="/"))

#declare as data table
pequiv = as.data.table(pequiv)

#restrict dataset to individuals aged 24 to 65
#restrict dataset to household heads due to availability of hgowner only on household level

pequivnew <- as.data.table(subset(pequiv, d11101>23 & d11101<66 & d11105 == 1))

pequivsmall <- select(pequivnew, one_of( pvar = c("pid" , "hid" , "syear" , "d11102ll" , "d11101" , 
                                                "d11104" , "d11108" , "d11109" ,  "e11106" , "i11101" , "i11103" , "w11102",
                                                "w11103" , "y11101" , "l11101" , "l11102" , "iself" , "ijob1" , "ijob2")))

save(pequivsmall, file="pequivsmall.RDA")

#rm(pequiv, pequivnew, pequivvariables)

load(file="pequivsmall.RDA")

#information on household renting/ownership status from hgen.dta

hgen <- read_dta("hgen.dta")

hgensmall <- select(hgen, one_of(hgenvariables = 
                                   c("cid" , "hid" , "syear", "hgacquis" , "hgowner" , "hgmoveyr", "hgrent")))

#save(hgensmall, file="hgensmall.RDA")
#View(hgensmall)

#rm(hgen)


##################################################
########## Merge Files ###########################

#need to remove attributes of identifying variables or otherwise dplyr::inner_join does not work

attributes(pequivsmall$syear) <-  NULL
attributes(pequivsmall$hid) <-  NULL 
attributes(hgensmall$hid) <-  NULL
attributes(hgensmall$hid) <-  NULL

#unname(pequivsmall$syear, force = TRUE)
#unname(pequivsmall$hid, force = TRUE)
#unname(hgensmall$syear, force = TRUE)
#unname(hgensmall$hid, force = TRUE)



data <- inner_join(pequivsmall, hgensmall, by = c("hid", "syear"))


#different merging approach:
merge <- merge(pequivsmall, hgensmall, by.pequivsmall = c("syear","hid"), by.hgensmall = c("syear", "hid"), all = FALSE)
View(merge)

rm(hgensmall, pequivsmall)


# Data cleaning on merged data set #######################################################

#Mark dissolved households (more than one PID per HID)

#compute max(pid)-min(pid) for each household
#if difference = 0 => household remains undissolved throughout observation period
dissolvedata <- aggregate(data$pid ~ data$hid, data , function(x) (max(x)-min(x))) 
names(dissolvedata)[names(dissolvedata) == "data$pid"] = "dissolved"
names(dissolvedata)[names(dissolvedata) == "data$hid"] <- "hid"
summary(dissolvedata$dissolved)
#View(dissolvedata)

attributes(dissolvedata$hid) <- NULL
attributes(data$hid) <- NULL
data <-  inner_join(data, dissolvedata, by = "hid")

#table(data$dissolved)
#View(data[,c("hid", "pid", "syear", "dissolved")])

#remove dissolved households from analysis
#obs go from 247k to 215k, which corresponds to what we expect
data <- subset(data, dissolved == 0)

rm(dissolvedata)


#mark households who acquired home ownership through inheritance/endowment

data$inherit <- 0
data$inherit[data$hgacquis == 2] <- 1
table(data$inherit)

#remove observations where inherit=1
#reduces data from 215k to 201k
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

#minage==25 reduces data from ~201k to <30k !

#keep only individuals that were surveyed starting before or at age 30
#reduces dataset from ~210k to 64k
#reducing to minage<=25 reduces dataset to 26k observations
data <- subset(data, minage <= 25)
#View(data)
save(data, file="data.RDA")
rm(list=ls())


# create indicators and time variables ########################################



load(file = "data.RDA")


# hgowner lagged variable 
setDT(data)[, laghgowner:= shift(hgowner), hid]
summary(data$laghgowner)
#NA for 3.2k out of 26k


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

#create last year of observation variable
lyears <- aggregate(syear ~ hid, data, function(x) max(x))
names(lyears)[names(lyears) == "syear"] <- "lastyear"
data = left_join(data, lyears, by = "hid")
rm(lyears)
table(data$lastyear)


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

data$time <- data$syear - (data$firstyear-1) #time since firstyear
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
data <- left_join(data, first.fail, by = "hid")
#households where no failure occurs are assigned NA
#rm(first.fail)

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
#rm(firstown.dat)

#re-count number of failures per hid
numev2.dat <- aggregate(failure ~ hid, data, function(x) sum(x))
names(numev2.dat)[names(numev2.dat) == "failure"] <- "new.numfails"
data = left_join(data, numev2.dat, by = "hid")
summary(data$new.numfails)
#rm(numev2.dat)

View(data[, c("hid", "syear", "hgowner" , "rent", "owner", "change", "failure", 
              "firstyear" , "lastyear", "failureflag", "failure2flag",  "time", "firstfailyear") ])


save(data, file="datalong.RDA")


# wide format ####################################################################

# create time-independenx covariates
# if covariate is constant over time, no issue
# for time-changing covariates, take value at syear=firstyear


firstvars <- subset(data, syear == firstyear)
firstvars <- firstvars[, c( "hid", "pid", "failureflag", "d11102ll", "d11104", "d11109", "e11106",
                            "i11101", "l11101", "l11102", "minage", "firstyear",
                            "lastyear", "numobs", "birthyear", "firstfailyear")]

#rename covariates
names(firstvars) <- c( "hid", "pid", "event", "gender", "married", "yearsedu", "sector",
                       "hhinc", "state", "region", "minage", "firstyear",
                       "lastyear", "numobs","birthyear", "firstfailyear")

#firstvars can be used as wide dataset with time-indep. covariates

#similar pattern of numobs holds in pequiv itself, too
#what should we do with households censored at t=0 (numobs=1)? leave in?
hist(firstvars$numobs)
summary(firstvars$numobs)
summary(firstvars$failureflag)


#sort data set by hid
firstvars <- firstvars[order(firstvars$hid),]

#create consecutive ID for observation units
firstvars$id <- seq(length(firstvars$firstyear))
id2 <- firstvars$id
firstvars <- cbind( id2, firstvars)
firstvars$id <- NULL
names(firstvars)[names(firstvars) == "id2"] <- "id"
#rm(id2)
head(firstvars)

#create time to event variable

dataw <- mutate(firstvars, 
                time = ifelse(firstvars$event==1, 
                              firstvars$firstfailyear- firstvars$firstyear +1,
                              firstvars$lastyear - firstvars$firstyear +1))
hist(dataw$time) #Alice: Funktioniert bei mir nicht 
dataw$minage <- NULL
dataw$numobs <- NULL
#rm(firstvars)

#put time variable in front

tvar <- dataw$time
other <- select(dataw, one_of(c("id", "pid", "hid")))
dataw$id <- NULL
dataw$pid <- NULL
dataw$hid <- NULL
dataw <- cbind(tvar, dataw)
dataw$time <- NULL
names(dataw)[names(dataw) == "tvar"] <- "time"
dataw <- cbind(other, dataw)
rm(other, tvar)


save(dataw, file="datawide.RDA")



# try out wide format survival object:

wide.fit <- survfit(Surv(time, event, type="right") ~ region, data=dataw)

kmcurve <- ggsurvplot(wide.fit, conf.int=T,
                      legend.labs=c("West", "East"), legend.title="Region",  
                      title="Kaplan-Meier Curve for Survival in Rent", 
                      censor=F,
                      palette = "strata",
                      risk.table = T,
                      pval=TRUE,
                      risk.table.height=.25,
                      ylim=c(0,1),
                      xlim=c(0,30),
                      surv.median.line="h",
                      linetype=c(1,1),
                      size = 0.5)
print(kmcurve)


#works fine

##################################################################################
#################### ANALYSIS ####################################################
##################################################################################

#TWO ways to create survival objects, as wide (time) or long (tstart, tstop)
#If long version is used, data is automatically assumed interval censored instead or right-censored
#Maybe need to convert to wide format to declare Surv(time,event)


######!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##########
#All of the following are simple KM curves using the long format for survival objects
#   => long format should not be used anymore, wide is now available (above) from $$dataw


#create minimal dataset to try out survival functions
minimal <- data[, c("hid", "syear", "tstart", "tstop", "time", "failure",
                    "failureflag", "l11102", "firstyear")]

minimal$tstart <- minimal$tstart - 2
minimal$tstop <- minimal$tstop - 2
#define survival object in long format version (interval censored)
min.fit <- survfit(Surv(tstart, tstop, failure) ~ l11102,
                   data=minimal)
summary(min.fit)

#define survival curve object
kmcurve <- ggsurvplot(min.fit, conf.int=T,
                      legend.labs=c("West", "East"), legend.title="Region",  
                      title="Kaplan-Meier Curve for Survival in Rent", 
                      censor=F,
                      palette = "strata",
                      risk.table = T,
                      risk.table.height=.25,
                      ylim=c(0.25,1),
                      xlim=c(0,30),
                      surv.median.line="h",
                      linetype=c(1,1),
                      size = 0.5)
#control line width through geom_step(size = 2)
#print and save survival curve
print(kmcurve)
ggsave(file = "kmcurve.pdf", print(kmcurve))

summary(min.fit)



glist <- list(
  ggsurvplot(min.fit, risk.table = F, main = "Survival Probability"),
  ggsurvplot(min.fit, fun = "event",  main = "Cumulative Proportion"),
  ggsurvplot(min.fit, fun = "cumhaz", main = "Cumulative Hazard")
)
arrange_ggsurvplots(glist, print = TRUE, ncol = 3, nrow = 1)


# cohort analysis#####################################################################

#define cohorts in long format version

#cohort comprising all 25-yo-renters observed from 1984 to 1986
minimal$c8486 <- 0
minimal$c8486[minimal$firstyear<=1986] <- 1
summary(minimal$c8486)

#cohort comprising all 25-yo-renters observed from 1994 to 1996
minimal$c9496 <- 0
minimal$c9496[minimal$firstyear<=1996 & minimal$firstyear>=1994] <- 1
summary(minimal$c9496)

#create cohort grouping variable
minimal$cohort <- NA
minimal$cohort[minimal$c8486 == 1] <- 1
minimal$cohort[minimal$c9496 == 1] <- 2
table(minimal$cohort)

#define survival object and strata
min.fit.coh <- survfit(Surv(tstart, tstop, failure) ~ cohort,
                       data=minimal)
summary(min.fit.coh)


kmcurve.coh <- ggsurvplot(min.fit.coh, conf.int=T,
                          legend.labs=c("84-86", "94-96"), legend.title="Cohort",  
                          title="Kaplan-Meier Curve for Survival in Rent", 
                          censor=F,
                          palette = "strata",
                          risk.table = T,
                          risk.table.height=.25,
                          ylim=c(0.2,1),
                          xlim=c(0,30),
                          surv.median.line="h",
                          linetype=c(1,1),
                          size = 0.5)
print(kmcurve.coh)

summary(min.fit.coh)

# aggregated graphs

#generate graphs, then aggregate them

survprob <- ggsurvplot(min.fit.coh, conf.int=T,
                       legend.labs=c("84-86", "94-96"), legend.title="Cohort",  
                       title="Kaplan-Meier Survival Probability", 
                       censor=F,
                       palette = "strata",
                       risk.table = F,
                       risk.table.height=.25,
                       ylim=c(0.2,1),
                       xlim=c(0,30),
                       surv.median.line="h",
                       linetype=c(1,1),
                       size = 0.5)
#print(survprob)

cumprop <- ggsurvplot(min.fit.coh, conf.int=T,
                      legend.labs=c("84-86", "94-96"), legend.title="Cohort",  
                      title="Kaplan-Meier Cumulative Proportion", 
                      main="Cumulative Proportion",
                      censor=F,
                      fun = "event",
                      palette = "strata",
                      risk.table = F,
                      risk.table.height=.25,
                      ylim=c(0,1),
                      xlim=c(0,30),
                      surv.median.line="h",
                      linetype=c(1,1),
                      size = 0.5)
#print(cumprop)


cumulhaz <- ggsurvplot(min.fit.coh, conf.int=T,
                       legend.labs=c("84-86", "94-96"), legend.title="Cohort",  
                       title="Kaplan-Meier Cumulative Hazard", 
                       main="Cumulative Hazard",
                       censor=F,
                       fun = "cumhaz",
                       palette = "strata",
                       risk.table = F,
                       risk.table.height=.25,
                       ylim=c(0,1.5),
                       xlim=c(0,30),
                       linetype=c(1,1),
                       size = 0.5)



glist <- list(survprob, cumprop, cumulhaz)
arrange_ggsurvplots(glist, print = TRUE, ncol = 3, nrow = 1)


# Lisa is genious 