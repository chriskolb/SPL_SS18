##########################################################################################
##########################################################################################
####################  Data Wrangling  ####################################################
##########################################################################################
##########################################################################################


#Note1: All scripts and SOEP data files need to be in the same directory
#Note2: .path.R file needs to be specified by the user

#Structure:
#0: Set-up
#1: Prepare individual files for merging
#2: Merge individual files
#3: Data clearning on merged file
#4: Create variables (covariates, time/status variable)
#5: First imputation (using forward values)
#6: Change to wide format data set
#7: Recoding of categorical variables
#8: Second imputation (using random forests)
#9: Final save


##########################################################################################
#### Set Up ###############################################################################
##########################################################################################


#clear workspace
rm(list=ls())

#setwd(path) in path.R
source("path.R")

#install and load packages
source("packages.R")


#First, merge relevant SOEP files into one data set

##########################################################################################
###### Prepare individual files for merging ##############################################
##########################################################################################

#### pequiv ####

#information on individual (household head) characteristics from pequiv.csv
pequiv <- import(paste(path, "pequiv.csv" , sep = "/"), setclass = "data.table")

#restrict dataset to individuals aged 24 to 65
pequivnew <- subset(pequiv, d11101>23 & d11101<66)

#restrict dataset to household heads due to availability of hgowner only on household level
pequivnew <- subset(pequivnew, d11105 == 1)
pequivnew = as.data.table(pequivnew)


pequivvariables <- c("pid" , "hid" , "syear" , "d11102ll" , "d11101" ,
                     "d11104" , "d11108" , "d11109" ,  "e11106" , "i11101" ,
                     "i11103" , "w11102" , "w11103" , "y11101" , "l11101" ,
                     "l11102" , "iself" , "ijob1" , "ijob2")

pequivsmall <- select(pequivnew, one_of(pequivvariables))

save(pequivsmall, file="pequivsmall.RDA")

rm(pequiv, pequivnew, pequivvariables, pequivsmall)

#### hgen #####

#information on household renting/ownership status from hgen.csv
hgen <- import(paste(path, "hgen.csv" , sep = "/"),setclass = "data.table")


hgenvariables <- c("hid" , "syear", "hgacquis" , "hgowner" , "hgmoveyr", "hgrent")

hgensmall <- select(hgen, one_of(hgenvariables))
hgensmall = as.data.table(hgensmall)


save(hgensmall, file="hgensmall.RDA")
#View(hgensmall)

rm(hgen, hgenvariables, hgensmall )

#### pgen ######

#information on individual education from pgen.csv (ISCED Classification)
pgen <- import(paste(path, "pgen.csv" , sep = "/"),setclass = "data.table")


pgenvariables <- c("pid" , "syear", "pgisced97")

pgensmall <- select(pgen, one_of(pgenvariables))
pgensmall = as.data.table(pgensmall)


save(pgensmall, file="pgensmall.RDA")
#View(hgensmall)

rm(pgen, pgenvariables, pgensmall )


#### ppfadl #######

#information on migration background from pffad.csv
ppfadl <- import(paste(path, "ppfadl.csv" , sep = "/"),setclass = "data.table")

# migration background: migback
ppfadlvariables <- c("pid", "syear", "migback")

ppfadlsmall <- select(ppfadl, one_of(ppfadlvariables))
ppfadlsmall = as.data.table(ppfadlsmall)

save(ppfadlsmall, file="ppfadlsmall.RDA")


rm(ppfadl, ppfadlvariables, ppfadlsmall)


##### hbrutto #######

#information on spatial category - Urban / Rural
hbrutto <- import(paste(path, "hbrutto.csv" , sep = "/"),setclass = "data.table")

#hbrutto: regtyp
hbruttovariables <- c("hid", "syear", "regtyp")

hbruttosmall <- select(hbrutto, one_of(hbruttovariables))
hbruttosmall = as.data.table(hbruttosmall)

save(hbruttosmall, file="hbruttosmall.RDA")


rm(hbrutto, hbruttovariables, hbruttosmall)

##########################################################################################
###### Merge invidividual Files ##########################################################
##########################################################################################

load(file="pequivsmall.RDA")
load(file="hgensmall.RDA")
load(file="ppfadlsmall.RDA")
load(file ="hbruttosmall.RDA")
load(file="pgensmall.RDA")


data = left_join(pequivsmall, hgensmall, by = c("hid", "syear"))
data = left_join(data, hbruttosmall, by = c("hid", "syear"))
data = left_join(data, ppfadlsmall, by = c("pid", "syear"))
data = left_join(data, pgensmall, by = c("pid", "syear"))


rm(hgensmall, pequivsmall, ppfadlsmall, hbruttosmall, pgensmall)


data = as.data.table(data)

##########################################################################################
##### Data cleaning on merged data set ###################################################
##########################################################################################

# mark dissolved households (more than one PID per HID)

# compute max(pid)-min(pid) for each household
# if difference = 0 => household remains undissolved throughout observation period
dissolvedata <- aggregate(data$pid ~ data$hid, data , function(x) (max(x)-min(x)))
names(dissolvedata)[names(dissolvedata) == "data$pid"] <- "dissolved"
names(dissolvedata)[names(dissolvedata) == "data$hid"] <- "hid"

data = left_join(data, dissolvedata, by = c("hid"))

# remove dissolved households from analysis
# obs go from 260k to 230k, which corresponds to what we expect
data <- subset(data, dissolved == 0)

rm(dissolvedata)


# mark households who acquired home ownership through inheritance/endowment
data$inherit <- 0
data$inherit[data$hgacquis == 2] <- 1
table(data$inherit)

# remove observations where inherit=1
# reduces data from 230k to 216k
data <- subset(data, inherit == 0)

# age at first observation (minage)
minage.dat <- aggregate(data$d11101 ~ data$hid, data , function(x) min(x))

# rename variables in minage.dat so that merge later works
names(minage.dat)[names(minage.dat) == "data$d11101"] <- "minage"
names(minage.dat)[names(minage.dat) == "data$hid"] <- "hid"

# merge minage variable to dataset
data = left_join(data, minage.dat, by = "hid")
summary(data$minage)
rm(minage.dat)

head(data[,c("hid", "syear", "d11101", "minage")])


# reduces dataset from ~210k to 64k
# reducing to minage<=25 reduces dataset to 29k observations
data <- subset(data, minage <= 25)

##########################################################################################
#### create status and time variables and covariates #####################################
##########################################################################################


# the change variable indicates type of change in homeownership from last year to current year

# hgowner lagged variable 
setDT(data)[, laghgowner:= shift(hgowner), hid]

# indicator for renting in current period
data$rent <- 0
data$rent[data$hgowner>=2 & data$hgowner<=5] <-1

# indicator for home ownership in current period
data$owner <- 0 
data$owner[data$hgowner == 1] <- 1


#L.rent
data$lrent <- 0
data$lrent[data$laghgowner>=2 & data$laghgowner<=5] <- 1
data$lrent[is.na(data$laghgowner)] <- NA


# L.owner
data$lowner <- 0
data$lowner[data$laghgowner == 1] <- 1
data$lowner[is.na(data$laghgowner)] <- NA


# create change variable
# coding: -2 = owning to renter, -1 = first obs of individual, 0 = no change, 1 = renter to owner
data$change <- NA
data$change[(data$lowner == 1 & data$owner == 1) | (data$lrent == 1 & data$rent == 1)] <- 0
data$change[data$lrent == 1 & data$owner == 1] <- 1
data$change[is.na(data$lrent)] <- -1
data$change[data$lowner == 1 & data$rent == 1] <- -2
table(data$change)
unique(data$change)

# create failure flag
# failure marks change from renter to owner during syear-1 to syear
data$failure <- 0
data$failure[data$change == 1] <- 1



# create 'first year of observation' variable
fyears <- aggregate(syear ~ hid, data, function(x) min(x))
names(fyears)[names(fyears) == "syear"] <- "firstyear"
data = left_join(data, fyears, by = "hid")
rm(fyears)
table(data$firstyear)

# create last year of observation variable
lyears <- aggregate(syear ~ hid, data, function(x) max(x))
names(lyears)[names(lyears) == "syear"] <- "lastyear"
data = left_join(data, lyears, by = "hid")
rm(lyears)
table(data$lastyear)


# create number of observations per household (numbobs) variable
numobs.dat <- aggregate(syear ~ hid, data, function(x) length(x))
summary(numobs.dat$syear)
names(numobs.dat)[names(numobs.dat) == "syear"] <- "numobs"
data = left_join(data, numobs.dat, by = "hid")
rm(numobs.dat)
summary(data$numobs)



# mark all households where failure occurs at some point
failure.dat <- aggregate(change ~ hid, data, function(x) max(x))
names(failure.dat)[names(failure.dat) == "change"] <- "failureflag"
data = left_join(data, failure.dat, by = "hid")
data$failureflag[data$failureflag != 1] <- 0
summary(data$failureflag)
table(data$failureflag)
rm(failure.dat)


# mark all households where ownership failure (change= -1) occurs at some point
failure2.dat <- aggregate(change ~ hid, data, function(x) min(x))
names(failure2.dat)[names(failure2.dat) == "change"] <- "failure2flag"
data = left_join(data, failure2.dat, by = "hid")
# recode as dummy variable
data$failure2flag[data$failure2flag == -2] <- 1
data$failure2flag[data$failure2flag == -1] <- 0
rm(failure2.dat)

# create birthyear variable
data$birthyear <- data$syear -data$d11101
summary(data$birthyear)

data$time <- data$syear - (data$firstyear-1) #time since firstyear
data$tstart <- data$time - 1
data$tstop <- data$time

summary(data$time)
summary(data$tstart)
summary(data$tstop)
#View(data[,c("hid", "syear", "firstyear", "time", "tstart", "tstop", "failure")])

# count number of failures per hid
numev.dat <- aggregate(failure ~ hid, data, function(x) sum(x))
names(numev.dat)[names(numev.dat) == "failure"] <- "numfails"
data = left_join(data, numev.dat, by = "hid")
summary(data$numfails)
rm(numev.dat)

# subset observations to include only those before and up to first failure per hid
# example: 0 0 0 0 1 1 1 => 0 0 0 0 1 and 0 0 1 0 1 => 0 0 1
# create syear at first fail for each household where failure occurs
data <- as.data.table(data)
first.fail <- data[failure == 1, .SD[1], by = hid]
first.fail <- first.fail[, c("hid", "syear", "time", "tstart", "tstop" )]
names(first.fail)[names(first.fail) == "syear"] <- "firstfailyear"
names(first.fail)[names(first.fail) == "time"] <- "firstfailtime"
names(first.fail)[names(first.fail) == "tstart"] <- "firstfailtstart"
names(first.fail)[names(first.fail) == "tstop"] <- "firstfailtstop"
data = left_join(data, first.fail, by = "hid")
# households where no failure occurs are assigned NA
rm(first.fail)

# subset data  to only include syear<=firstfailyear and censored units
data1 <- subset(data, syear <= firstfailyear)
data2 <- subset(data, is.na(firstfailyear))
data <- rbind(data1, data2)
rm(data1, data2)

# subset data to only include units that start out as renters, not as owners
firstown.dat <- subset(data, syear == firstyear & owner==1)
firstown.dat$firstownflag <- 1
firstown.dat <- firstown.dat[, c("hid", "firstownflag")]
data = left_join(data, firstown.dat, by = "hid")
data <- subset(data, is.na(firstownflag))
rm(firstown.dat)

# re-count number of failures per hid
numev2.dat <- aggregate(failure ~ hid, data, function(x) sum(x))
names(numev2.dat)[names(numev2.dat) == "failure"] <- "new.numfails"
data = left_join(data, numev2.dat, by = "hid")
summary(data$new.numfails)
rm(numev2.dat)

#ever divorced

data <- data %>% 
  mutate(divorced = ifelse(d11104==4,1,0)) %>% 
  group_by(hid) %>% 
  mutate(ever_div = max(divorced)) %>% 
  ungroup() %>% 
  select(-divorced)

# transform hh income to real hh income (in 2010 prices)
data <- mutate(data, i11101 = i11101/(y11101/100))

# transform hh income to hh income in 1000s
data <- mutate(data, i11101 = i11101/1000)

names(data)

###########################################################################################
##### First Imputation by using forward (next year) values ################################
###########################################################################################

# First Imputation: 
# Impute missing values (at first year) from other years if available


#### Impute pre government HH income #####

# If income value is NA take value of next year otherwise of the year after 

summary(data$i11101)
data$i11101[data$i11101 < 0] <- NA


setDT(data)[, shiftincome:= lead(i11101), hid]
setDT(data)[, shift2income:= lead(shiftincome), hid]
data <- mutate(data, hhincimp = ifelse(is.na(data$i11101),
                                       ifelse(is.na(data$shiftincome), 
                                              ifelse(is.na(data$shift2income), NA , data$shift2income), data$shiftincome ), data$i11101))
lapply(list(data$i11101, data$hhincimp, data$shiftincome, data$shift2income), summary) 


###### Impute education #########

# isced97 classification has fewer missings than years of education variable
# split isced97 in 4 homogenous groups
# 0 = elementary education, 1 = medium education, 2 = higher vocational, 3 = high education

data <- data %>% 
  mutate(educ = ifelse(pgisced97>=0 & pgisced97 <3,0, 
                       ifelse(pgisced97==3,1,
                              ifelse(pgisced97>3 & pgisced97<6,2,
                                                    ifelse(pgisced97==6,3,NA))))) 

table(data$educ)

# maximum years of education is superior measure and has fewer missings
maxedu.dat <- aggregate(d11109 ~ hid, data, function(x) max(x))
names(maxedu.dat)[names(maxedu.dat) == "d11109"] <- "maxedu"
data = left_join(data, maxedu.dat, by = "hid")
rm(maxedu.dat)


data$maxedu[data$maxedu == -2] <- 0
data$maxedu[data$maxedu == -1] <- NA

data$d11109[data$d11109 == -2] <- 0
data$d11109[data$d11109 == -1] <- NA

lapply(list(data$d11109, data$maxedu, data$educ), summary) 

##########################################################################################
##### Switch to Wide Format Data Set #####################################################
##########################################################################################

# create time-invariant covariates
# if covariate is constant over time, no issue
# for time-changing covariates, take value at first year of observation or first-imputed value


dataw <- subset(data, syear == firstyear)
dataw <- as.data.frame(dataw)

# subset only selevant variables
datawvars <- c("hid", "pid", "failureflag", "d11102ll", "d11104", "educ", "ever_div", "d11109" , "e11106",
                "i11101" ,"l11101", "l11102", "minage", "firstyear",
                "lastyear", "birthyear", "firstfailyear", "migback", "maxedu", "hhincimp", "regtyp")

dataw <- select(dataw, one_of(datawvars))

rm(datawvars)

# rename variables
names(dataw) <- c( "hid", "pid", "event", "gender", "married", "educ", "ever_div", "yearsedu", "sector",
                    "hhinc2", "state", "region", "minage", "firstyear",
                    "lastyear", "birthyear", "firstfailyear", "migback", "maxedu", "hhinc", "rural")



# sort data set by hid
dataw <- dataw[order(dataw$hid),]

# create consecutive ID for observation units
dataw$id <- seq(length(dataw$firstyear))
id2 <- dataw$id
dataw <- cbind( id2, dataw)
dataw$id <- NULL
names(dataw)[names(dataw) == "id2"] <- "id"
rm(id2)
head(dataw)

# create time to event variable
dataw <- mutate(dataw, 
                 time = ifelse(dataw$event==1, 
                               dataw$firstfailyear- dataw$firstyear +1,
                               dataw$lastyear - dataw$firstyear +1))
# delete old identifiers               
dataw$hid <- NULL
dataw$pid <- NULL


# put time variable in front
tvar <- dataw$time
pnr <- dataw$id
dataw$id <- NULL
dataw <- cbind(tvar, dataw)
dataw$time <- NULL
names(dataw)[names(dataw) == "tvar"] <- "time"
dataw <- cbind(pnr, dataw)
rm(pnr, tvar)


##########################################################################################
##### Recoding of categorical variables  #################################################
##########################################################################################

# recoding of categorical variables in wide data set

# state of residnce
dataw$state <- factor(dataw$state, levels=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16), 
                       labels=c("Schleswig-Holstein", "Hamburg", "Lower Saxony", "Bremen", "NRW", "Hessia",
                                "Rhineland-Palatinate", "Baden-Wuerttemberg", "Bavaria", "Saarland",
                                "Berlin", "Brandenburg", "Mecklenburg-Vorpommern", "Saxony",
                                "Saxony-Anhalt", "Thuringia"))
# region
dataw$region <- factor(dataw$region, levels=c(1,2), labels=c("West", "East"))

# gender
# 0 = male, 1 = female
dataw <- mutate(dataw, gender = ifelse(dataw$gender==1,0,1))

# married
dataw$married[dataw$married != 1] <- 0

# pre-government household income as numeric
dataw$hhinc <- as.numeric(dataw$hhinc)

# recoding missing data in sector as NA
dataw$sector[dataw$sector<0] <- NA

#migration background
names(dataw)[names(dataw)=="migback"] <- "mig2"
#must be set as dataframe otherwise columns not unique
dataw <- mutate(dataw, migback = ifelse(dataw$mig2>1,1,0))

#rural/urban indicator 
dataw$rural[dataw$rural<0] <- NA
dataw <- mutate(dataw, rural = ifelse(dataw$rural==2,1,0))


# recording of object class
# needed for imputation algorithm to recognize categorical variables
dataw$gender <- as.factor(dataw$gender)
dataw$married <- as.factor(dataw$married)
dataw$ever_div <- as.factor(dataw$ever_div)
dataw$sector <- as.factor(dataw$sector)
dataw$mig2 <- as.factor(dataw$mig2)
dataw$rural <- as.factor(dataw$rural)
dataw$migback <- as.factor(dataw$migback)
dataw$educ <- as.factor(dataw$educ)


##########################################################################################
#### Second Imputation using random forests ################################################
##########################################################################################

gg_miss_fct(x = dataw, fct = firstyear)


# randomForestSRC package
# implement non-parametric random forest imputation
# Tang and Ishwaran (2017) https://arxiv.org/pdf/1701.05305.pdf


# choose on-the-fly imputation using unsupervised multivariate node splitting
# default nimpute is 2

# firstfailyear should not be imputed
dataw$firstfailyear[is.na(dataw$firstfailyear)] <- -1

dat <- impute(data = dataw, nimpute = 3)


# set firstfailyear back to NA
dat$firstfailyear[dat$firstfailyear == -1 ] <- NA


gg_miss_fct(x = dat, fct = firstyear)


##########################################################################################
##### Final Save #########################################################################
##########################################################################################

save(dat, file="datfinal.RDA")




