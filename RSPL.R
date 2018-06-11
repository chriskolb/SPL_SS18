
#Set-up

rm(list=ls())

#setwd(path) in path.R
source(".path.R")

#install and load packages
source("packages.R")
source("library.R")


#############################################################################################
####################  Data cleaning #########################################################
#############################################################################################


#information on individual (household head) characteristics from pequiv.dta

pequiv <- import(paste(path, "pequiv.csv" , sep = "/"),setclass = "data.table")

attach(pequiv)

#restrict dataset to individuals aged 24 to 65
pequivnew <- subset(pequiv, d11101>23 & d11101<66)

#restrict dataset to household heads due to availability of hgowner only on household level

pequivnew <- subset(pequivnew, d11105 == 1)
pequivnew = as.data.table(pequivnew)


pequivvariables <- c("pid" , "hid" , "syear" , "d11102ll" , "d11101" , "d11104" , "d11108" , "d11109" ,  "e11106" , "i11101" , "i11103" , "w11102" , "w11103" , "y11101" , "l11101" , "l11102" , "iself" , "ijob1" , "ijob2")

pequivsmall <- select(pequivnew, one_of(pequivvariables))

save(pequivsmall, file="pequivsmall.RDA")

rm(pequiv, pequivnew, pequivvariables)

############################################################################################
#rm(list=ls())


#information on household renting/ownership status from hgen.dta

hgen <- import(paste(path, "hgen.csv" , sep = "/"),setclass = "data.table")


hgenvariables <- c("cid" , "hid" , "syear", "hgacquis" , "hgowner" , "hgmoveyr", "hgrent")

hgensmall <- select(hgen, one_of(hgenvariables))
hgensmall = as.data.table(hgensmall)


save(hgensmall, file="hgensmall.RDA")
#View(hgensmall)

rm(hgen, hgenvariables)


############################
############################
############################

#other data sets include interesting variables:
#ppfadl: germborn migback
#pl: plh0204 (willingness to take risks),plb0022 working status


###########################################################################################
#Additional variables from files with minor importance
#rm(list=ls())

#memory.limit(size=80000)  # Da der Datensatz Riesig ist, bisschen big data skills
pl <- import(paste(path, "pl.csv" , sep = "/"),setclass = "data.table")
#pllabel <- pl %>% map_chr(~attributes(.)$label)
#View(pllabel)
# Gibt z.B. noch Variablen bzgl. Erbe damit man da noch genauer Observations entfernen könnte, plb0022: employment Status: Restschuld Haus, Wohnung plc0411, plc0348 Eigentumsanteil Haus, Wohnung
plvariables <- c("cid" ,"pid", "hid" , "syear", "plb0022", "plh0204", "plc0411", "plc0348")

plsmall <- select(pl, one_of(plvariables))
plsmall = as.data.table(plsmall)

save(plsmall, file="plsmall.RDA")

rm(pl, plvariables)
#View(plsmall)

####################################biol#########################################
rm(list=ls())
biol <- read_csv("biol.csv") 
#biollabel <- biol %>% map_chr(~attributes(.)$label)
#View(biollabel)
#biol: ll0090 (edumom), ll0091 (edudad)
biolvariables <- c("cid" , "pid", "hid" , "syear", "ll0090", "ll0091")

biolsmall <- select(biol, one_of(biolvariables))
biolsmall = as.data.table(biolsmall)

save(biolsmall, file="biolsmall.RDA")


rm(biol, biolvariables)
#View(biolsmall)

#View(data)
rm(list=ls())

##########################################################################################
# Merge Files ########################################################################
load(file="pequivsmall.RDA")
load(file="hgensmall.RDA")
load(file="plsmall.RDA")
load(file="biolsmall.RDA")


#names(biolsmall)[names(biolsmall) == "hid.x"] <- "hid"

#View(plsmall)
#need to remove attributes of identifying variables or otherwise dplyr::inner_join does not work

attributes(pequivsmall$syear) <- NULL
attributes(pequivsmall$hid) <- NULL
attributes(hgensmall$syear) <- NULL
attributes(hgensmall$hid) <- NULL
attributes(plsmall$syear) <- NULL
attributes(plsmall$pid) <- NULL
attributes(biolsmall$syear) <- NULL
attributes(biolsmall$pid) <- NULL

#unname(pequivsmall$syear, force = TRUE)
#unname(pequivsmall$hid, force = TRUE)
#unname(hgensmall$syear, force = TRUE)
#unname(hgensmall$hid, force = TRUE)


data = inner_join(pequivsmall, hgensmall, by = c("hid", "syear"))

attributes(data$pid) <- NULL
#Hier Anwendung von Left Join, da keine Observationen von data verloren werden sollen.
#######################MERGING PL###################################################
data = left_join(data, plsmall, by = c("pid", "syear"))

names(data)
#Namen diversifiziert, da mehrere Daten Blätter
data$cid.y <- NULL
data$hid.y <- NULL

names(data)[names(data) == "hid.x"] <- "hid"
names(data)[names(data) == "cid.x"] <- "cid"
#######################MERGING BIOL###################################################

data = left_join(data, biolsmall, by = c("pid", "syear"))
#Namen diversifiziert, da mehrere Daten Blätter

data$cid.y <- NULL
data$hid.y <- NULL

names(data)[names(data) == "hid.x"] <- "hid"
names(data)[names(data) == "cid.x"] <- "cid"

#View(datatest)

rm(hgensmall, pequivsmall, plsmall, biolsmall)


data = as.data.table(data)

#anschliessenden Rechnungen müssen noch überarbeitet werden ( hinzufügen der neuen variablen, aber fügt gerne in die variablenlisten hinzu, was ihr gerne noch hättet)

# Data cleaning on merged data set #######################################################

#View(data)

#Mark dissolved households (more than one PID per HID)

#compute max(pid)-min(pid) for each household
#if difference = 0 => household remains undissolved throughout observation period
dissolvedata <- aggregate(data$pid ~ data$hid, data , function(x) (max(x)-min(x)))
names(dissolvedata)[names(dissolvedata) == "data$pid"] <- "dissolved"
names(dissolvedata)[names(dissolvedata) == "data$hid"] <- "hid"
summary(dissolvedata$dissolved)

attributes(dissolvedata$hid) <- NULL
attributes(data$hid) <- NULL

View(dissolvedata)
View(data)

data = inner_join(data, dissolvedata, by = c("hid"))

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
#befehle nacheinander ausf?hren funktioniert, aber ganzen minage code auf einmal nicht 
print("join minage to data")
data = left_join(data, minage.dat, by = "hid")
summary(data$minage)
rm(minage.dat)
#lapply(list(x, y, z), summary) <- summarize from stata

head(data[,c("hid", "syear", "d11101", "minage")])

#minage==25 reduces data from ~201k to <30k !

#keep only individuals that were surveyed starting before or at age 30
#reduces dataset from ~210k to 64k
#reducing to minage<=25 reduces dataset to 29k observations
data <- subset(data, minage <= 25)
#View(data)


#transform hh income to real hh income (in 2010 prices)
data <- mutate(data, i11101 = i11101/(y11101/100))
save(data, file="data.RDA")
#rm(list=ls())

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

#re-count number of failures per hid
numev2.dat <- aggregate(failure ~ hid, data, function(x) sum(x))
names(numev2.dat)[names(numev2.dat) == "failure"] <- "new.numfails"
data = left_join(data, numev2.dat, by = "hid")
summary(data$new.numfails)
rm(numev2.dat)

#View(data[, c("hid", "syear", "hgowner" , "rent", "owner", "change", "failure", "firstyear" , "lastyear", "failureflag", "failure2flag",  "time", "firstfailyear") ])
names(data)

save(data, file="datalong.RDA")

#View(data)

# Imputation ###########################################################################

# Multiple Imputation by Chained Equations
#with mice package


#or just use simple imputation, because only data on hhinc and yearsedu is missing
#=> maybe can simply take value from other year as much better imputation strategy


# If income value is NA take value of next year otherwise of the year after 

setDT(data)[, shiftincome:= lead(i11101), hid]
setDT(data)[, shift2income:= lead(shiftincome), hid]
data <- mutate(data, i11101impute = ifelse(data$i11101 <= 0, ifelse(data$shiftincome > 0, data$shiftincome, ifelse(data$shift2income> 0, data$shift2income, NA) ), data$i11101))
View(data[,c("hid", "pid", "syear", "i11101", "shiftincome", "shift2income", "i11101impute")])




# If education value is NA take value of next year otherwise of the year after 
setDT(data)[, shiftedu:= lead(d11109), hid]
setDT(data)[, shift2edu:= lead(shiftedu), hid]
data <- mutate(data, d11109impute = ifelse(data$d11109 <= 0, ifelse(data$shiftedu > 0, data$shiftedu, ifelse(data$shift2edu> 0, data$shift2edu, NA) ), data$d11109))


# As only the maximum amount of education is necessary, max impute can be generated
maxeducation <- aggregate(data$d11109 ~ data$hid, data , function(x) (max(x)))
names(maxeducation)[names(maxeducation) == "data$d11109"] <- "yearsedumaximputed"
names(maxeducation)[names(maxeducation) == "data$hid"] <- "hid"

data = inner_join(data, maxeducation, by = "hid")
#View(maxeducation)
View(data)
#View(data[,c("hid", "pid", "syear", "d11109", "shiftedu", "shift2edu", "d11109impute")])
#data$d11109impute <- NULL
#names(maxeducation)[names(maxeducation) == "data$hid"] <- "hid"

#Other possibility to be neglected to choose the mean of the income by hid
#data$hhincimputed <- ifelse(data$i11101 <= 0, aggregate(data$i11101 ~ hid, data, function(x) mean(x)), data$i11101)
#data <- mutate(data,  hhincimputed = ifelse(data$i11101 <= 0, aggregate(i11101 ~ hid, data, function(x) mean(x)), i11101) , i11101)
#View(data[,c("hid", "pid", "syear", "i11101", "lagincome")])
#########################################################################################
#########################################################################################



# wide format ####################################################################

# create time-independenx covariates
# if covariate is constant over time, no issue
# for time-changing covariates, take value at syear=firstyear


firstvars <- subset(data, syear == firstyear)
firstvars <- firstvars[, c( "hid", "pid", "failureflag", "d11102ll", "d11104", "d11109", "d11109impute", "yearsedumaximputed" , "e11106",
                            "i11101", "i11101impute" ,"l11101", "l11102", "minage", "firstyear",
                            "lastyear", "numobs", "birthyear", "firstfailyear")]

#rename covariates
names(firstvars) <- c( "hid", "pid", "event", "gender", "married", "yearsedu", "yearseduimpute", "yearsedumaximputed" , "sector",
                       "hhinc", "hhincimpuute", "state", "region", "minage", "firstyear",
                       "lastyear", "numobs","birthyear", "firstfailyear")

#View(firstvars)
# Looks good, a lot more values after imputation
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
rm(id2)
head(firstvars)

#create time to event variable

dataw <- mutate(firstvars, 
                time = ifelse(firstvars$event==1, 
                              firstvars$firstfailyear- firstvars$firstyear +1,
                              firstvars$lastyear - firstvars$firstyear +1))
hist(dataw$time)
dataw$minage <- NULL
dataw$numobs <- NULL
rm(firstvars)

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


#recoding of categorical variables in wide data set

#state of residnce
dataw$state <- factor(dataw$state, levels=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16), 
                      labels=c("Schleswig-Holstein", "Hamburg", "Lower Saxony", "Bremen", "NRW", "Hessia",
                               "Rhineland-Palatinate", "Baden-Wuerttemberg", "Bavaria", "Saarland",
                               "Berlin", "Brandenburg", "Mecklenburg-Vorpommern", "Saxony",
                               "Saxony-Anhalt", "Thuringia"))
#region
dataw$region <- factor(dataw$region, levels=c(1,2), labels=c("West", "East"))

#gender
dataw$gender <- factor(dataw$gender, levels=c(1,2), labels=c("Male", "Female"))

#married
dataw$married[dataw$married != 1] <- 0
dataw$married <- factor(dataw$married, levels=c(0,1), labels=c("not married", "married"))

#pre-government household income as numeric
dataw$hhinc <- as.numeric(dataw$hhinc)

#recoding missing data as NA

dataw$yearsedu[dataw$yearsedu<0] <- NA
dataw$hhinc[dataw$hhinc<0] <- NA
dataw$sector[dataw$sector<0] <- NA


####################################
save(dataw, file="datawide.RDA")
####################################

####################################DESCRIPTIVES#########################################
#########################################################################################


#distribution of time to failure by state

dist <- subset(dataw, event==1)
dist$yeargroup <- as.factor(dist$firstyear)
levels(dist$state)
hist(dist$time)

ggplot(
  dist, 
  aes(x = dist$time, y = dist$state, fill=dist$state)
) +
  geom_density_ridges_gradient(
    aes(fill = ..x..), scale = 2.5, size = 0.3
  ) +
  scale_fill_gradientn(
    colours = c("#0D0887FF", "#CC4678FF", "#F0F921FF"),
    name = "Duration"
  )+
  theme_ridges() +
  labs(title = 'Density of Time to Event', x = "Time", y = "States")


ggplot(dist, aes(x = time, y = state, fill = state)) + 
  geom_density_ridges(scale = 2.5) + theme_minimal() +
  scale_fill_cyclical(values = c("blue", "green")) +
  labs(title = 'Density of Time to Event', x = "Time", y = "States")

rm(dist)


# Draftmans Plot

pairplotcont <- dataw[, c("hhinc", "yearsedu", "time")]
pairplotdisc <- dataw[, c("gender", "event", "married", "region")]

pairs(pairplot, main = "Survival Data", pch = 21, bg = c("red", "blue")[unclass(dataw$region)])

ggpairs(pairplot, lower = list(continuous = "points", combo = "box", discrete="facetbar"))

ggpairs(pairplotdisc, lower = list(discrete="box"))

ggpairs(pairplotcont, lower = list(continuous = "smooth_loess"), diag = list(continuous = "density"))

rm(pairplotcont, pairplotdisc)

# Visualization of missing values in wide dataset #
# In case of income, a lot of zero values not defined as NA
dataw$hhinc[dataw$hhinc <= 0] <- NA

#few missings in final data set => no surprise b/c of generated variables
sapply(dataw, function(x) sum(is.na(x)))

aggr_plot <- aggr(dataw, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE,
                  labels=names(data), cex.axis=.7, gap=3, 
                  ylab=c("Histogram of missing data","Pattern"))
rm(aggr_plot)

gg_miss_fct(x = dataw, fct = firstyear)

#View(dataw)


##################################################################################
#################### ANALYSIS ####################################################
##################################################################################

#TWO ways to create survival objects, as wide (time) or long (tstart, tstop)
#If long version is used, data is automatically assumed interval censored instead or right-censored
#Therefore only use wide format (dataw) [some data cleaning was only done on dataw anyways]

# KM by region ####################################################################

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
                      surv.median.line="hv",
                      linetype=c(1,1),
                      size = 0.5)
print(kmcurve)

rm(kmcurve, wide.fit)


# KM by highinc/lowinc ###########################################################

medinc <- median(as.numeric(dataw$hhinc), na.rm=TRUE)
dataw <- mutate(dataw, highinc = ifelse(dataw$hhinc > medinc, 1, 0))
summary(dataw$highinc)
#define survival object and fit KM estimator
inc.fit <- survfit(Surv(time, event, type="right") ~ highinc, data=dataw)
km.inc <- ggsurvplot(inc.fit, conf.int=T,
                     legend.labs=c("Low", "High"), legend.title="HH Inc.",  
                     title="Kaplan-Meier Curve for Survival in Rent", 
                     censor=F,
                     palette = "strata",
                     risk.table = T,
                     pval=TRUE,
                     risk.table.height=.25,
                     ylim=c(0,1),
                     xlim=c(0,30),
                     surv.median.line="hv",
                     linetype=c(1,1),
                     size = 0.5)
print(km.inc)

rm(km.inc, inc.fit, medinc)

# KM by higedu/lowedu ###########################################################

mededu <- median(dataw$yearsedu, na.rm=TRUE)
dataw <- mutate(dataw, highedu = ifelse(dataw$yearsedu > mededu, 1, 0))
summary(dataw$highedu)
#define survival object and fit KM estimator
edu.fit <- survfit(Surv(time, event, type="right") ~ highedu, data=dataw)
km.edu <- ggsurvplot(edu.fit, conf.int=T,
                     legend.labs=c("Low", "High"), legend.title="Years Educ.",  
                     title="Kaplan-Meier Curve for Survival in Rent", 
                     censor=F,
                     palette = "strata",
                     risk.table = T,
                     pval=TRUE,
                     risk.table.height=.25,
                     ylim=c(0,1),
                     xlim=c(0,30),
                     surv.median.line="hv",
                     linetype=c(1,1),
                     size = 0.5)
print(km.edu)

rm(km.edu, edu.fit, mededu)

# KM by cohorts 84-87 and 94-97 #########################################################

dataw <- mutate(dataw, cohort8494 = ifelse (dataw$firstyear<=1987, 1,ifelse(dataw$firstyear>=1994 & dataw$firstyear<=1997, 2, NA)))
summary(dataw$cohort8494)
table(dataw$cohort8494)
#define survival object and fit KM estimator
coh.fit <- survfit(Surv(time, event, type="right") ~ cohort8494, data=dataw)
km.coh <- ggsurvplot(coh.fit, conf.int=T,
                     legend.labs=c("84-87", "94-97"), legend.title="Strata",  
                     title="Kaplan-Meier Curve for Survival in Rent", 
                     censor=F,
                     palette = "strata",
                     risk.table = T,
                     pval=TRUE,
                     risk.table.height=.25,
                     ylim=c(0,1),
                     xlim=c(0,30),
                     surv.median.line="hv",
                     linetype=c(1,1),
                     size = 0.5)
print(km.coh)

rm(km.coh, coh.fit)



#cohorts 84-87 and 04-07

dataw <- mutate(dataw, cohort8404 = ifelse
                (dataw$firstyear>=1984 & dataw$firstyear<=1987, 1,
                  ifelse(dataw$firstyear>=2004 & dataw$firstyear<=2007, 2, NA)))
summary(dataw$cohort8404)
table(dataw$cohort8404)
#define survival object and fit KM estimator
coh.fit2 <- survfit(Surv(time, event, type="right") ~ cohort8404, data=dataw)
km.coh2 <- ggsurvplot(coh.fit2, conf.int=T,
                      legend.labs=c("84-87", "94-97"), legend.title="Strata",  
                      title="Kaplan-Meier Curve for Survival in Rent", 
                      censor=F,
                      palette = "strata",
                      risk.table = T,
                      pval=TRUE,
                      risk.table.height=.25,
                      ylim=c(0,1),
                      xlim=c(0,30),
                      surv.median.line="hv",
                      linetype=c(1,1),
                      size = 0.5)
print(km.coh2)

rm(km.coh2, coh.fit2)




# cox proportional hazard regression #################################################

#survival/rms to estimate models, survminer package for plots and diagnostics

#define survival object
coxsurv <- Surv(dataw$time, dataw$event, type="right")
plot(coxsurv)

#Cox PH model

#using survival package
cox.ph <- coxph(coxsurv ~ yearsedu + hhinc + gender + region + married, data=dataw)

#using rms package
cox.ph2 <- cph(coxsurv ~ yearsedu + hhinc + gender + region + strat(married), data=dataw,
               na.rm=FALSE, y=TRUE, x=TRUE)

print(cox.ph)
print(cox.ph2)

#Schoenfeld test of cox ph assumption
coxtest <- cox.zph(cox.ph)
coxtest
#Schoenfeld graphical test of cox ph assumption
ggcoxzph(coxtest)


#adjusted (for covariates) survival curves from cox model

#by highinc
medinc <- median(dataw$hhinc, na.rm=TRUE)
dataw <- mutate(dataw, highinc = ifelse(dataw$hhinc > medinc, 2, 1))
summary(dataw$highinc)
rm(medinc)

ggadjustedcurves(cox.ph, data = dataw, variable = "highinc")


#by firstyear

dataw$firstyear <- as.integer(dataw$firstyear)
cu <- c(1984, 1990, 2000, 2010, 2015)
dataw$yearcut <- cut(dataw$firstyear, cu, dig.lab = min(nchar(cu)))
ggadjustedcurves(cox.ph, data = dataw, variable="yearcut")
rm(cu)

rm(cox.ph, cox.ph2, coxtest, coxsurv)

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




