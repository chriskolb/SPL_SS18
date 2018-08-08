
##################################################################################
##################################################################################
#################### Descriptives ################################################
##################################################################################
##################################################################################

#Note1: All scripts and SOEP data files need to be in the same directory
#Note2: .path.R file needs to be specified by the user

#Structure:
#0: Set-up
#1: Summary table
#2: Data structure visualization
#3: Density plot of survival times
#4: Density plots by federal states
#5: Missings plot (in datawrangling.R)


##########################################################################################
####Set Up ###############################################################################
##########################################################################################


#clear workspace
rm(list=ls())

#setwd(path) in path.R
source(".path.R")

#install and load packages
source("packages.R")

load("datfinal.RDA")


##################################################################################
#### summary table ###############################################################
##################################################################################

#recode factors as numeric for them to be included in table

sum.dat <- dat

sum.dat$gender <- as.integer(as.character(sum.dat$gender))
sum.dat$married <- as.integer(as.character(sum.dat$married))
sum.dat$ever_div <- as.integer(as.character(sum.dat$ever_div))
sum.dat$region <- as.integer(sum.dat$region)-1
sum.dat$rural <- as.integer(as.character(sum.dat$rural))
sum.dat$migback <- as.integer(as.character(sum.dat$migback))

sum.dat <- select(sum.dat,
                  one_of(c("time", "event", "hhinc", "maxedu", "birthyear",
                           "gender", "region", "rural", "married", "ever_div",
                           "migback")) )

stargazer(sum.dat, type="latex",
          summary.stat=c("mean", "sd", "min",
                         "median", "max", "n"),
          title = "Summary Statistics",
          covariate.labels = c("Time to Event", "Event", "HH Income",
                               "Educ. (years)", "Birthyear", "Gender",
                               "East Germany", "Rural", "Married", 
                               "Ever Divorced", "Migr. Background"),
          digits=2,
          summary.logical = TRUE)
# need to add column w/ variable names (hhinc, maxedu etc.) manually in latex doc

rm(sum.dat)


##################################################################################
#### Data structure visualization ################################################
##################################################################################


# permutate persID to get rid of pnr (=syear) dependencies in surv. time
dat.str <- dat
dat.str$pnr <- sample(1:nrow(dat), nrow(dat), replace=F)

ggplot(dat.str, aes(x = pnr, y = time)) +
  geom_linerange(aes(ymin = 0, ymax = time), size=0.3) +
  geom_point(aes(shape = as.factor(event), color = as.factor(event)), stroke = 0.5, cex = 1) +
  scale_shape_manual(values = c(3,4)) + guides(shape = F, color = F) +
  labs(y = "Time (years)", x = "Subject ID") + coord_flip() + theme_classic()

# plot too crowded, look only at subsample of 500

dat.str1 <- subset(dat.str, pnr<500)

ggplot(dat.str1, aes(x = pnr, y = time)) +
  geom_linerange(aes(ymin = 0, ymax = time), size=0.5) +
  geom_point(aes(shape = as.factor(event), color = as.factor(event)), stroke = 1.3, cex = 2) +
  scale_shape_manual(values = c(1,4))  + labs(color="Event") + guides(shape=F) +
  labs(y = "Time (years)", x = "Subject ID") + coord_flip() + theme_classic()

##################################################################################
#### Density plot of survival time ###############################################
##################################################################################

dens.dat <- subset(dat, event==1)

ggplot(dens.dat, aes(x=time)) + 
  geom_histogram(binwidth = 0.2, aes(fill = ..count..) ) + theme_classic() +
  labs(y = "Count", x = "Time (years)")

hist(dens.dat$time, breaks = 15, freq = F, xlab = 'Time', ylim = c(0, 0.2), ylab = 'Relative Frequency', main = 'Histogram of Survival Times')
lines(density(dens.dat$time, na.rm = T, from = 0, to = 30))

hist(dens.dat$time, # histogram
     col="Dodger Blue", # column color
     border="black",
     prob = TRUE, # show densities instead of frequencies
     xlab = "Time",
     ylab = "Relative Frequency",
     xlim = c(0, 30), 
     ylim = c(0, 0.28),
     breaks=30,
     main = "Survival Time Density")
lines(density(dens.dat$time, from = 0, to = 30), # density plot
      lwd = 2, # thickness of line
      col = "red")


##################################################################################
### Density plots by federal states ##############################################
##################################################################################

#### distribution of time to event by state ###
dist <- subset(dat, event==1)
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


rm(dist)

#### distribution of hhinc by State ###
dist <- subset(dat, hhinc<90000)
dist$yeargroup <- as.factor(dist$firstyear)
levels(dist$state)
hist(dist$hhinc)

# gradient color style
ggplot(
  dist, 
  aes(x = dist$hhinc, y = dist$state, fill=dist$region)
) +
  geom_density_ridges_gradient(
    aes(fill = ..x..), scale = 2.5, size = 0.3
  ) +
  scale_fill_gradientn(
    colours = c("#0D0887FF", "#CC4678FF", "#F0F921FF"),
    name = "Duration"
  )+
  theme_ridges() +
  labs(title = 'Density of Household Income ', x = "Household Income", y = "States")

# alternating color style
ggplot(dist, aes(x = hhinc, y = state, fill = state)) + 
  geom_density_ridges(scale = 2.5) + theme_minimal() +
  scale_fill_cyclical(values = c("blue", "green")) +
  labs(title = 'Density of Household Income', x = "Household Income", y = "States")

rm(dist)



##################################################################################
### Missings plot ################################################################
##################################################################################

# created in datawrangling.R script, because it needs to be created before RF imputation
