
##################################################################################
##################################################################################
#################### Descriptives ################################################
##################################################################################
##################################################################################

# 2 do:
# graphical representation of data set (see rpubs paper) (done)
# descriptives table (done)
# survival time density plot (done)
# missing data plots before and after imputation (done)
# distributions and densities (by state plots, draftmans plot, countour/3d)

##################################################################################
# summary table ##################################################################
##################################################################################

#recode factors as numeric for them to be included in table

load("datfinal.RDA")

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
# data structure visualization ###################################################
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
# density plot of survival time ##################################################
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
##################################################################################



# distribution of time to failure by state

dist <- subset(dat, event==1)
dist$yeargroup <- as.factor(dist$firstyear)
levels(dist$state)
hist(dist$time)

ggplot(
  dist, 
  aes(x = dist$time, y = dist$state, fill=dist$state)
) +
  geom_density_ridges_gradient(
    aes(fill = ..x..), scale = 3, size = 0.3
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

# distribution of hhinc by State

dist <- subset(dat, hhinc<90000)
dist$yeargroup <- as.factor(dist$firstyear)
levels(dist$state)
hist(dist$hhinc)

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


ggplot(dist, aes(x = hhinc, y = state, fill = state)) + 
  geom_density_ridges(scale = 2.5) + theme_minimal() +
  scale_fill_cyclical(values = c("blue", "green")) +
  labs(title = 'Density of Household Income', x = "Household Income", y = "States")

rm(dist)



# Distribution of educational attainment by State


dist <- subset(dat, event==1)
dist$yeargroup <- as.factor(dist$firstyear)
levels(dist$state)
hist(dist$maxedu)

ggplot(
  dist, 
  aes(x = dist$maxedu, y = dist$state, fill=dist$state)
) +
  geom_density_ridges_gradient(
    aes(fill = ..x..), scale = 2.5, size = 0.3
  ) +
  scale_fill_gradientn(
    colours = c("#0D0887FF", "#CC4678FF", "#F0F921FF"),
    name = "Duration"
  )+
  theme_ridges() +
  labs(title = 'Density of Years of eduation', x = "Education", y = "States")


ggplot(dist, aes(x = maxedu, y = state, fill = state)) + 
  geom_density_ridges(scale = 2.5) + theme_minimal() +
  scale_fill_cyclical(values = c("blue", "green")) +
  labs(title = 'Density of Years of Education', x = "Education", y = "States")

rm(dist)




# area + contour 2d density
ggplot(dat, aes(x=maxedu, y=hhinc) ) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", colour="white",
                  n=100, h=NULL)



# Draftmans Plot

pairplotcont <- dat[, c("hhinc", "maxedu", "time")]
pairplotdisc <- dat[, c("gender", "event", "married", "region")]

pairplot <- dat[, c("time", "event", "hhinc", "maxedu", "migback", "rural")]

pairs(pairplotcont, main = "Survival Data", pch = 21, bg = c("red", "blue")[unclass(dat$region)])

ggpairs(pairplot, lower = list(continuous = "points", combo = "box", discrete="facetbar"))

ggpairs(pairplotdisc, lower = list(discrete="box"))

ggpairs(pairplotcont, lower = list(continuous = "smooth_loess"), diag = list(continuous = "density"))


rm(pairplotcont, pairplotdisc)

# Visualization of missing values in wide dataset #
# In case of income, a lot of zero values not defined as NA
dat$hhinc[dat$hhinc < 0] <- NA

#few missings in final data set => no surprise b/c of generated variables


sapply(dat, function(x) sum(is.na(x)))

aggr_plot <- aggr(dat, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE,
                  labels=names(dat), cex.axis=.7, gap=3, 
                  ylab=c("Histogram of missing data","Pattern"))
rm(aggr_plot)

gg_miss_fct(x = dat, fct = firstyear)

#View(dat)
