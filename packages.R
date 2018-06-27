# Preparing R -------------------------------------------------------------

# Packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  bigmemory,
  biganalytics,
  dplyr,
  plm,
  car,
  varhandle,
  foreign,
  survival,
  rms,
  survminer,
  randomForestSRC,
  ggRandomForests,
  pec,
  flexsurv,
  ggfortify,
  grid,
  gridExtra,
  stargazer,
  Hmisc,
  pastecs,
  rio,
  haven,
  VIM,
  naniar,
  ggridges,
  GGally,
  # those beneath always load last
  readr,
  ggplot2,
  tidyverse)


#options(scipen = 100)
options(digits = 2)



