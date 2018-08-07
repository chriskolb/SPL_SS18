# Preparing R -------------------------------------------------------------

# Packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  bigmemory,
  biganalytics,
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
  broom,
  grid,
  gridExtra,
  stargazer,
  reporttools,
  forestplot,
  Hmisc,
  pastecs,
  rio,
  haven,
  VIM,
  naniar,
  ggridges,
  GGally,
  # those beneath always load last
  dplyr,
  readr,
  ggplot2,
  tidyverse)


#options(scipen = 100)
options(digits = 2)



