
##########################################################################################
##########################################################################################
####################  Master  ############################################################
##########################################################################################
##########################################################################################

#Note1: All scripts and SOEP data files need to be in the same directory
#Note2: .path.R file needs to be specified by the user

rm(list=ls())

# load path to SOEP data
source(".path.R")

# install and load packages
source("packages.R")

# load all written functions 
source("functions.R")

# Wrangle the data
source("datawrangling.R")

# load descriptives 
source("descriptives.R")

# load analysis
source("analysis.R")


