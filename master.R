
##########################################################################################
##########################################################################################
####################  Master  ############################################################
##########################################################################################
##########################################################################################

#Note1: All scripts and SOEP data files need to be in the same directory
#Note2: path.R file needs to be specified by the user 

rm(list=ls())

# Load path to SOEP data (runtime <1sec)
source("path.R")

# Install and load packages (runtime <1sec)
source("packages.R")

# Wrangle the data (runtime 337sec)
# This script produces datfinal.RDA from SOEP .csv files
source("datawrangling.R")

# Load written functions (runtime <1sec)
source("functions.R")

# Load descriptives (runtime 11sec)
source("descriptives.R")

# Load analysis (runtime 58sec)
source("analysis.R")


