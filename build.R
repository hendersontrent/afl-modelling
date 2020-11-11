#---------------------------------------------------------
# This script drives the key aspects of the project for
# replication and error checks
#
# NOTE: THIS MAY TAKE A SUBSTANTIAL PERIOD OF TIME TO RUN
#---------------------------------------------------------

#------------------------------------------
# Author: Trent Henderson, 11 November 2020
#------------------------------------------

# Load function which runs all scripts in a given folder

source("setup.R")

#------------------------- MODELLING BUILD -------------------------

# Run all scripts in the folder

scripts <- list.files("modelling", pattern = "\\.[Rr]$", full.names = TRUE)

for(s in scripts){
  source(s)
}

