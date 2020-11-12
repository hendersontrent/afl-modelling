#------------------------------------------
# This script sets out to load all things
# required for the project
#------------------------------------------

#------------------------------------------
# Author: Trent Henderson, 11 November 2020
#------------------------------------------

# Load packages

library(tidyverse)
library(broom)
library(janitor)
library(fitzRoy)
library(scales)
library(Cairo)
library(ggpubr)
library(randomForest)
library(caret)
library(sjPlot)
library(caTools)
library(rstan)
library(bayesplot)

# Turn off scientific notation

options(scipen = 999)

# Define a plotting theme to be used throughout the project

my_theme <- theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "#189AB4"),
        strip.text = element_text(colour = "white"))

# Create an output folder if none exists:

if(!dir.exists('output')) dir.create('output')