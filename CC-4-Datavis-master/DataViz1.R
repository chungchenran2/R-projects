# Purpose of the script
# Your name, date and email

# Your working directory, set to the folder you just downloaded from Github, e.g.:
setwd("/Users/chungchenran/Documents/Google_Certificate/Coding_Club_R_Tutorial2/CC-4-Datavis-master")

# Libraries - if you haven't installed them before, run the code install.packages("package_name")
library(tidyr)
library(dplyr)
library(ggplot2)
library(readr)
library(gridExtra)

# Import data from the Living Planet Index - population trends of vertebrate species from 1970 to 2014
LPI <- read.csv("LPIdata_CC.csv")
