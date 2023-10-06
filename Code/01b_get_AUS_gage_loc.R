### Jane S. Rogosch
### Created 08 Oct 2021
### This code is to extract USGS discharge data from gages in xeric ecoregion

### relevant links:
# Freshwater ecoregions: https://feow.org/


##################################################################################
#### STEPS TO TAKE - WORK PLAN ###
##################################################################################
# 1. Get AUS gage data to be similar to USA gage data
# 2. Get gages coordinates near fish surveys 
# 3. Exploratory analysis in trends.

###################################################################################################################
### LOAD LIBRARIES ###
####################################################################################################################
### Relevant discharge libraries -----------------------------------------------------------------------
# Use to look at Weighted regressions on times and discharge in streams to look at temporal variations
# According to Freya this library with wrtds function separates signal from noise to get good idea of trends
# Data hungry - needs long trend
library(EGRET)
  #function wrtds

## Load discharge package for NAA and variance metrics
library(discharge)

# If you do not have the package you can download it 
# from here: https://sourceforge.net/projects/discharge/ (see documentation)

## To get USGS gage data in USA
library(dataRetrieval)
### NOTE: cannot have dplyr library loaded, because also has a filter function

##USGS code to get water year
# rprofile_path = file.path(Sys.getenv("HOME"), ".Rprofile")
# write('\noptions(repos=c(getOption(\'repos\'),
#       CRAN=\'https://cloud.r-project.org\',
#       USGS=\'https://owi.usgs.gov/R\'))\n',
#       rprofile_path,
#       append =  TRUE)
# 
# # cat('Your Rprofile has been updated to include GRAN.
# #     Please restart R for changes to take effect.')
#  install.packages("smwrBase")
## Functions like getting water year
library(smwrBase)

### Other libraries--------------------------------------------------------------
# Note: some of these libraries may not be necessary
# code copied from script to calculate Discrete Fast Fourier Transform 
# on mean daily discharge data  for installing packages and dependencies 
# originally from another project circa 2017,

library(ggplot2)
library(lmom)
library(CircStats)



test <- read.table("Data/GRDC_data/5712100_Q_Day.Cmd")
head(test)
test2 <- read.csv("Data/GRDC_data/5712100_Q_Day.Cmd")
test3 <- read.csv("Data/GRDC_data/5110010_Q_Day.Cmd.txt")
head(test2)
# Get Gage locations from the .cmd files
# This is how the USGS gage locations were extracted
# ?grep
# data[, grep("_va", names(data))] <- sapply(data[, grep("_va", 
#                                                        names(data))], as.numeric)

t.string <- test3[c(12:13),]


substr(t.string,24,33) # Ok this works, so can make a loopty loo

# Read in all Australia gage files from the GRDC_data folder
# Example code: Adjust so that it takes the ones from "Data/GRDC_data" NOT entire working directory

test3b <- read.delim("Data/GRDC_data/Australia/5101020_Q_Day.Cmd.txt")
head(test3b); head(test3) #looks the same, hopefully behaves the same
substr(test3b[c(12:13),], 24,33)
test3
# starting to edit code
temp <- list.files(path="Data/GRDC_data/Australia/", pattern="_Q_Day.Cmd.txt")
AUS_gage_files <- lapply(paste0("Data/GRDC_data/Australia/",temp), read.delim)
str(AUS_gage_files)

# Loop or try lapply
test_list <- lapply(AUS_gage_files, function(x) substr(x[c(8,12:13), ], 24,33))
test_list
length(test_list)
# read.csv("Data/GRDC_data/5405030_Q_Day.Cmd.txt") making sure is same values, even though short

# Get from list to dataframe
test.df <- data.frame(matrix(unlist(test_list), nrow=length(test_list), byrow=TRUE),
                      stringsAsFactors=FALSE)
colnames(test.df) <- c("site_no", "dec_lat_va", "dec_long_va")
test.df
write.csv(test.df, file = "Output/Gage_location_AUS.csv")
