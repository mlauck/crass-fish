### Jane S. Rogosch
### Created 19 March 2021
### This code is to extract USGS discharge data from gages in xeric ecoregion

### relevant links:
# Freshwater ecoregions: https://feow.org/

### Load libraries--------------------------------------------------------------
# Note: some of these libraries may not be necessary
# code copied from script to calculate Discrete Fast Fourier Transform 
# on mean daily discharge data  for installing packages and dependencies 
# originally from Metacommunity Project circa 2017,
# so some of these libraries may not be necessary
library(ggplot2)
library(lmom)
library(CircStats)

## Load discharge package
library(discharge)

# If you do not have the package you can download it 
# from here: https://sourceforge.net/projects/discharge/ (see documentation)
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
library(smwrBase)
# ?smwrBase
# ?waterYear

### Example code to extract one ------------------------------------------------------------------------------------------------------
# using San Pedro River, Arizona (USGS station 09471000)
# Now let's download data from USGS servers
siteNumber <-"09471000"
parameterCd <- "00060" # this is discharge
rawDailyData <-readNWISdv(siteNumber, parameterCd, "1996-10-01","2015-09-30")
head(rawDailyData)
rawDailyData <- rawDailyData[,3:4]
class(rawDailyData$Date) # needs to be class 'Date', if not run next line
#rawDailyData$Date<-as.Date(rawDailyData$Date, "%m/%d/%Y")
head(rawDailyData) # OK

# Convert raw data to 'asStreamflow' object
SANPEDRO <- asStreamflow(rawDailyData, river.name="San Pedro River, AZ")
?asStreamflow
dim(SANPEDRO$data)[1]
summary(SANPEDRO)

### To get gage site numbers and other info by state, modify URL to have correct state ID (e.g. UT)---------------
# https://waterservices.usgs.gov/nwis/iv?format=rdb&stateCd=UT&modifiedSince=PT30M
# data exported from each webpage to text file in Data>Gage_list_USA

# Open files
AZ_gages_long <- read.delim("Data/Gage_list_USA/AZ_gages.txt",  header = F) 
head(AZ_gages_long)

CA_gages_long <- read.delim("Data/Gage_list_USA/CA_gages.txt",  header = F) 
head(CA_gages_long)

CO_gages_long <- read.delim("Data/Gage_list_USA/CO_gages.txt",  header = F) 
head(CO_gages_long)

NM_gages_long <- read.delim("Data/Gage_list_USA/NM_gages.txt",  header = F) 
head(NM_gages_long)

NV_gages_long <- read.delim("Data/Gage_list_USA/NV_gages.txt",  header = F) 
head(NV_gages_long)

TX_gages_long <- read.delim("Data/Gage_list_USA/TX_gages.txt",  header = F) 
head(TX_gages_long)

UT_gages_long <- read.delim("Data/Gage_list_USA/UT_gages.txt",  header = F) 
head(UT_gages_long)


# Find lines with gage numbers
AZ_gages_long[15,] # 108 sites in this file
AZ_gages_long[16:123,]
AZ_gages_long[16:117,]

UT_gages_long[15,] # 76 sites in this file
UT_gages_long[16:92,]

# Get gage site numbers
AZ_siteNumbers <- substring(AZ_gages_long[16:117,], 11, 18)
UT_siteNumbers <- substring(UT_gages_long[16:86,], 11, 18)

# Get site info and lat/long for each gage
?dataRetrieval


# Get site data for USGS gages
siteNumbers <- c()
siteINFO <- readNWISsite(siteNumbers)


# Where to find Fish sample locations
# Xeric_Points_All.csv

# Get list of gages (siteNumbers) that are near a fish sampling location only

