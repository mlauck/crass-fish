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

CA_gages_long[15,] # 369 sites 
CA_gages_long[16:385,]
CA_gages_long[16:304,]

CO_gages_long[15,] # 213 sites 
CO_gages_long[16:229,]
CO_gages_long[16:184,]

NM_gages_long[15,] # 97 sites 
NM_gages_long[16:113,]
NM_gages_long[19:103,]

NV_gages_long[15,] # 97 sites
NV_gages_long[16:113,]
NV_gages_long[16:97,]

TX_gages_long[15, ] # 571 sites
TX_gages_long[16:587,]
TX_gages_long[16:539,]

UT_gages_long[15,] # 76 sites in this file
UT_gages_long[16:92,]

# Get gage site numbers
AZ_siteNumbers <- substring(AZ_gages_long[16:117,], 11, 18)
CA_siteNumbers <- substring(CA_gages_long[16:304,], 11, 18)
CO_siteNumbers <- substring(CO_gages_long[16:184,], 11, 18)
NM_siteNumbers <- substring(NM_gages_long[19:103,], 11, 18)
NV_siteNumbers <- substring(NV_gages_long[16:97,], 11, 18)
TX_siteNumbers <- substring(TX_gages_long[16:539,], 11, 18)
UT_siteNumbers <- substring(UT_gages_long[16:86,], 11, 18)


# Fix Texas site numbers
TX_siteNumbers[c(332,333,340,341,345,347,518)]
TX_gages_long[c(332,333,340,341,345,347,518)+15,]
TX_siteNumbers_b <- substring(TX_gages_long[c(332,333,340,341,345,347,518)+15,], 12, 19)
TX_siteNumbers[c(332,333,340,341,345,347,518)] <- replace(TX_siteNumbers[c(332,333,340,341,345,347,518)], 
                                                      1:7,TX_siteNumbers_b)

# Get site info and lat/long for each gage
siteINFO_state <- readNWISsite(TX_siteNumbers) #AZ, CA, CO, NM, NV, UT are ok

USA_siteNumbers <- c(AZ_siteNumbers, CA_siteNumbers, CO_siteNumbers, NM_siteNumbers, 
                     NV_siteNumbers, TX_siteNumbers, UT_siteNumbers)

siteINFO <- readNWISsite(USA_siteNumbers)

# Find what data is available
dailyDataAvailable <- whatNWISdata(siteNumber = USA_siteNumbers, service = "dv", statCd="00003") 
  # duplicates for sites with site numbers that are longer than 8 characters plus if there are multiple parameters
  # want columns dec_lat_va and dec_long_va

# Get specific info from gages
# Common USGS Parameter Codes
# 00060 is discharge (cubic ft per sec)
# 00010 is temperature
# 00400 is pH
library(dplyr)
gageLocation <- distinct(dailyDataAvailable, x = site_no, .keep_all = TRUE)

# Get list of gages (siteNumbers) that are near (xx km) a fish sampling location only

# Fish sample locations
USA_fish <- read.csv("Data/Xeric_Points_All.csv") # datum is WGSM 84
head(USA_fish)
