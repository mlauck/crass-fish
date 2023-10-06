### Jane S. Rogosch
### Created 19 March 2021
### This code is to extract list of USGS gages in xeric ecoregion and get locations
citation("base")
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
#citation("discharge")


# If you do not have the package you can download it 
# from here: https://sourceforge.net/projects/discharge/ (see documentation)
library(dataRetrieval)
#citation("dataRetrieval")
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

dim(SANPEDRO$data)[1]
summary(SANPEDRO)

### To get gage site numbers and other info by state, modify URL to have correct state ID (e.g. UT)---------------
# HOW TO USE SERVICE: 
# site service: https://nwis.waterservices.usgs.gov/rest/Site-Service.html
# instantaneous values service: https://nwis.waterservices.usgs.gov/rest/IV-Service.html
## https://waterservices.usgs.gov/nwis/iv?format=rdb&stateCd=UT&modifiedSince=PT30M # I'm not deleting this but FML this URL didn't catch all of the gages
# OH i think this is gages updated within the last 30 minutes?
# https://waterservices.usgs.gov/nwis/iv?format=rdb&stateCd=UT
# &parameterCd=00060,00010 # to get discharge and temperature
# e.g. https://waterservices.usgs.gov/nwis/iv?format=rdb&stateCd=UT&parameterCd=00060,00010
# data exported from each webpage to text file in Data>Gage_list_USA

# Open files
AZ_gages_long <- read.delim("Data/Gage_list_USA/AZ_gages_updated.txt",  header = F) 
head(AZ_gages_long)

CA_gages_long <- read.delim("Data/Gage_list_USA/CA_gages_updated.txt",  header = F) 
head(CA_gages_long)

CO_gages_long <- read.delim("Data/Gage_list_USA/CO_gages_updated.txt",  header = F) 
head(CO_gages_long)

NM_gages_long <- read.delim("Data/Gage_list_USA/NM_gages_updated.txt",  header = F) 
head(NM_gages_long)

NV_gages_long <- read.delim("Data/Gage_list_USA/NV_gages_updated.txt",  header = F) 
head(NV_gages_long)

TX_gages_long <- read.delim("Data/Gage_list_USA/TX_gages_updated.txt",  header = F) 
head(TX_gages_long)

UT_gages_long <- read.delim("Data/Gage_list_USA/UT_gages_updated.txt",  header = F) 
head(UT_gages_long)

ID_gages_long <- read.delim("Data/Gage_list_USA/ID_gages_updated.txt", header = F)
head(ID_gages_long)

OR_gages_long <- read.delim("Data/Gage_list_USA/OR_gages_updated.txt", header = F)
head(ID_gages_long)

WY_gages_long <- read.delim("Data/Gage_list_USA/WY_gages_updated.txt", header = F)
head(ID_gages_long)


# Find lines with gage numbers
AZ_gages_long # Read this to see where indicates number of gages and site info
AZ_gages_long[19,] # 281 sites in this file
AZ_gages_long[20:300,]
AZ_gages_long[20:297,]

CA_gages_long[19,] # 591 sites 
CA_gages_long[20:611,]
CA_gages_long[20:592,]

CO_gages_long[18,] # 599 sites 
CO_gages_long[19:618,]
CO_gages_long[19:561,]

NM_gages_long[18,] # 264 sites 
NM_gages_long[19:282,]
# NM_gages_long[19:103,]

NV_gages_long[18,] # 258 sites
NV_gages_long[19:277,]
NV_gages_long[19:265,]

TX_gages_long[18, ] # 654 sites
TX_gages_long[19:673,]
#TX_gages_long[19:539,]

UT_gages_long[18,] # 294 sites in this file
UT_gages_long[19:313,]
UT_gages_long[19:297,]

ID_gages_long[19, ] #392 sites
ID_gages_long[20:412, ]
ID_gages_long[20:408, ]

OR_gages_long[18, ] #353 sites
OR_gages_long[19:372, ]
OR_gages_long[19:364, ]

WY_gages_long[18, ] #238 sites
WY_gages_long[19:257, ]
WY_gages_long[19:254, ]

# # Get gage site numbers
# ?substring
# AZ_siteNumbers <- substring(AZ_gages_long[20:297, ], 11, 18)
# CA_siteNumbers <- substring(CA_gages_long[20:592, ], 11, 18)
# CO_siteNumbers <- substring(CO_gages_long[19:561, ], 11, 18)
# NM_siteNumbers <- substring(NM_gages_long[19:282, ], 11, 18)
# NV_siteNumbers <- substring(NV_gages_long[19:265, ], 11, 18)
# TX_siteNumbers <- substring(TX_gages_long[19:672, ], 11, 18)
# UT_siteNumbers <- substring(UT_gages_long[19:297, ], 11, 18)
# ID_siteNumbers <- substring(ID_gages_long[20:408, ], 11, 18)
# OR_siteNumbers <- substring(OR_gages_long[19:364, ], 11, 18)
# WY_siteNumbers <- substring(WY_gages_long[19:254, ], 11, 18)

# Get full sitenumbers not just 8 digit ones
AZ_siteNumbers <- unlist(lapply(strsplit(AZ_gages_long[20:297, ], " "), function(x) x[6]))
CA_siteNumbers <- unlist(lapply(strsplit(CA_gages_long[20:592, ], " "), function(x) x[6]))
CO_siteNumbers <- unlist(lapply(strsplit(CO_gages_long[19:561, ], " "), function(x) x[6]))
NM_siteNumbers <- unlist(lapply(strsplit(NM_gages_long[19:282, ], " "), function(x) x[6]))
NV_siteNumbers <- unlist(lapply(strsplit(NV_gages_long[19:265, ], " "), function(x) x[6]))
TX_siteNumbers <- unlist(lapply(strsplit(TX_gages_long[19:672, ], " "), function(x) x[6]))
UT_siteNumbers <- unlist(lapply(strsplit(UT_gages_long[19:297, ], " "), function(x) x[6]))
ID_siteNumbers <- unlist(lapply(strsplit(ID_gages_long[20:408, ], " "), function(x) x[6]))
OR_siteNumbers <- unlist(lapply(strsplit(OR_gages_long[19:364, ], " "), function(x) x[6]))
WY_siteNumbers <- unlist(lapply(strsplit(WY_gages_long[19:254, ], " "), function(x) x[6]))


# Fix Texas site numbers
# TX_siteNumbers[c(332,333,340,341,345,347,518)]
# TX_gages_long[c(332,333,340,341,345,347,518)+15,]
# TX_siteNumbers_b <- substring(TX_gages_long[c(332,333,340,341,345,347,518)+15,], 12, 19)
# TX_siteNumbers[c(332,333,340,341,345,347,518)] <- replace(TX_siteNumbers[c(332,333,340,341,345,347,518)], 
#                                                       1:7,TX_siteNumbers_b)


# Get site info and lat/long for each gage
siteINFO_state <- readNWISsite(NM_siteNumbers) #AZ, CA, CO, NM, NV, UT are ok
readNWISsite(UT_siteNumbers)

USA_siteNumbers <- c(AZ_siteNumbers, CA_siteNumbers, CO_siteNumbers, NM_siteNumbers, 
                     NV_siteNumbers, TX_siteNumbers, UT_siteNumbers, ID_siteNumbers,
                     OR_siteNumbers, WY_siteNumbers)

siteINFO <- readNWISsite(USA_siteNumbers)

# Find what data is available
dailyDataAvailable <- whatNWISdata(siteNumber = USA_siteNumbers, service = "dv", statCd="00003") # StatCd = 00003 is daily mean, 00001 is max, 00002 is min 

  # duplicates for sites with site numbers that are longer than 8 characters plus if there are multiple parameters
  # want columns dec_lat_va and dec_long_va

# Get specific info from gages
# Common USGS Parameter Codes (parm_cd)
# 00060 is discharge (cubic ft per sec)
# 00010 is temperature
# 00400 is pH
library(dplyr)
gageLocation <- distinct(dailyDataAvailable, x = site_no, .keep_all = TRUE)
str(gageLocation)
gageLocation <- as.matrix(gageLocation)
str(gageLocation)
head(gageLocation)

# write.csv(gageLocation, file = "Output/Gage_location_USA_update.csv")
# Get list of gages (siteNumbers) that are near (xx km) a fish sampling location only (using GIS)


