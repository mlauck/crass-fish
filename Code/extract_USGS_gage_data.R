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
UT_gages_long <- read.delim("Data/Gage_list_USA/UT_gages.txt",  header = F) 
head(UT_gages_long)

UT_gages_long[15,]
UT_gages_long[15:92,]

# Example loop ---------------------------------------------------------------------------------------------
# Loopty loo Verde
siteNumbers <- c("09503700", "09504000", "09504420", "09504500", 
                 "09505200", "09505800", "09506000", "09507980",
                 "09508300", "09510200")

gageName <- c("VERDE RIVER NEAR PAULDEN", "VERDE RIVER NEAR CLARKDALE", "OAK CREEK NEAR SEDONA", " OAK CREEK NEAR CORNVILLE", 
              "WET BEAVER CREEK NEAR RIMROCK", "WEST CLEAR CREEK NEAR CAMP VERDE", "VERDE RIVER NEAR CAMP VERDE", "EAST VERDE RIVER NEAR CHILDS",
              "WET BOTTOM CREEK NEAR CHILDS", "SYCAMORE CREEK NEAR FORT MCDOWELL")
objNames <- c("Verde_Paulden", "Verde_Clarkdale", "Oak_Sedona", "Oak_Cornville", 
              "Beaver_Rimrock", "W.Clear_Camp.Verde", "E.Verde_Childs", 
              "Bottom_Childs", "Sycamore_McDowell")

parameterCd <- "00060" # this is discharge
i <- 1
for (i in 1:length(siteNumbers)) {
  rawDailyData <- readNWISdv(siteNumbers[i], parameterCd, startDate = "", endDate = "")
  rawDailyData <- rawDailyData[,3:4]
  #charge <- print(rawDailyData)
  
  # Convert raw data to 'asStreamflow' object
  Verde.stream <- asStreamflow(rawDailyData, river.name= paste0('USGS', ' ', siteNumbers[i],' ', gageName[i], ', AZ')) # USGS 09503700 VERDE RIVER NEAR PAULDEN, AZ
  summary(Verde.stream)
  # Run Fourier on the 'asStreamflow' object
  Verde.stream_seas <- fourierAnalysis(Verde.stream)
  # Make a file of the main data
  #write.csv(Verde.stream_seas$signal, file = paste0('DFFT_export/USGS', ' ', siteNumbers[i],' ', gageName[i], ', AZ.csv'))
  # plot characteristic hydrograph
  # dots are daily values, the red line is the long-term seasonal profile (integrates the 3 significant signals)
  plot(Verde.stream_seas)
}
