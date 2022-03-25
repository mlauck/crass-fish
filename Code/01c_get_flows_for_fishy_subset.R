### Jane S. Rogosch
### Created 18 Feb 2022
### This code is to extract USGS discharge data from subset of gages that are in HUCs where
### we have fish sampling data. The subset of gages was found using ArcGIS 10.2 select by location functions

### Load libraries--------------------------------------------------------------
# Note: some of these libraries may not be necessary

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

### Load the file with the subset of gages
# USA_subset <- read.csv("Data/from_GIS/USA_fish_nearest_gage_within5km.csv", row.names = 1, 
#                        colClasses = c(rep(NA, 12), "character", rep(NA, 24)))
#                         #import to excel and change the gage_ID to 00000000, 
# # then save as text file, then import and switch column format from general to "text" nOPE
?read.delim
USA_subset <- read.csv("Data/from_GIS/Join_fish_gage_5km_USA.csv",  row.names = 1,
                       colClasses = c(rep(NA, 13), "character", rep(NA, 22)))
#import to excel and change the gage_ID to 00000000, 
# then save as .csv file, then run this. 

head(USA_subset)
str(USA_subset)

site_IDs <- unique(USA_subset$site_no)
compDailyData <- list()

# IDs that don't make it: 18, 60, 74
# i <- 60 #18, 60, 74
for (i in 1:length(site_IDs)) { 
  rawDailyData <- readNWISdv(site_IDs[i], parameterCd, startDate = "", endDate = "")
  compDailyData[[i]] <- rawDailyData[,2:4]
  #charge <- print(rawDailyData)
}

str(compDailyData)
length(compDailyData)
#   # Convert raw data to 'asStreamflow' object
#   Verde.stream <- asStreamflow(rawDailyData, river.name= paste0('USGS', 'siteNumbers[i]')) # USGS 09503700 VERDE RIVER NEAR PAULDEN, AZ
#   summary(Verde.stream)
#   # Run Fourier on the 'asStreamflow' object
#   Verde.stream_seas <- fourierAnalysis(Verde.stream)
#   # Make a file of the main data
#   #write.csv(Verde.stream_seas$signal, file = paste0('DFFT_export/USGS', ' ', siteNumbers[i],' ', gageName[i], ', AZ.csv'))
#   # plot characteristic hydrograph
#   # dots are daily values, the red line is the long-term seasonal profile (integrates the 3 significant signals)
#   plot(Verde.stream_seas)
# }

# Make sure gages have at least 30 years of data
library(dplyr)
df_daily <- bind_rows(compDailyData)
str(df_daily)
head(df_daily)
unique(df_daily$site_no) # hmm there are only 155, whereas there are 158 siteIDs

#write.csv(df_daily, file = "Output/Discharge_win5kmfish_USA.csv")


# year(df_daily$Date)
# unique(df_daily$site_no)
# ?match
# #anti_join(as.factor(site_IDs), as.factor(df_daily$site_no))
# site_IDs[site_IDs %in% df_daily$site_no == FALSE] #"09096100" "09470920" "09522005" 
# which(site_IDs %in% df_daily$site_no == FALSE) # 18, 60, 74 # these don't come up with any data using readNWIS

# "09096100" only has temperature and gage height in the reservoir, no discharge
# "09470920"only has gage height
# "09522005" temp conductivity and dissolved solids
# So we're good.

# df_daily2 <- df_daily[ ,1:3]  %>% mutate(year = year(Date))
# str(df_daily2)
# head(df_daily2)
# ?n_distinct
# 
# yr_count <- df_daily2 %>% group_by(site_no) %>% summarise(yrs_record = n_distinct(year))
# yr_count
# yr_count$yrs_record[which(yr_count$yrs_record >= 30)] # most (114 of 155) of these have long records yay!


# Next time: See what dates match with fish data surveys
# info on fish data "points_daterange_location.csv"
# use that to determine which time frame is most important for analyzing discharge and getting summary metrics.
library(dplyr)


fish_dates <- read.csv("Data/points_daterange_location.csv")
df_daily <- read.csv("Output/Discharge_win5kmfish_USA.csv", colClasses = c(NA, "character", rep(NA,3)))
USA_subset <- read.csv("Data/from_GIS/Join_fish_gage_5km_USA.csv",  row.names = 1,
                       colClasses = c(rep(NA, 13), "character", rep(NA, 22)))

USA_subset
USA_subset[USA_subset$Dataset == "NEON", ]
fish_dates[fish_dates$Dataset == "NEON", ] # hmmm. maybe they are not near a gage well REDB is near 10172200
USA_subset[USA_subset$site_no == "10172200",] # umm, this is not in the dataset. YIKES! go to 01a and figure out what happened
str(df_daily)

nrow(USA_subset[USA_subset$Total_Samp > 1,])
nrow(USA_subset[USA_subset$Total_Samp >= 10,]) #13
nrow(USA_subset[USA_subset$Total_Samp >= 30,]) #4


df_daily2 <- df_daily[ ,1:3]  %>% mutate(year = year(Date))
str(df_daily2)
head(df_daily2)

yr_count <- df_daily2 %>% group_by(site_no) %>% summarise(yrs_record = n_distinct(year))
yr_count
yr_count$yrs_record[which(yr_count$yrs_record >= 30)] # most (114) of these have long recors yay!

# Next time: Where are they?
# if good geographic spread, calculate trends
# for all other one-offs calculate zero-flow metrics for that year.


# What dates do I need for each gage?
dates_summary <- USA_subset %>% group_by(site_no) %>% summarise(first_yr = min(Year_First),
                                                                last_yr = max(Year_Last_),
                                                                total_samps = sum(Total_Samp))
dates_summary
dates_summary[dates_summary$last_yr - dates_summary$first_yr >=10, ] #22 Zipper et al. use gages with more than 10 years (for future citation justification)
                                                                     # we can use gages with more than 10 years that have fish
dates_summary[dates_summary$last_yr - dates_summary$first_yr >=30, ] # 6, for 20 yrs there are 8, for 15 yrs there are 13

# ANNUAL METRICS
###################################################################################################################
gages_10yrs <- dates_summary[dates_summary$last_yr - dates_summary$first_yr >=10, ] 
df_daily[df_daily$site_no %in% gages_10yrs$site_no,] 

# No flow days
gage
noflow <- as.numeric(length(which(df_daily$X_00060_00003 == 0)))


###################################################################################################################
### OVERALL TREND ANALYSIS ###
# For trend analysis - do we want gages all time trend, or only for when there are fish data?
# well other papers have for continental or global trend, so perhaps just where fish are, but 
# they don't necessarily know what it's like for gages with fish - start big, and zoom in later if needed
gages_10yrs <- dates_summary[dates_summary$last_yr - dates_summary$first_yr >=10, ] 
