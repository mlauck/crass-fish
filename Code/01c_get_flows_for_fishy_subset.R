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
USA_subset <- read.csv("Data/from_GIS/Join_fish_gage_5km_USA_update.csv",  row.names = 1,
                       colClasses = c(rep(NA, 12), "character", rep(NA, 22)))
#import to excel and change the gage_ID to 00000000, 
# then save as .csv file, then run this. 
unique(USA_subset$site_no)
head(USA_subset)
str(USA_subset)

site_IDs <- unique(USA_subset$site_no)
compDailyData <- list()

site_IDs <- site_IDs[-98]
# IDs that don't make it: 18, 60, 74
# i <- 60 #18, 60, 74
for (i in 1:length(site_IDs)) { 
  rawDailyData <- readNWISdv(site_IDs[i], parameterCd, startDate = "", endDate = "")
  compDailyData[[i]] <- rawDailyData[,2:4] # stopped at 98 site_IDs[98] "09285000"
  #charge <- print(rawDailyData)
}
?readNWISdv
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
unique(df_daily$site_no)
df_daily[df_daily$site_no == "08427500",]
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


# See what dates match with fish data surveys
# info on fish data "points_daterange_location.csv"
# use that to determine which time frame is most important for analyzing discharge and getting summary metrics.
library(dplyr)


fish_dates <- read.csv("Data/points_daterange_location.csv")
df_daily <- read.csv("Output/Discharge_win5kmfish_USA.csv", colClasses = c(NA, "character", rep(NA,3)))
USA_subset <- read.csv("Data/from_GIS/Join_fish_gage_5km_USA_update.csv",  row.names = 1,
                       colClasses = c(rep(NA, 13), "character", rep(NA, 22)))

USA_subset
USA_subset[USA_subset$Dataset == "NEON", ]
fish_dates[fish_dates$Dataset == "NEON", ] # hmmm. maybe they are not near a gage well REDB is near 10172200
USA_subset[USA_subset$site_no == "10172200",] # ok now that's there after the update, good.
str(df_daily)

nrow(USA_subset[USA_subset$Total_Samp > 1,])
nrow(USA_subset[USA_subset$Total_Samp >= 10,]) #update 31 # old 13
nrow(USA_subset[USA_subset$Total_Samp >= 30,]) #update 6# old 4


df_daily2 <- df_daily[ ,1:3]  %>% mutate(year = year(Date))
str(df_daily2)
head(df_daily2)
unique(df_daily2$site_no)

yr_count <- df_daily2 %>% group_by(site_no) %>% summarise(yrs_record = n_distinct(year))
yr_count
yr_count$yrs_record[which(yr_count$yrs_record >= 30)] # more than half (229 of 380) have long records yay!

# Next time: Where are they?
# if good geographic spread, calculate trends
# for all other one-offs calculate zero-flow metrics for that year.


# What dates do I need for each gage?
dates_summary <- USA_subset %>% group_by(site_no) %>% summarise(first_yr = min(Year_First),
                                                                last_yr = max(Year_Last_),
                                                                total_samps = sum(Total_Samp))
dates_summary
dates_summary[dates_summary$last_yr - dates_summary$first_yr >=10 & dates_summary$total_samps >=10, ] # 37 -Zipper et al. use gages with more than 10 years (for future citation justification)
                                                                     # we can use gages with more than 10 years that have fish
dates_summary[dates_summary$last_yr - dates_summary$first_yr >=15 & dates_summary$total_samps >=10, ] # 10, -for 20 yrs there are 21, for 15 yrs there are 24

# ANNUAL METRICS for all gages
###################################################################################################################
head(df_daily)
df_daily_info <- df_daily[ ,1:4] %>% mutate(month = month(Date),
                                            year = year(Date),
                                            yday = yday(Date),
                                            wday = ifelse(yday > 274, yday - 273, yday+92),
                                            wyear = dataRetrieval::calcWaterYear(Date),
                                            flowbinary = ifelse(X_00060_00003>0,1,0))

#number of discrete periods of flow/no flow
#length of each period of flow                                                                                        
periods <- df_daily_info %>% group_by(site_no, wyear) %>% summarise(periods_lengths = rle(flowbinary)$lengths,
                                                                    periods_values = rle(flowbinary)$values
                                                                    )
                                                                   
tail(periods)
  
flowperiods <- subset(periods, periods_values == 1)
noflowperiods <- subset(periods, periods_values == 0)

flow_periods_metrics <- periods[periods$periods_values == 1, ] %>% 
  group_by(site_no, wyear) %>% summarise(tot_flowperiods = sum(periods_values),
                                         mean_lengthflow = mean(periods_lengths),
                                         max_lengthflow = max(periods_lengths),
                                         min_lengthflow = min(periods_lengths),
                                         med_lengthflow = median(periods_lengths),
                                         cv_lengthflow = sd(periods_lengths, na.rm = TRUE)/mean(periods_lengths))

noflow_periods_metrics <- periods[periods$periods_values == 0, ] %>% 
  group_by(site_no, wyear) %>% summarise(tot_periods_noflow = sum(periods_values),
                                         mean_length_noflow = mean(periods_lengths),
                                         max_length_noflow = max(periods_lengths),
                                         min_length_noflow = min(periods_lengths),
                                         med_length_noflow = median(periods_lengths),
                                         cv_length_noflow = sd(periods_lengths, na.rm = TRUE)/mean(periods_lengths))
tail(periods_metrics)
tail(flow_periods_metrics)
tail(noflow_periods_metrics)


head(df_daily_info)
# see how many years of data missing less than 10% daily values, only want to use those years of data
dailydatacount <- df_daily_info %>% group_by(site_no, wyear) %>% summarise(count = sum(!is.na(X_00060_00003)),
                                                                           noflowdays = sum(X_00060_00003 == 0),
                                                                           annual_noflow_fraction = noflowdays/count,
                                                                           peakdate_wy = wday[which.max(X_00060_00003)], #day of water year with peak flow
                                                                           lowflowdate_wy = wday[which.min(X_00060_00003)],#days of water year with lowest flow
                                                                           zeroflowfirst_wy = wday[which(X_00060_00003 == 0)[1]],
                                                                           zeroflowcentroid_wy = wday[mean(which(X_00060_00003 == 0), na.rm = TRUE)]
                                                                           )


                                                               
dailydatacount[dailydatacount$noflowdays >0, ]

dailydata_10permissing <- subset(dailydatacount, dailydatacount$count < 330) #1,268 rows



# Omit years with 10% or more discharge data missing
daily_data_metrics <- subset(dailydatacount, dailydatacount$count > 330)

# Join period and annual metrics to one dataset


###################################################################################################################
### OVERALL TREND ANALYSIS ###
# For trend analysis - do we want gages all time trend, or only for when there are fish data?
# well other papers have for continental or global trend, so perhaps just where fish are, but 
# they don't necessarily know what it's like for gages with fish - start big, and zoom in later if needed
gages_10yrs <- dates_summary[dates_summary$last_yr - dates_summary$first_yr >=10 & dates_summary$total_samps >=10, ] 
df_daily[df_daily$site_no %in% gages_10yrs$site_no,] 

str(df_daily)

library(ggplot2)
unique(dailydatacount$site_no)
dailydatacount[dailydatacount$noflowdays > 0, ]
gage1 <- dailydatacount[dailydatacount$site_no == "08382830",]
a <- ggplot(gage1, aes(x = wyear, y = noflowdays)) + geom_point()
a

#plot(gage1$wyear, gage1$noflowdays)
dailydatacount$wyear
library(effects)
library(lme4)


m1 <- glmer(noflowdays ~  scale(wyear) + (1|site_no), family = "poisson", data = dailydatacount)
ef <- Effect("wyear", m1)
theme_set(theme_bw())
ggplot(as.data.frame(ee),
       aes(wyear, fit))+
  geom_line()+
  ## colour=NA suppresses edges of the ribbon
  geom_ribbon(colour=NA,alpha=0.1,
              aes(ymin=lower,ymax=upper))+
  ## add rug plot based on original data
  geom_rug(data=ee$data,aes(y=NULL),sides="b")


# Number of no flow days has decreased through time and not a large effect.
# but it’s very small and the trend is opposite what we would expect (perhaps due to dam construction 
# induced baseflows during summer for irrigation and whatnot ???
# Maybe should separate perennial from non-perennial gages ? before and after 1960?
#                                                                       