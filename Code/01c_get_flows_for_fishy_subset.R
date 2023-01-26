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
# cat('Your Rprofile has been updated to include GRAN.
#     Please restart R for changes to take effect.')
#  install.packages("smwrBase")
library(smwrBase)
#########################################################################################################################
##### IF NOT RUNNING FOR FIRST TIME SKIP AHEAD #####
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

### Load the file with the subset of gages------------------------------------------------------
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
head(compDailyData)
length(compDailyData)
### GET NAAs----------------------------------------------------------------------------------
?asStreamflow
CompSignal <- list() #IDs that don't make it 24, 115, 117, 158,239,245
for (i in 246:length(site_IDs)) { 
  rawDailyData <- readNWISdv(site_IDs[i], parameterCd, startDate = "", endDate = "")
  streamflow <- asStreamflow(rawDailyData[ ,3:4], river.name = rawDailyData$site_no[1], max.na = 1815) # Convert raw data to 'asStreamflow' object
  summary(streamflow)
  USA_stream <- fourierAnalysis(streamflow) # Run Fourier on the 'asStreamflow' object
  CompSignal[[i]] <- cbind(USA_stream$signal, site_no = streamflow$name)
  # Make a file of the main data
  write.csv(cbind(USA_stream$signal, site_no = streamflow$name), file = paste0('Output/NAA/USGS', '_', rawDailyData$site_no[1],'.csv'))
  # plot characteristic hydrograph
  #   # dots are daily values, the red line is the long-term seasonal profile (integrates the 3 significant signals)
  #plot(USA_stream)
}

# Make things not a list
library(dplyr)
signal_daily <- bind_rows(CompSignal)
# write.csv(signal_daily, file = "Output/NAA/USA_signal_daily.csv")

df_daily <- bind_rows(compDailyData)
str(df_daily)
head(df_daily)
unique(df_daily$site_no)
df_daily[df_daily$site_no == "08427500",]
#write.csv(df_daily, file = "Output/Discharge_win5kmfish_USA.csv")


#################################################################################################################
### START HERE IF SKIPPED AHEAD --------------------------------------------------------------------------------
# See what dates match with fish data surveys
# info on fish data "points_daterange_location.csv"
# use that to determine which time frame is most important for analyzing discharge and getting summary metrics.
library(dplyr)
library(tidyr)
library(Kendall)
??Kendall
??trend

fish_dates <- read.csv("Data/points_daterange_location.csv")
USA_subset <- read.csv("Data/from_GIS/Join_fish_gage_5km_USA_update.csv",  row.names = 1,
                       colClasses = c(rep(NA, 12), "character", rep(NA, 22)))
df_daily <- read.csv("Output/Discharge_win5kmfish_USA.csv", colClasses = c(NA, "character", rep(NA,3)))
signal_daily <- read.csv("Output/NAA/USA_signal_daily.csv", row.names = 1, colClasses = c(rep(NA,12), "character") )
str(signal_daily)


USA_subset
USA_subset[USA_subset$Dataset == "NEON", ]
fish_dates[fish_dates$Dataset == "NEON", ] # hmmm. maybe they are not near a gage well REDB is near 10172200
USA_subset[USA_subset$site_no == "10172200",] # ok now that's there after the update, good.
str(df_daily)

USA_subset[USA_subset$site_no == "10163000",] # huc_cd = 0, why? don't know but the HUC8 it's in is	"16020203"
min(USA_subset$Year_First)
sort(unique(USA_subset$Year_First))

nrow(USA_subset[USA_subset$Total_Samp > 1,])
nrow(USA_subset[USA_subset$Total_Samp >= 10,]) #update 31 # old 13
nrow(USA_subset[USA_subset$Total_Samp >= 30,]) #update 6# old 4


df_daily2 <- df_daily[ ,1:3]  %>% mutate(year = year(Date))
str(df_daily2)
unique(df_daily2$site_no)

yr_count <- df_daily2 %>% group_by(site_no) %>% summarise(yrs_record = n_distinct(year))
yr_count
yr_count$yrs_record[which(yr_count$yrs_record >= 30)] # more than half (229 of 380) have long records yay!

# Next time: Where are they?
# if good geographic spread, calculate trends
# for all other one-offs calculate zero-flow metrics for that year.


# What dates do I need for each gage?
# Result is gages corresponding to fish data points that have 10 or more fish samples and 10 or more years of discharge data.
dates_summary <- USA_subset %>% group_by(site_no) %>% summarise(first_yr = min(Year_First),
                                                                last_yr = max(Year_Last_),
                                                                total_samps = sum(Total_Samp))
min(dates_summary$first_yr)
date_summary <- dates_summary[dates_summary$last_yr - dates_summary$first_yr >=10 & dates_summary$total_samps >=10, ] # 37 -Zipper et al. use gages with more than 10 years (for future citation justification)
                                                                     # we can use gages with more than 10 years that have fish
dates_summary[dates_summary$last_yr - dates_summary$first_yr >=30 & dates_summary$total_samps >=10, ] # 10 for 30 years, -for 20 yrs there are 21, for 15 yrs there are 24

hist(date_summary$first_yr)
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

###
df_daily_info %>% group_by(site_no) %>% summarise(first_yr = min(year),
                                last_yr = max(year),
                                total_samps = sum(unique(year)))
###################################################################################################################
# Exploring trends with Mann-Kendall and Sen's slope
###################################################################################################################
library(trend)
str(daily_data_metrics)
?sens.slope
###### NO FLOW DAYS #######
noflowmets <- arrange(daily_data_metrics[ , c("site_no", "wyear", "noflowdays") ], wyear) %>% pivot_wider(names_from = site_no, values_from = noflowdays)
tail(noflowmets)
# df.noflowmets <- as.data.frame(noflowmets[noflowmets$wyear >= 1954, ]) #why did I pick 1954?
# Because first year of fish data is 1954.
df.noflowmets <- as.data.frame(noflowmets)

# str(df.noflowmets)
# df.simple.noflowmets <- df.noflowmets %>% replace(is.na(.), 0)
# 
# test.res <- apply(df.simple.noflowmets[,-1], 2, function(x) mk.test(x)$p.value)
# 
# test.res.sen <- apply(df.simple.noflowmets[,-1], 2, function(x) sens.slope(x)$estimates)
# test.res.senp <- apply(df.simple.noflowmets[,-1], 2, function(x) sens.slope(x)$p.value)

# next time need to figure out how to do this right so that including just a set of consecutive years and not
# missing chunks in time series. Probably in a loop. Remove all NA rows for a gage (but still need to monitor for gaps between years)
# alternatively make some rules, like only include gages with metrics for last consecutive 30 years
# might have some gages that go through last 100 years which could be useful, then when plot they'ds
# just have to start when the consecutive record starts.
# Let's see (this is not elegant or streamlined, but it will get the job done in the slow pace I like to think at!)
str(df.noflowmets)

# # By decade
# # Oldest could possibly be 1902, but it's not. Start with 1912 then
# df.noflowmets_1912 <- as.data.frame(noflowmets[noflowmets$wyear >= 1912, ])
#   # get rid of columns with NAs
# df.noflowmets_1912b <- df.noflowmets_1912 %>% select_if(~ !any(is.na(.)))
# 
# sen_1912 <- apply(df.noflowmets_1912b[,-1], 2, function(x) sens.slope(x)$estimates)
# senp_1912 <- apply(df.noflowmets_1912b[,-1], 2, function(x) sens.slope(x)$p.value)
# 
# # 1922
# df.noflowmets_1922 <- as.data.frame(noflowmets[noflowmets$wyear >= 1922, ])
# df.noflowmets_1922c <- df.noflowmets_1922 %>% select(-names(df.noflowmets_1912b[-1]))
# # get rid of columns with NAs
# df.noflowmets_1922b <- df.noflowmets_1922c %>% select_if(~ !any(is.na(.)))
# 
# sen_1922 <- apply(df.noflowmets_1922b[,-1], 2, function(x) sens.slope(x)$estimates)
# senp_1922 <- apply(df.noflowmets_1922b[,-1], 2, function(x) sens.slope(x)$p.value)
# 
# # 1932
# df.noflowmets_1932 <- as.data.frame(noflowmets[noflowmets$wyear >= 1932, ])
# df.noflowmets_1932c <- df.noflowmets_1932 %>% select(-names(c(df.noflowmets_1912b[-1], df.noflowmets_1922b[-1])))
# # get rid of columns with NAs
# df.noflowmets_1932b <- df.noflowmets_1932c %>% select_if(~ !any(is.na(.)))
# 
# sen_1932 <- apply(df.noflowmets_1932b[,-1], 2, function(x) sens.slope(x)$estimates)
# senp_1932 <- apply(df.noflowmets_1932b[,-1], 2, function(x) sens.slope(x)$p.value)
# 
# # 1942
# df.noflowmets_1942 <- as.data.frame(noflowmets[noflowmets$wyear >= 1942, ])
# df.noflowmets_1942c <- df.noflowmets_1942 %>% select(-names(c(df.noflowmets_1912b[-1], df.noflowmets_1922b[-1], df.noflowmets_1932b[-1])))
# # get rid of columns with NAs
# df.noflowmets_1942b <- df.noflowmets_1942c %>% select_if(~ !any(is.na(.)))
# 
# sen_1942 <- apply(df.noflowmets_1942b[,-1], 2, function(x) sens.slope(x)$estimates)
# senp_1942 <- apply(df.noflowmets_1942b[,-1], 2, function(x) sens.slope(x)$p.value)

# # 1952
# df.noflowmets_1954 <- as.data.frame(noflowmets[noflowmets$wyear >= 1954, ])
# #df.noflowmets_1954c <- df.noflowmets_1952 #%>%
#   # select(-names(c(df.noflowmets_1912b[-1], df.noflowmets_1922b[-1],
#   #                 df.noflowmets_1932b[-1], df.noflowmets_1942b[-1])))
# # get rid of columns with NAs
# df.noflowmets_1954b <- df.noflowmets_1954 %>% select_if(~ !any(is.na(.)))
# 
# sen_1954 <- apply(df.noflowmets_1954b[,-1], 2, function(x) sens.slope(x)$estimates)
# senp_1954 <- apply(df.noflowmets_1954b[,-1], 2, function(x) sens.slope(x)$p.value)
# 
# # 1962
# df.noflowmets_1962 <- as.data.frame(noflowmets[noflowmets$wyear >= 1962, ])
# df.noflowmets_1962c <- df.noflowmets_1962 %>% 
#   select(-names(c(#df.noflowmets_1912b[-1], df.noflowmets_1922b[-1],
#                   #df.noflowmets_1932b[-1], df.noflowmets_1942b[-1],
#                   df.noflowmets_1954b[-1])))
# # get rid of columns with NAs
# df.noflowmets_1962b <- df.noflowmets_1962c %>% select_if(~ !any(is.na(.)))
# 
# sen_1962 <- apply(df.noflowmets_1962b[,-1], 2, function(x) sens.slope(x)$estimates)
# senp_1962 <- apply(df.noflowmets_1962b[,-1], 2, function(x) sens.slope(x)$p.value)
# 
# # 1972
# df.noflowmets_1972 <- as.data.frame(noflowmets[noflowmets$wyear >= 1972, ])
# df.noflowmets_1972c <- df.noflowmets_1972 %>% 
#   select(-names(c(#df.noflowmets_1912b[-1], df.noflowmets_1922b[-1], 
#                   #df.noflowmets_1932b[-1], df.noflowmets_1942b[-1],
#                   df.noflowmets_1954b[-1], df.noflowmets_1962b[-1])))
# # get rid of columns with NAs
# df.noflowmets_1972b <- df.noflowmets_1972c %>% select_if(~ !any(is.na(.)))
# 
# sen_1972 <- apply(df.noflowmets_1972b[,-1], 2, function(x) sens.slope(x)$estimates)
# senp_1972 <- apply(df.noflowmets_1972b[,-1], 2, function(x) sens.slope(x)$p.value)
# 
# # 1982
# df.noflowmets_1982 <- as.data.frame(noflowmets[noflowmets$wyear >= 1982, ])
# df.noflowmets_1982c <- df.noflowmets_1982 %>% 
#   select(-names(c(#df.noflowmets_1912b[-1], df.noflowmets_1922b[-1], 
#                   #df.noflowmets_1932b[-1], df.noflowmets_1942b[-1],
#                   df.noflowmets_1954b[-1], df.noflowmets_1962b[-1],
#                   df.noflowmets_1972b[-1])))
# # get rid of columns with NAs
# df.noflowmets_1982b <- df.noflowmets_1982c %>% select_if(~ !any(is.na(.)))
# 
# sen_1982 <- apply(df.noflowmets_1982b[,-1], 2, function(x) sens.slope(x)$estimates)
# senp_1982 <- apply(df.noflowmets_1982b[,-1], 2, function(x) sens.slope(x)$p.value)
# 
# # 1992
# df.noflowmets_1992 <- as.data.frame(noflowmets[noflowmets$wyear >= 1992, ])
# df.noflowmets_1992c <- df.noflowmets_1992 %>% 
#   select(-names(c(#df.noflowmets_1912b[-1], df.noflowmets_1922b[-1], 
#                   #df.noflowmets_1932b[-1], df.noflowmets_1942b[-1],
#                   df.noflowmets_1954b[-1], df.noflowmets_1962b[-1],
#                   df.noflowmets_1972b[-1], df.noflowmets_1982b[-1])))
# # get rid of columns with NAs
# df.noflowmets_1992b <- df.noflowmets_1992c %>% select_if(~ !any(is.na(.)))
# 
# sen_1992 <- apply(df.noflowmets_1992b[,-1], 2, function(x) sens.slope(x)$estimates)
# senp_1992 <- apply(df.noflowmets_1992b[,-1], 2, function(x) sens.slope(x)$p.value)
# 
# sen_by_decade <- c(sen_1954, sen_1962, sen_1972, sen_1982, sen_1992) # sen_1912, sen_1922, sen_1932, sen_1942,
# senp_by_decade <- c(senp_1954, senp_1962, senp_1972, senp_1982, senp_1992) #senp_1912, senp_1922, senp_1932, senp_1942,
#   # so in total 146 of 360 gages with at least 30 years of data for trends
# plot(sen_by_decade)
# ?as.data.frame
# ?names
# df.decadal.sen <- as.data.frame(sen_by_decade, stringsAsFactors = TRUE)
# rownames(df.decadal.sen)
# df.decadal.sen$gageID <- rownames(df.decadal.sen)
# df.decadal.sen
# df.decadal.sen <- arrange(df.decadal.sen, sen_by_decade)
# 
# 
# a <- ggplot(df.decadal.sen, aes(as.numeric(sen_by_decade), reorder(gageID, -sen_by_decade, mean)))
# a+geom_point() + 
#   theme_classic() +
#   labs(x = "Sen slope", y = "GageID")
# 
# gage_more_noflow <- df.decadal.sen$gageID[df.decadal.sen$sen_by_decade > 0 ]
# gage_less_noflow <- df.decadal.sen$gageID[df.decadal.sen$sen_by_decade < 0 ]
# 
# which.watershed_more <- USA_subset[USA_subset$site_no %in% gage_more_noflow, ]
# unique(which.watershed_more$huc_cd)
# unique(which.watershed_more$station_nm)
# 
# which.watershed_less <- USA_subset[USA_subset$site_no %in% gage_less_noflow, ]
# unique(which.watershed_less$huc_cd)
# unique(which.watershed_less$station_nm)
# 
# unique(which.watershed_more$huc_cd[which.watershed_more$huc_cd %in% which.watershed_less$huc_cd])
# unique(which.watershed_more$station_nm[which.watershed_more$huc_cd %in% which.watershed_less$huc_cd])

# Since 1980 Only ##################################################################
# 1982
daily_data_metrics
noflow_periods_metrics 


# start with apprpriate metric
noflowmets <- arrange(daily_data_metrics[ , c("site_no", "wyear", "zeroflowfirst_wy") ], wyear) %>% pivot_wider(names_from = site_no, values_from = zeroflowfirst_wy)
# noflowmets <- arrange(daily_data_metrics[ , c("site_no", "wyear", "noflowdays") ], wyear) %>% pivot_wider(names_from = site_no, values_from = noflowdays)
noflowmets <- arrange(noflow_periods_metrics[ , c("site_no", "wyear", "max_length_noflow") ], wyear) %>% pivot_wider(names_from = site_no, values_from = max_length_noflow)

tail(noflowmets)

df.noflowmets <- as.data.frame(noflowmets)

df.noflowmets_1980 <- as.data.frame(noflowmets[noflowmets$wyear >= 1980, ])

df.noflowmets_1980[is.na(df.noflowmets_1980)] <- 0

# get rid of columns with NAs
df.noflowmets_1980b <- df.noflowmets_1980 %>% select_if(~ !any(is.na(.)))


sen_1980 <- data.frame(matrix(nrow = 187, ncol = 5)) #359,108
colnames(sen_1980) <- c('gageID', 'slope', 'p', 'lci', 'uci')
sen_1980$gageID <- colnames(df.noflowmets_1980b[-1])
# slope = double(),
#                        p = double(),
#                        lci = double(),
#                        uci = double())

sen_1980$slope<- apply(df.noflowmets_1980b[,-1], 2, function(x) sens.slope(x)$estimates)
sen_1980$p <- apply(df.noflowmets_1980b[,-1], 2, function(x) sens.slope(x)$p.value)
sen_1980$lci <- apply(df.noflowmets_1980b[-1], 2, function(x) sens.slope(x)$conf.int[1])
sen_1980$uci <- apply(df.noflowmets_1980b[-1], 2, function(x) sens.slope(x)$conf.int[2])
mean(sen_1980$slope)
mean(sen_1980$p, na.rm = TRUE)
mean(sen_1980$lci)
mean(sen_1980$uci)

  # # 1991 # catch any others that came on in last 30 years # for zeroflowdays and NAA
  # df.noflowmets_1991 <- as.data.frame(noflowmets[noflowmets$wyear >= 1991, ])
  # df.noflowmets_1991c <- df.noflowmets_1991 %>%
  #   select(-names( df.noflowmets_1980b[-1]))
  # # get rid of columns with NAs
  # df.noflowmets_1991b <- df.noflowmets_1991c %>% select_if(~ !any(is.na(.)))
  # 
  # sen_1991 <- data.frame(matrix(nrow = 36, ncol = 5)) #359
  # colnames(sen_1991) <- c('gageID', 'slope', 'p', 'lci', 'uci')
  # sen_1991$gageID <- colnames(df.noflowmets_1991b[-1])
  # 
  # sen_1991$slope <- apply(df.noflowmets_1991b[,-1], 2, function(x) sens.slope(x)$estimates)
  # sen_1991$p <- apply(df.noflowmets_1991b[,-1], 2, function(x) sens.slope(x)$p.value)
  # sen_1991$lci <- apply(df.noflowmets_1991b[-1], 2, function(x) sens.slope(x)$conf.int[1])
  # sen_1991$uci <- apply(df.noflowmets_1991b[-1], 2, function(x) sens.slope(x)$conf.int[2])
  
  #sens_1980 <- rbind(sen_1980, sen_1991)


# Hand calculate 95%CI
SE <- (sd(sen_1980$slope)/sqrt(length(sen_1980$slope)))
mean(sen_1980$slope) + (1.96*(sd(sen_1980$slope)/sqrt(length(sen_1980$slope))))
mean(sen_1980$slope) - (1.96*(sd(sen_1980$slope)/sqrt(length(sen_1980$slope))))
Z <- mean(sen_1980$slope)/SE
P <- pnorm(Z, mean = mean(sen_1980$slope), sd = sd(sen_1980$slope), lower.tail = FALSE)
?pnorm



df.1980.sen <- as.data.frame(sen_1980, stringsAsFactors = TRUE)
rownames(df.1980.sen)
df.1980.sen$gageID <- rownames(df.1980.sen)

b <- ggplot(df.1980.sen, aes(as.numeric(sen_1980), reorder(gageID, -sen_1980, mean)))
b+geom_point() + 
  theme_classic() +
  labs(x = "Sen slope", y = "GageID")

gage_more_noflow80 <- df.1980.sen$gageID[df.1980.sen$sen_1980 > 0 ]
gage_less_noflow80 <- df.1980.sen$gageID[df.1980.sen$sen_1980 < 0 ]

which.watershed_more80 <- USA_subset[USA_subset$site_no %in% gage_more_noflow80, ]
unique(which.watershed_more80$huc_cd)
unique(which.watershed_more80$station_nm)

which.watershed_less80 <- USA_subset[USA_subset$site_no %in% gage_less_noflow80, ]
unique(which.watershed_less80$huc_cd)
unique(which.watershed_less80$station_nm)

unique(which.watershed_more80$huc_cd[which.watershed_more80$huc_cd %in% which.watershed_less80$huc_cd])
unique(which.watershed_more80$station_nm[which.watershed_more80$huc_cd %in% which.watershed_less80$huc_cd])

# vec <- unlist(noflowmets[ ,3])
# vec2 <- vec[is.na(vec) == FALSE]
# mk.test(vec2)
# ovj <- mk.test(df.noflowmets[is.na(df.noflowmets[,3]) == FALSE, 3])

###### NO FLOW DAYS #######
# $ annual_noflow_fraction: num [1:15292] 0 0 0 0 0 0 0 0 0 0 ...
# $ peakdate_wy           : num [1:15292] 235 306 307 318 311 235 230 329 310 233 ...
# $ lowflowdate_wy        : num [1:15292] 104 44 95 71 98 61 121 99 97 138 ...
# $ zeroflowfirst_wy      : num [1:15292] NA NA NA NA NA NA NA NA NA NA ...
# $ zeroflowcentroid_wy
noflowmets <- arrange(daily_data_metrics[ , c("site_no", "wyear", "noflowdays") ], wyear) %>% pivot_wider(names_from = site_no, values_from = noflowdays)
tail(noflowmets)
# df.noflowmets <- as.data.frame(noflowmets[noflowmets$wyear >= 1954, ]) #why did I pick 1954?
df.noflowmets <- as.data.frame(noflowmets)

###################################################################################################################
### EXPLORING TRENDS USING NET ANNUAL ANNOMALIES - following similar approach to Temp and Precip data we have 20 Oct 2022
####################################################################################################################
length(unique(signal_daily$site_no)) #==376
head(signal_daily)
?Date
??Year
library(ggplot2)
library(lubridate)
NAA <- signal_daily[signal_daily$year >= 1980, ] %>% 
  group_by(site_no, year) %>% 
  summarise(NAA= sum(resid.sig, na.rm = TRUE))


# plot avg discharge anomaly boxplots with fill 
NAA_plot <- signal_daily[signal_daily$year >= 1980, ] %>% 
  group_by(site_no, year) %>% 
  summarise(NAA= sum(resid.sig, na.rm = TRUE)) %>% 
  group_by(year) %>% 
  mutate(mean_NAA= mean(NAA)) %>% 
  ggplot( aes(x = year, y = NAA, group = year) )+  ylim(-150, 150) +
  scale_fill_viridis_c(name = "Discharge anomaly", option = "C") +
  geom_boxplot(aes(fill = mean_NAA)) +
  theme_classic(base_size = 14) +
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
  xlab("Year") +
  ylab("Net annual discharge anomaly") +
  ggtitle("USA") +
  geom_hline(yintercept = 0, color = "red", linetype = "dotted", size = 1) 
print(NAA_plot)
ggsave(NAA_plot, filename = "figures/Discharge_NAA_box_USA.jpg", dpi = 300, height = 5, width = 7)

# No, this is not right.
# NAA_summ <- NAA %>%
#   group_by(year) %>%
#   summarise(mean_NAA= mean(NAA))
# sens.slope(NAA_summ$mean_NAA)

#alternatively and more accurately
?pivot_wider
?sens.slope
NAA_wide <- pivot_wider(NAA, names_from = site_no, values_from = NAA)
NAA_wider <- NAA_wide %>% select_if(~ !any(is.na(.)))
?rbind
sens_NAA <- apply(NAA_wider[-1], 2, function(x) as.data.frame(rbind(sens.slope(x)$estimates, sens.slope(x)$p.value, 
                                                      sens.slope(x)$conf.int[1], sens.slope(x)$conf.int[2]), 
                                                      row.names = c("slope", "p", "lci", "uci")))
dfsensNAA <- as.data.frame(do.call(cbind, sens_NAA))
str(dfsensNAA)

dfsensNAA<-bind_cols(sens_NAA)
mean(dfsensNAA[1,], na.rm = TRUE)

sens_NAA <- NA

sens_NAA$slope <- apply(NAA_wider[-1], 2, function(x) sens.slope(x)$estimates)
sens_NAA$p <- apply(NAA_wider[-1], 2, function(x) sens.slope(x)$p.value)
sens_NAA$lci <- apply(NAA_wider[-1], 2, function(x) sens.slope(x)$conf.int[1])
sens_NAA$uci <- apply(NAA_wider[-1], 2, function(x) sens.slope(x)$conf.int[2])
mean(sens_NAA$slope)
mean(sens_NAA$p, na.rm = TRUE)
mean(sens_NAA$lci) # Is that the same as getting 95% CI on the mean slope?
mean(sens_NAA$uci) # Is that the same as getting 95% CI on the mean slope?
# Hand calculate 95%CI
SE <- (sd(sens_NAA$slope)/sqrt(length(sens_NAA$slope)))
mean(sens_NAA$slope) + (1.96*(sd(sens_NAA$slope)/sqrt(length(sens_NAA$slope))))
mean(sens_NAA$slope) - (1.96*(sd(sens_NAA$slope)/sqrt(length(sens_NAA$slope))))
Z <- mean(sens_NAA$slope)/SE
P <- pnorm(Z, mean = mean(sens_NAA$slope), sd = sd(sens_NAA$slope))
# Next time look at plots for larger catchment regions - see if some regional differences are being masked by the whole.
# Also want to do this for Australia, and still look at Kendall stat or Mann-Kendall

###################################################################################################################
### OVERALL TREND ANALYSIS using Generalized Linear Modeling Approach - Given up Oct 2022 for NAA approach, 
# but could still be useful as record of what we tried and for some data subsetting ###


# For trend analysis - do we want gages all time trend, or only for when there are fish data?
# well other papers have for continental or global trend, so perhaps just where fish are, but 
# they don't necessarily know what it's like for gages with fish - start big, and zoom in later if needed

# gages with 10yrs fish data
gages_10yrs_fish <- dates_summary[dates_summary$last_yr - dates_summary$first_yr >=10 & dates_summary$total_samps >=10, ] 
min(gages_10yrs_fish$first_yr)
gages_10yrs_fish_site <- dates_summary$site_no[dates_summary$last_yr - dates_summary$first_yr >=10 & dates_summary$total_samps >=10] 
df_daily[df_daily$site_no %in% gages_10yrs$site_no,] 

str(df_daily)


#gages 10 years of flow data
gage_record <- daily_data_metrics %>% group_by(site_no) %>% summarise(first_yr = min(wyear),
                                                  last_yr = max(wyear),
                                                  total_samps = sum(unique(wyear)))
gages_10wyr <- gage_record[gage_record$last_yr - gage_record$first_yr >=10 & gage_record$total_samps >=10, ]

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
library(lattice)

?glmer
m1 <- glmer(noflowdays ~  wyear + (1|site_no), family = "poisson", data = daily_data_metrics)
m1 <- lmer(noflowdays ~  wyear + (1|site_no),  data = daily_data_metrics)

ee <- Effect("wyear", m1)
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

unique(daily_data_metrics$site_no) # 359
gage_w_zero <- unique(daily_data_metrics$site_no[daily_data_metrics$noflowdays > 0 ]) # 173

daily_I_gages <- daily_data_metrics[daily_data_metrics$site_no %in% gage_w_zero, ]
daily_I_gages_10wyr <- daily_I_gages[daily_I_gages$site_no %in% gages_10wyr$site_no, ]

daily_I_gages_10wyr$s_wyear <- scale(daily_I_gages_10wyr$wyear)[,1]
str(daily_I_gages$s_wyear)
m2 <- glmer(noflowdays ~  s_wyear + (1|site_no), family = "poisson", data = daily_I_gages_10wyr)
ef <- Effect("s_wyear", m2)
theme_set(theme_bw())
ggplot(as.data.frame(ef),
       aes(s_wyear, fit))+
  geom_line()+
  ## colour=NA suppresses edges of the ribbon
  geom_ribbon(colour=NA,alpha=0.1,
              aes(ymin=lower,ymax=upper))+
  ## add rug plot based on original data
  geom_rug(data=ef$data,aes(y=NULL),sides="b")

m3 <- glmer(noflowdays ~  wyear + (1|site_no), family = "poisson", data = daily_I_gages_10wyr)
ef <- Effect("wyear", m3)
theme_set(theme_bw())
ggplot(as.data.frame(ef),
       aes(wyear, fit))+
  geom_line()+
  ## colour=NA suppresses edges of the ribbon
  geom_ribbon(colour=NA,alpha=0.1,
              aes(ymin=lower,ymax=upper))+
  ## add rug plot based on original data
  geom_rug(data=ef$data,aes(y=NULL),sides="b")

modern_daily_I <- daily_I_gages_10wyr[daily_I_gages_10wyr$wyear >= 1980, ]
m4 <- glmer(noflowdays ~ wyear + (1|site_no), family = "poisson", data = modern_daily_I)
ef <- Effect("wyear", m4)
theme_set(theme_bw())
ggplot(as.data.frame(ef),
       aes(wyear, fit))+
  geom_line()+
  ## colour=NA suppresses edges of the ribbon
  geom_ribbon(colour=NA,alpha=0.1,
              aes(ymin=lower,ymax=upper))+
  ## add rug plot based on original data
  geom_rug(data=ef$data,aes(y=NULL),sides="b")


gage_record_modern <- daily_data_metrics[daily_data_metrics$wyear >= 1980, ] %>% group_by(site_no) %>% summarise(first_yr = min(wyear),
                                                                      last_yr = max(wyear),
                                                                      total_samps = sum(unique(wyear)))
gages_10wyr_modern <- gage_record_modern[gage_record_modern$last_yr - gage_record_modern$first_yr >=10 & gage_record_modern$total_samps >=10, ]

# Should make with 15wyr instead. Found this citation in McManamay 2022
# Kennard, M. J., Mackay, S. J., Pusey, B. J., Olden, J. D. & Marsh, N. 
# Quantifying uncertainty in estimation of hydrologic metrics for ecohydrological studies. 
# River Res. Applic. 26, 137–156, https://doi.org/10.1002/rra.1249 (2010).

### Data subset for analyses -----------
modern_daily_I_10b <- modern_daily_I[modern_daily_I$site_no %in% gages_10wyr_modern$site_no, ] #sites with 10yrs of data that have recorded 0 flow at least once since 1980

 
str(USA_subset)
USA_subset_HUCS <- as.data.frame(cbind(as.character(USA_subset$site_no), USA_subset$huc_cd)) %>% distinct()
colnames(USA_subset_HUCS) <- c("site_no", "huc_cd")
USA_subset_HUCS$huc_cd[USA_subset_HUCS$huc_cd == "0"] <- "16020203"
USA_subset_HUCS[USA_subset_HUCS$huc_cd == "16020203",]



#Why are some HUCS 0? 
# Ok only for site_no 10163000
USA_subset_HUCS$huc_cd[USA_subset_HUCS$huc_cd == "0"] <- "16020203"
USA_subset_HUCS[USA_subset_HUCS$huc_cd == "16020203",]

modern_daily_I_10 <- left_join(modern_daily_I_10b, USA_subset_HUCS, by = "site_no")
modern_daily_I_10[modern_daily_I_10$huc_cd == "0",]

m5 <- glmer(noflowdays ~ s_wyear + (1|site_no), family = "poisson", data = modern_daily_I_10)
summary(m5)
?anova
ef <- Effect("wyear", m5)
theme_set(theme_bw())
ggplot(as.data.frame(ef),
       aes(wyear, fit))+
  geom_line()+
  ## colour=NA suppresses edges of the ribbon
  geom_ribbon(colour=NA,alpha=0.1,
              aes(ymin=lower,ymax=upper))+
  ## add rug plot based on original data
  geom_rug(data=ef$data,aes(y=NULL),sides="b")

# Make a plot of trends by gage. Scatterplot very unclear
plot(noflowdays~wyear, data = daily_data_metrics)


# Example from Effect library
# data(cake, package="lme4")
# fm1 <- lmer(angle ~ recipe * temperature + (1|recipe:replicate), cake,
#             REML = FALSE)
# plot(Effect(c("recipe", "temperature"), fm1))
# 
# plot(effect("recipe:temperature", fm1),
#      axes=list(grid=TRUE)) # equivalent (plus grid)
?glmer
# m6 <- glmer(noflowdays ~ wyear + (wyear|site_no),  family = "poisson", data = modern_daily_I_10)
# m6l <- lmer(noflowdays ~ wyear + (wyear|site_no),  data = modern_daily_I_10)
m6 <- glmer(noflowdays ~ wyear + (wyear|huc_cd),  family = "poisson", data = modern_daily_I_10)
m6l <- lmer(noflowdays ~ wyear + (wyear|huc_cd),  data = modern_daily_I_10)
#str(m6)
?Effect()
?ranef()
ef <- Effect("wyear", m6l)
theme_set(theme_bw())
ggplot(as.data.frame(ef),
       aes(wyear, fit))+
  geom_line()+ 
  ## colour=NA suppresses edges of the ribbon
  geom_ribbon(colour=NA,alpha=0.1,
              aes(ymin=lower,ymax=upper))+
  ## add rug plot based on original data
  geom_rug(data=ef$data,aes(y=NULL),sides="b")

?ranef
rr6 <- ranef(m6)
# How make predictions?
exp(76.97632692 + -0.0365064476*1980) #try transforming b/c log link
76.97632692 + -0.0365064476*1980 #without transformation
exp(76.97632692) + exp(-0.0365064476*1980)# does this work to fit in ggplot framework? same answer? NOPE


ff6 <- fixef(m6)
dotplot(rr6)
qqmath(rr6)
# dotplot(rr6, scales = list(x = list(relation = 'free')))[["site_no"]]
dotplot(rr6, scales = list(x = list(relation = 'free')))[["huc_cd"]]


## as.data.frame() provides RE's and conditional standard deviations:
str(dd <- as.data.frame(rr6))
  ggplot(dd, aes(y=grp,x=condval)) +
    geom_point() + facet_wrap(~term,scales="free_x") +
    geom_errorbarh(aes(xmin=condval -2*condsd,
                       xmax=condval +2*condsd), height=0)

#row.names(rr6$site_no)
#df6 <- cbind(dd$grp[1:142], as.data.frame(rr6$site_no))
row.names(rr6$huc_cd)
df6 <- cbind(dd$grp[1:61], as.data.frame(rr6$huc_cd))


colnames(df6) <- c("huc_cd", "intercept", "slope_wyear")
dff6 <- as.data.frame(df6)

df6v2 <- df6
#str(df6v2$site_no)
str(df6v2$huc_cd)
# df6v2[143, ] <- c(paste0(11111111), ff6[[1]], ff6[[2]])

df6v2$'1980' <- exp(df6v2$intercept + df6v2$slope_wyear*1980) # predicts infinite zero flow days. makes no sense.

wyear_dates <- seq(from = 1980, to = 2020, by = 5)
for (i in 1:length(wyear_dates)){
  df6v2[,(i+3)]<- exp(df6v2$intercept + df6v2$slope_wyear*wyear_dates[i])
}

# colnames(df6v2) <- c("site_no", "intercept", "slope_wyear", "1980", "1985", "1990", "1995", "2000", "2005", "2010", "2015", "2020", "2025")
colnames(df6v2) <- c("huc_cd", "intercept", "slope_wyear", "1980", "1985", "1990", "1995", "2000", "2005", "2010", "2015", "2020")
str(df6v2)


# library(reshape)
#  df6v3 <- melt(df6v2, id.vars = "site_no") # in reshape library

# ?gather
# ?pivot_longer
 df6v3 <- df6v2 %>% pivot_longer(cols = '1980':'2020', names_to = "wyear_dates", values_to = "preds")

# ggplot(modern_daily_I_10) +
#   aes(x = wyear, y = noflowdays) +
#   geom_abline(
#     aes(intercept = intercept, slope = slope_wyear),
#     data = dff6,
#     color = "#3366FF",
#     alpha = .1
#   ) +
#    geom_point() # +
#   # facet_wrap("site_no") +
#   # scale_x_continuous(breaks = 0:4 * 2)
ef$fit
ggplot(as.data.frame(ef),
aes(wyear, fit))+
  geom_line()+ 
  ## colour=NA suppresses edges of the ribbon
  geom_ribbon(colour=NA,alpha=0.1,
              aes(ymin=lower,ymax=upper))+
  ## add rug plot based on original data
  geom_rug(data=ef$data,aes(y=NULL),sides="b") +

  geom_abline(
    aes(intercept = intercept, slope = slope_wyear), 
    data = df6, 
    color = "#3366FF", 
    alpha = .5)

?ggplot()
ggplot(modern_daily_I_10) +
       aes(wyear, noflowdays)+
  geom_point()+
  # ylim(0,25)+
  geom_abline(
    aes(intercept = intercept, slope = slope_wyear), 
    data = df6, 
    color = "#3366FF", 
    alpha = .5)+
  geom_abline(
    aes(intercept = ff6[[1]], slope = ff6[[2]]),
    color = "red",
    size = 2)

ggplot(modern_daily_I_10) +
  aes(wyear, noflowdays)+
  geom_point()+# ylim(0,25)+
  
  geom_line(
    aes(wyear_dates, preds, group = huc_cd), #site_no
    data = as.data.frame(df6v3), 
    color = "#3366FF", 
    alpha = .5)+
  geom_abline(
    aes(intercept = ff6[[1]], slope = ff6[[2]]),
    color = "red",
    size = 2)


main_preds <- c((ff6[[1]]+ ff6[[2]]*1980), (ff6[[1]]+ ff6[[2]]*1985), 
                (ff6[[1]]+ ff6[[2]]*1990), (ff6[[1]]+ ff6[[2]]*1995),
                (ff6[[1]]+ ff6[[2]]*2000), (ff6[[1]]+ ff6[[2]]*2005),
                (ff6[[1]]+ ff6[[2]]*2010), (ff6[[1]]+ ff6[[2]]*2015),
                (ff6[[1]]+ ff6[[2]]*2020) )
main_dates <- c("1980", "1985", "1990", "1995", "2000", "2005", "2010", "2015", "2020")

mod_daily_I_10v2 <- modern_daily_I_10[modern_daily_I_10$wyear %in% wyear_dates, ]

ggplot(as.data.frame(df6v3),
       aes(wyear_dates, preds, group = huc_cd, color =huc_cd))+ # group = site_no
  geom_line()+ 
  theme(legend.position="none") +

  geom_line(aes(rep(main_dates, 61), rep(main_preds, 61)), #122, 142
  
    color = "red",
    size = 1)

str(df6v3)
str(as.data.frame(mod_daily_I_10v2))


ggplot(NULL, aes(x=Year, y=NoFlowDays))+
  geom_line(data = as.data.frame(df6v3),
            aes(x = wyear_dates, y = preds, group = huc_cd, color =huc_cd))+ 
  theme(legend.position="none") +
  ylim(0,400)+
  geom_line(aes(rep(main_dates, 61), rep(main_preds, 61)), #142
            color = "red",
            size = 1)+
  geom_point(data = as.data.frame(modern_daily_I_10),
             aes(x = as.character(wyear), y = noflowdays, group = site_no, color = site_no))

ggplot(data = as.data.frame(df6v3), 
       aes(x=wyear_dates, y=preds,group = huc_cd, color =huc_cd))+
  geom_line()+ 
  theme(legend.position="none") +
  ylim(0,400)+
  geom_line(aes(rep(main_dates, 61), rep(main_preds, 61)),
            color = "red",
            size = 1)+
  geom_point(data = as.data.frame(modern_daily_I_10),
             aes(x= as.character(wyear), y = noflowdays)) #group = site_no, color = "blue"

modern_daily_I_10$wyear

?geom_abline


library(ggeffects) # https://strengejacke.github.io/ggeffects/
# https://strengejacke.github.io/ggeffects/articles/introduction_randomeffects.html

library(sjPlot) # https://strengejacke.github.io/sjPlot/
# # Examples from https://www.r-bloggers.com/2014/11/visualizing-generalized-linear-mixed-effects-models-part-2-rstats-lme4/


plot_model(m6, type = "pred")
plot_model(m6, type = "re", facet.grid = TRUE)

fp <- ggpredict(m6, terms = "wyear", type = "random")
plot(fp, ci = TRUE)

fp <- ggpredict(m6, terms = c("wyear", "site_no"), type = "random")
plot(fp, ci = FALSE)

m7 <- glmer(noflowdays ~ s_wyear + (1 + s_wyear|site_no), family = "poisson", data = modern_daily_I_10)

fp <- ggpredict(m7, terms = "s_wyear", type = "random")
plot(fp, ci = TRUE)

fp <- ggpredict(m6, terms = c("wyear", "site_no [sample = 1]"), type = "random") # NOte the max lines it will plot is 9, something to do with paletted settings on plot I think
fp_main <- ggpredict(m6, terms = "wyear", type = "fixed")
plot(fp, ci = FALSE)
plot(fp, add.data = TRUE, ci = TRUE)
plot(fp_main, add.data = TRUE,  ci = FALSE) #doesn't work
ggpredict
ggpredict_helper()
?predict
fbp <- predict(m6)

fp2 <- ggpredict(m6, terms = c("wyear", "site_no"), type = "random")
plot (fp2)


