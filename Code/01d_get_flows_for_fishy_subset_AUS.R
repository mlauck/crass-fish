### Jane S. Rogosch
### Created 15 Dec 2022
### Updated with all xeric gages 20 Dec 2023 (but not necessarily any having sampled fish, using all because sample size is so low)
### This code is to extract AUS gage discharge data from subset of gages that are in watershed where
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
### Load the file with the subset of gages that are in watersheds with fish data ------------------------------------------------------
# AUS_GRDC_subset <- read.csv("Data/from_GIS/AUS_gage_wfish_table.csv", row.names = 1)
#                        # colClasses = c(rep(NA, 12), "character", rep(NA, 24)))
AUS_xeric <- read.csv("Output/Gage_location_AUS_xeric.csv")
AUS_CAMEL_subset <- read.csv("Data/from_GIS/AUS_CAMEL_gage_wfish_table.csv", row.names = 1)

### Load discharge files
CAMELS_streamflow <- read.csv("Data/AUS_data/03_streamflow/streamflow_MLd.csv")

# Get the files we want
# column name is "site_no" for GRDC
# for CAMEL us "station_id" and lat_outlet and long_outle for the dec_lat_va and dec_long_v
?

temp <- list.files(path="Data/GRDC_data/Australia/", pattern="_Q_Day.Cmd.txt")
temp_xer <-temp[str_sub(temp, start = 1, end = 7) %in% AUS_xeric$site_no]
AUS_gage_xer <- lapply(paste0("Data/GRDC_data/Australia/",temp_xer), read.delim)
str(AUS_gage_xer)
AUS_gage_xer

library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
# 
# AUS_gage_files[[254]]
# 
# unlist(strsplit(substr(AUS_gage_files[[254]][c(37:nrow(AUS_gage_files[[254]])), ], 1, 31), "   "))
# 
# AUS_split <- strsplit(substr(AUS_gage_files[[254]][c(37:nrow(AUS_gage_files[[254]])), ], 1, 31), "    ")
# AUS_gage_sub <- data.frame(matrix(unlist(AUS_split), nrow=length(AUS_split), byrow=TRUE),
#            stringsAsFactors=FALSE)
# colnames(AUS_gage_sub) <- c("Date", "discharge")
# 
# ??str_sub
# # AUS_split <- cbind(str_sub(AUS_gage_files[[254]][c(37:nrow(AUS_gage_files[[254]])), ], start = 1, end = 10),
# #                        str_sub(AUS_gage_files[[254]][c(37:nrow(AUS_gage_files[[254]])), ], start = -7 ))
# # AUS_gage_sub <- as.data.frame(AUS_split)
# # colnames(AUS_gage_sub) <- c("Date", "discharge")
# ?as_date
# x <- AUS_gage_files[[252]]
test_list <- lapply(AUS_gage_xer, function(x) data.frame(site_no = str_sub(x[8, ], start = -7),
                                                       Date = lubridate::as_date(str_sub(x[c(37:nrow(x)), ], start = 1, end = 10)),
                                                       X_00060_00003 = as.numeric(str_sub(x[c(37:nrow(x)), ], start = -7)),
                                                       dec_lat_va = as.numeric(str_sub(x[12, ], start = -10)),
                                                       dec_long_v = as.numeric(str_sub(x[13, ], start = -9))) )
str(test_list)



#?as.data.frame
# AUS_gage_sub <- lapply(test_list, function(x) as.data.frame(x)) #units are cms cubic meters per second
AUS_gage_sub <- test_list

# AUS_CAMEL_subset 
# # station_id                                        station_na      drainage_d                 river_regi    notes lat_outlet
# 0   A0020101         Diamantina River at Birdsville (A0020101) Lake Eyre Basin DIAMANTINA-GEORGINA RIVERS No notes  -25.90880
# 1   A0030501 Cooper Creek at Cullyamurra Water Hole (A0030501) Lake Eyre Basin  COOPER CREEK-BULLOO RIVER No notes  -27.70130
# 2   G0010005       Ranken River at Soudan Homestead (G0010005) Lake Eyre Basin DIAMANTINA-GEORGINA RIVERS No notes  -20.04528
# 3   G0050115      Hugh River at South Road Crossing (G0050115) Lake Eyre Basin DIAMANTINA-GEORGINA RIVERS No notes  -24.35167
# 4   G0060005       Trephina Creek at Trephina Gorge (G0060005) Lake Eyre Basin DIAMANTINA-GEORGINA RIVERS No notes  -23.53361
head(CAMELS_streamflow)

CAM <- CAMELS_streamflow %>% dplyr::select(year, month, day, A0020101, A0030501, G0010005, G0050115, G0060005) #units are MLd Mega Liters per day
# 1 megalitres per day to cubic metre/second = 0.01157 cubic metre/second
?paste
?unite
CAM <- unite(CAM, Date, 1:3, sep = "-")
CAM$Date <- ymd(CAM$Date)

# Separate sites into their own dataframes with site name, discharge, date
# Omit rows with missing values (i.e. -99.99)
CAM_A0020101 <- CAM[CAM$A0020101 != -99.99, 1:2]
CAM_A0030501 <- CAM[CAM$A0030501 != -99.99, c(1, 3)]
CAM_G0010005 <- CAM[CAM$G0010005 != -99.99, c(1, 4)]
CAM_G0050115 <- CAM[CAM$G0050115 != -99.99, c(1, 5)]
CAM_G0060005 <- CAM[CAM$G0060005 != -99.99, c(1, 6)]


# Convert to cms
CAM_cms <- CAM[,2:6]*0.01157
CAM_A0020101$cms <- CAM_A0020101[,2]*0.01157
CAM_A0030501$cms <- CAM_A0030501[,2]*0.01157
CAM_G0010005$cms <- CAM_G0010005[,2]*0.01157
CAM_G0050115$cms <- CAM_G0050115[,2]*0.01157
CAM_G0060005$cms <- CAM_G0060005[,2]*0.01157


### Next steps
### Get daily discharge for the sites in this subset (AUS_gage_sub and CAM objects)
### Format data: Date format YYYY-MM-DD Discharge format is cubic meters per second (convert to cfs? or does it matter for unitless metrics?)
### If need to have same column heading they are "site_no", "Date", "X00060_00003"

# Get rid of 999s in AUS_gage_sub
#AUS_gage_sub$X_00060_00003[AUS_gage_sub$X_00060_00003 == 999] <- NA # doesn't work for list, put in loop.


AUS_gage_sub[[6]][99:110,3]
# str(compDailyData) # from USA to compare
# head(rawDailyData) # from USA to compare
# str(rawDailyData)
# ??rawDailyData
# rawDailyData[is.na(rawDailyData$X_00060_00003) ==TRUE,]
# i=18
# rawDailyData2<-readNWISdv(site_IDs[i], parameterCd, startDate = "", endDate = "")
# str(rawDailyData2[is.na(rawDailyData2$X_00060_00003) ==TRUE,])


str(AUS_gage_sub)
head(AUS_gage_sub)
length(AUS_gage_sub)
AUS_gage_sub[[6]]$X_00060_00003[AUS_gage_sub[[6]]$X_00060_00003 == 999] #<- NA
#AUS_gage_sub[6]$X_00060_00003[AUS_gage_sub[6]$X_00060_00003 == 999]

i = 6
str(data.frame(AUS_gage_sub[[i]][ ,2:3]))


AUS_gage_sub[[i]][ ,2:3]
### GET NAAs----------------------------------------------------------------------------------
?asStreamflow
CompSignal <- list() 
#i = 6
for (i in 1:length(AUS_gage_sub)) { 
  # AUS_gage_sub[[i]]$X_00060_00003[AUS_gage_sub[[i]]$X_00060_00003 == 999] <- NA # something about running this and generating NA's not happy
  # for next time https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/NA
  #is.na(AUS_gage_sub[[i]]$X_00060_00003[AUS_gage_sub[[i]]$X_00060_00003 == 999])
  is.na(AUS_gage_sub[[i]]$X_00060_00003) <- AUS_gage_sub[[i]]$X_00060_00003 == 999
  AUS_gage_df_update <- data.frame(AUS_gage_sub[[i]])
  streamflow <- asStreamflow(AUS_gage_df_update[ ,2:3], river.name = AUS_gage_df_update[1,1], max.na = 6403) # Convert raw data to 'asStreamflow' object
  summary(streamflow)
  AUS_stream <- fourierAnalysis(streamflow) # Run Fourier on the 'asStreamflow' object
  CompSignal[[i]] <- cbind(AUS_stream$signal, site_no = streamflow$name)
  # Make a file of the main data
  write.csv(cbind(AUS_stream$signal, site_no = streamflow$name), file = paste0('Output/NAA/Individual_AUS_gage_NAA_data/', 'AUS', '_', AUS_gage_df_update$site_no[1],'.csv'))
  # plot characteristic hydrograph
  #   # dots are daily values, the red line is the long-term seasonal profile (integrates the 3 significant signals)
  plot(AUS_stream)
}



# Make things not a list

signal_daily <- bind_rows(CompSignal)
# write.csv(signal_daily, file = "Output/NAA/AUS_signal_daily_update.csv")
head(AUS_gage_sub)
df_daily <- bind_rows(AUS_gage_sub)
str(df_daily)
head(df_daily)
unique(df_daily$site_no)

#write.csv(df_daily[,1:3], file = "Output/Discharge_winwatershedfish_AUS_update.csv")

CAM_A0020101
CAM_A0030501 
CAM_G0010005 
CAM_G0050115 
CAM_G0060005 

CompSignal_CAM <- list()

# This is not how you're supposed to code....copy paste for every CAM gage
streamflow <- asStreamflow(CAM_G0050115[ ,c(1,3)], river.name = "G0050115", max.na = 4500) # Convert raw data to 'asStreamflow' object
summary(streamflow)
AUS_stream <- fourierAnalysis(streamflow) # Run Fourier on the 'asStreamflow' object
CompSignal_CAM[[4]] <- cbind(AUS_stream$signal, site_no = streamflow$name)
# Make a file of the main data
  # write.csv(cbind(AUS_stream$signal, site_no = streamflow$name), file = paste0('Output/NAA/Individual_AUS_gage_NAA_data/', 'AUS', '_', CompSignal_CAM[[4]]$site_no[1],'.csv'))
# plot characteristic hydrograph
#   # dots are daily values, the red line is the long-term seasonal profile (integrates the 3 significant signals)
plot(AUS_stream)


signal_daily2 <- bind_rows(CompSignal, CompSignal_CAM)
# write.csv(signal_daily2, file = "Output/NAA/AUS_signal_daily_subandCAM_update.csv")

?pivot_longer
head(df_daily) # the bound AUS_gage_sub
df_daily[df_daily$X_00060_00003 == 999,]
is.na(df_daily$X_00060_00003) <- df_daily$X_00060_00003 == 999
df_daily[is.na(df_daily$X_00060_00003),]

CAM2 <- CAM %>% pivot_longer(cols = A0020101:G0060005, names_to = "site_no", values_to = "X_00060_00003")
CAM3 <- CAM2 %>% dplyr::select(site_no, Date, X_00060_00003)
is.na(CAM3$X_00060_00003) <- CAM3$X_00060_00003 < 0
CAM3$X_00060_00003 <- CAM3$X_00060_00003*0.01157
CAM5 <- CAM3 %>% arrange(site_no, Date)

df_daily2 <- bind_rows(df_daily[,1:3], CAM5)
#write.csv(df_daily2, file = "Output/Discharge_winwatershedfish_AUS_subandCAM_update.csv")

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

df_daily <- read.csv("Output/Discharge_winwatershedfish_AUS_subandCAM_update.csv", colClasses = c(NA, "character", rep(NA,3)))
signal_daily <- read.csv("Output/NAA/AUS_signal_daily_subandCAM_update.csv", row.names = 1, colClasses = c(rep(NA,12), "character") )
str(signal_daily)


df_daily2 <- df_daily2  %>% mutate(year = year(Date))
str(df_daily2)
unique(df_daily2$site_no)

yr_count <- df_daily2 %>% group_by(site_no) %>% summarise(yrs_record = n_distinct(year))
yr_count
yr_count[which(yr_count$yrs_record < 30),] # 33 of 34 have long records yay!


# # What dates do I need for each gage?
# # Result is gages corresponding to fish data points that have 10 or more fish samples and 10 or more years of discharge data.
# dates_summary <- AUS_subset %>% group_by(site_no) %>% summarise(first_yr = min(Year_First),
#                                                                 last_yr = max(Year_Last_),
#                                                                 total_samps = sum(Total_Samp))
# min(dates_summary$first_yr)
# date_summary <- dates_summary[dates_summary$last_yr - dates_summary$first_yr >=10 & dates_summary$total_samps >=10, ] # 37 -Zipper et al. use gages with more than 10 years (for future citation justification)
#                                                                      # we can use gages with more than 10 years that have fish
# dates_summary[dates_summary$last_yr - dates_summary$first_yr >=30 & dates_summary$total_samps >=10, ] # 10 for 30 years, -for 20 yrs there are 21, for 15 yrs there are 24
# 
# hist(date_summary$first_yr)

# ANNUAL METRICS for all gages
###################################################################################################################
head(df_daily)
df_daily_info <- df_daily2[ ,1:4] %>% mutate(month = month(Date),
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

dailydata_10permissing <- subset(dailydatacount, dailydatacount$count < 330) #75 rows



# Omit years with 10% or more discharge data missing
daily_data_metrics <- subset(dailydatacount, dailydatacount$count > 330)

###
summary_firstlast <- df_daily_info %>% group_by(site_no) %>% summarise(first_yr = min(year),
                                last_yr = max(year),
                                total_samps = sum(unique(year)))

max(summary_firstlast$last_yr)
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
# Because first year of fish data is 1954. But we want 1980
df.noflowmets <- as.data.frame(noflowmets)


str(df.noflowmets)


# Since 1980 Only ##################################################################
# 1982
daily_data_metrics
noflow_periods_metrics 


# start with apprpriate metric
#noflowmets <- arrange(daily_data_metrics[ , c("site_no", "wyear", "zeroflowfirst_wy") ], wyear) %>% pivot_wider(names_from = site_no, values_from = zeroflowfirst_wy)
#noflowmets <- arrange(daily_data_metrics[ , c("site_no", "wyear", "noflowdays") ], wyear) %>% pivot_wider(names_from = site_no, values_from = noflowdays)
noflowmets <- arrange(noflow_periods_metrics[ , c("site_no", "wyear", "max_length_noflow") ], wyear) %>% pivot_wider(names_from = site_no, values_from = max_length_noflow)

tail(noflowmets)

df.noflowmets <- as.data.frame(noflowmets)

df.noflowmets_1980 <- as.data.frame(noflowmets[noflowmets$wyear >= 1980, ])


df.noflowmets_1980[is.na(df.noflowmets_1980)] <- 0

# get rid of columns with NAs
###df.noflowmets_1980b <- df.noflowmets_1980 %>% select_if(~ !any(is.na(.)))


sen_1980 <- data.frame(matrix(nrow = ncol(df.noflowmets_1980[-1]), ncol = 5)) #359,108
colnames(sen_1980) <- c('gageID', 'slope', 'p', 'lci', 'uci')
sen_1980$gageID <- colnames(df.noflowmets_1980[-1]) ###df.noflowmets_1980b[-1]
# slope = double(),
#                        p = double(),
#                        lci = double(),
#                        uci = double())

sen_1980$slope<- apply(df.noflowmets_1980[,-1], 2, function(x) sens.slope(x)$estimates)
sen_1980$p <- apply(df.noflowmets_1980[,-1], 2, function(x) sens.slope(x)$p.value)
sen_1980$lci <- apply(df.noflowmets_1980[-1], 2, function(x) sens.slope(x)$conf.int[1])
sen_1980$uci <- apply(df.noflowmets_1980[-1], 2, function(x) sens.slope(x)$conf.int[2])
mean(sen_1980$slope)
median(sen_1980$p, na.rm = TRUE)
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
1-P



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
length(unique(signal_daily$site_no)) #==should be 34
head(signal_daily)
?Date
??Year
library(ggplot2)
library(lubridate)
NAA <- signal_daily[signal_daily$year >= 1980, ] %>% 
  group_by(site_no, year) %>% 
  summarise(NAA= sum(resid.sig, na.rm = TRUE))

cc <- signal_daily[signal_daily$year >= 1980, ] %>% 
  group_by(site_no, year) %>% 
  summarise(NAA= sum(resid.sig, na.rm = TRUE)) %>% 
  group_by(year) %>% 
  mutate(mean_NAA= mean(NAA)) 


# plot avg discharge anomaly boxplots with fill 
NAA_plot <- signal_daily[signal_daily$year >= 1980, ] %>% 
  group_by(site_no, year) %>% 
  summarise(NAA= sum(resid.sig, na.rm = TRUE)) %>% 
  group_by(year) %>% 
  mutate(mean_NAA= mean(NAA)) %>% 
  ggplot( aes(x = year, y = NAA, group = year) )+  ylim(-500, 500) +
  scale_fill_viridis_c(name = "Discharge anomaly", option = "C") +
  geom_boxplot(aes(fill = mean_NAA)) +
  theme_classic(base_size = 14) +
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
  xlab("Year") +
  ylab("Net annual discharge anomaly") +
  ggtitle("AUS") +
  geom_hline(yintercept = 0, color = "red", linetype = "dotted", size = 1) 
print(NAA_plot)
ggsave(NAA_plot, filename = "figures/Discharge_NAA_box_AUS.jpg", dpi = 300, height = 5, width = 7)

# No, this is not right.
# NAA_summ <- NAA %>%
#   group_by(year) %>%
#   summarise(mean_NAA= mean(NAA))
# sens.slope(NAA_summ$mean_NAA)

#alternatively and more accurately
NAA_wide <- pivot_wider(NAA, names_from = site_no, values_from = NAA)
#NAA_wider <- NAA_wide %>% select_if(~ !any(is.na(.)))
#NAA_wider <- NAA_wide %>% na.omit(.)
NAA_wider <- replace(NAA_wide,is.na(NAA_wide),0)


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

