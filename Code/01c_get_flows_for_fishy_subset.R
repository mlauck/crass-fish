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

###
df_daily_info %>% group_by(site_no) %>% summarise(first_yr = min(year),
                                last_yr = max(year),
                                total_samps = sum(unique(year)))


###################################################################################################################
### OVERALL TREND ANALYSIS ###
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

modern_daily_I_10 <- modern_daily_I[modern_daily_I$site_no %in% gages_10wyr_modern$site_no, ]
m5 <- glmer(noflowdays ~ wyear + (1|site_no), family = "poisson", data = modern_daily_I_10)
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
m6 <- glmer(noflowdays ~ wyear + (wyear|site_no),  family = "poisson", data = modern_daily_I_10)
m6l <- lmer(noflowdays ~ wyear + (wyear|site_no),  data = modern_daily_I_10)
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
fixef(m6l)
dotplot(rr6)
qqmath(rr6)
dotplot(rr6, scales = list(x = list(relation = 'free')))[["site_no"]]

library(lattice) ## for dotplot, qqmath
## as.data.frame() provides RE's and conditional standard deviations:
str(dd <- as.data.frame(rr6))
  ggplot(dd, aes(y=grp,x=condval)) +
    geom_point() + facet_wrap(~term,scales="free_x") +
    geom_errorbarh(aes(xmin=condval -2*condsd,
                       xmax=condval +2*condsd), height=0)

row.names(rr6$site_no)
df6 <- cbind(dd$grp[1:142], as.data.frame(rr6$site_no))

colnames(df6) <- c("site_no", "intercept", "slope_wyear")
dff6 <- as.data.frame(df6)

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
  geom_abline(
    aes(intercept = -1309.790310, slope = 0.698083),
    color = "#3366FF",
    size = 2
    
  ) + 
  geom_abline(
    aes(intercept = intercept, slope = slope_wyear), 
    data = df6, 
    color = "#3366FF", 
    alpha = .5)


library(ggeffects) # https://strengejacke.github.io/ggeffects/
# https://strengejacke.github.io/ggeffects/articles/introduction_randomeffects.html

library(sjPlot) # https://strengejacke.github.io/sjPlot/
# # Examples from https://www.r-bloggers.com/2014/11/visualizing-generalized-linear-mixed-effects-models-part-2-rstats-lme4/


plot_model(m6l, type = "pred")
plot_model(m6l, type = "re", facet.grid = FALSE)

fp <- ggpredict(m6, terms = "wyear", type = "random")
plot(fp, ci = FALSE)

fp <- ggpredict(m6l, terms = c("wyear", "site_no"), type = "random")
plot(fp, ci = FALSE)

m7 <- glmer(noflowdays ~ s_wyear + (1 + s_wyear|site_no), family = "poisson", data = modern_daily_I_10)

fp <- ggpredict(m7, terms = "s_wyear", type = "random")
plot(fp, ci = TRUE)

fp <- ggpredict(m6, terms = c("wyear", "site_no [sample = 9]"), type = "random") # NOte the max lines it will plot is 9, something to do with paletted settings on plot I think
fp_main <- ggpredict(m6, terms = "wyear", type = "random")
plot(fp, ci = FALSE)
plot(fp, add.data = TRUE, ci = FALSE)
plot(fp_main, add.dat = TRUE,  ci = FALSE) #doesn't work
ggpredict

fp2 <- ggpredict(m6, terms = c("wyear", "site_no"), type = "random")
plot (fp2)

#ugh next time just use stanR and Freya's code this is ridiculous
# https://www.tjmahr.com/plotting-partial-pooling-in-mixed-effects-models/
# "PartialPoolingExample.R"
## Rstanarm partial pooling model

## example pulled from https://www.tjmahr.com/plotting-partial-pooling-in-mixed-effects-models/

## libraries
library(rstanarm)



# OTHER THINGS I TRIED. THe ondes that aren't hashed out kind of worked----------------------------------------------------
# http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#predictions-andor-confidence-or-prediction-intervals-on-predictions

# Another way - maybe more flexible to plot all - DOESN"T WORK
# https://drizopoulos.github.io/GLMMadaptive/articles/Methods_MixMod.html

# A manual way to do it
# from https://stackoverflow.com/questions/53255211/plotting-random-effects-for-a-binomial-glmer-in-ggplot
# https://stackoverflow.com/questions/51519690/convert-cbind-format-for- binomial-glm-in-r-to-a-dataframe-with-individual-rows
#


