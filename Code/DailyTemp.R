##### Temperature data
## Code by FER
## last edit April 2024


# climate data
dailytemp <- read.csv("~/Repositories/LargeData/Avg_daily_temp_Feb2024_NAremoved.csv", header = TRUE)


# libraries
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(ggridges)
library(ggpubr)

# huge data frame. Try filtering out some values up front
dailytemp2 <- dailytemp %>%
  select(-contains('X2023'))

# separate US and AUS gages
AUStemp <- dailytemp2 %>%
  filter(longitude > 0)

UStemp <- dailytemp2 %>%
  filter(longitude < 0)

# convert from wide to long format using dplyr
templongAUS <- AUStemp %>%
  tidyr::pivot_longer(
    cols = c("X1979.01.01":"X2021.12.31"),
    names_to = "date",
    values_to = "TempC"
  )
templongUS <- UStemp %>%
  tidyr::pivot_longer(
    cols = c("X1979.01.01":"X2021.12.31"),
    names_to = "date",
    values_to = "TempC"
  )


# new dataframe
AUStempUSE <- templongAUS
UStempUSE <- templongUS

# # change NaN into NA
# tempUSE[is.nan(tempUSE$TempC)]<-NA
# View(tempUSE)

# make habitat and site factors
AUStempUSE$hex.id <- as.factor(AUStempUSE$X.1)
UStempUSE$hex.id <- as.factor(UStempUSE$X.1)

# extract month from date 
# format = X1979.01.01
AUStempUSE$month <- as.numeric(str_sub(AUStempUSE$date, 7, 8))
UStempUSE$month <- as.numeric(str_sub(UStempUSE$date, 7, 8))

# extract year
AUStempUSE$year <- as.numeric(str_sub(AUStempUSE$date, 2, 5))
UStempUSE$year <- as.numeric(str_sub(UStempUSE$date, 2, 5))

# NA omit
AUStempUSE2 <- na.omit(AUStempUSE)
UStempUSE2 <- na.omit(UStempUSE)

# ## save this as rda
# saveRDS(tempUSE, "Data/tempUSE.rda")
# 
# # load data
# load(file = 'Data/tempUSE.rda')

## make average monthly temperature by site

### Australia
## for Dec-Feb
tempsummer <- AUStempUSE2 %>%
  group_by(hex.id, longitude, year) %>%
  filter(month == 12 | month == 1 | month == 2) %>%
  summarise(avgTemp = mean(TempC, na.rm = TRUE),
            sdTemp = sd(TempC, na.rm = TRUE),
            n = n())

tempspring <- AUStempUSE2 %>%
  group_by(hex.id, longitude, year) %>%
  filter(month == 9 | month == 10 | month == 11) %>%
  summarise(avgTemp = mean(TempC, na.rm = TRUE),
            sdTemp = sd(TempC, na.rm = TRUE),
            n = n())

tempwinterAUS <- AUStempUSE2 %>%
  group_by(hex.id, longitude, year) %>%
  filter(month == 6 | month == 7 | month == 8) %>%
  summarise(avgTemp = mean(TempC, na.rm = TRUE),
            sdTemp = sd(TempC, na.rm = TRUE),
            n = n())

tempfallAUS <- AUStempUSE2 %>%
  group_by(hex.id, longitude, year) %>%
  filter(month == 3 | month == 4 | month == 5) %>%
  summarise(avgTemp = mean(TempC, na.rm = TRUE),
            sdTemp = sd(TempC, na.rm = TRUE),
            n = n())

### US
tempsummerUS <- UStempUSE2 %>%
  group_by(hex.id, longitude, year) %>%
  filter(month == 6 | month == 7 | month == 8) %>%
  summarise(avgTemp = mean(TempC, na.rm = TRUE),
            sdTemp = sd(TempC, na.rm = TRUE),
            n = n())

tempspringUS <- UStempUSE2 %>%
  group_by(hex.id, longitude, year) %>%
  filter(month == 3 | month == 4 | month == 5) %>%
  summarise(avgTemp = mean(TempC, na.rm = TRUE),
            sdTemp = sd(TempC, na.rm = TRUE),
            n = n())

tempwinterUS <- UStempUSE2 %>%
  group_by(hex.id, longitude, year) %>%
  filter(month == 12 | month == 1 | month == 2) %>%
  summarise(avgTemp = mean(TempC, na.rm = TRUE),
            sdTemp = sd(TempC, na.rm = TRUE),
            n = n())

tempfallUS <- UStempUSE2 %>%
  group_by(hex.id, longitude, year) %>%
  filter(month == 9 | month == 10 | month == 11) %>%
  summarise(avgTemp = mean(TempC, na.rm = TRUE),
            sdTemp = sd(TempC, na.rm = TRUE),
            n = n())

# Summer averages
tempall <- tempsummer %>%
  group_by(hex.id) %>%
  summarize(overallavg = mean(avgTemp, na.rm = TRUE))
tempallUS <- tempsummerUS %>%
  group_by(hex.id) %>%
  summarize(overallavg = mean(avgTemp, na.rm = TRUE))

# Spring averages
tempSp_all <- tempspring %>%
  group_by(hex.id) %>%
  summarize(overallavg = mean(avgTemp, na.rm = TRUE))
tempSp_allUS <- tempspringUS %>%
  group_by(hex.id) %>%
  summarize(overallavg = mean(avgTemp, na.rm = TRUE))

# winter averages
tempWi_all <- tempwinterAUS %>%
  group_by(hex.id) %>%
  summarize(overallavg = mean(avgTemp, na.rm = TRUE))
tempWi_allUS <- tempwinterUS %>%
  group_by(hex.id) %>%
  summarize(overallavg = mean(avgTemp, na.rm = TRUE))

# fall averages
tempFa_all <- tempfallAUS %>%
  group_by(hex.id) %>%
  summarize(overallavg = mean(avgTemp, na.rm = TRUE))
tempFa_allUS <- tempfallUS %>%
  group_by(hex.id) %>%
  summarize(overallavg = mean(avgTemp, na.rm = TRUE))


### merge overall avg with annual average
## summer
tempsummer2 <- left_join(tempsummer, tempall, by = "hex.id")
tempsummer2US <- left_join(tempsummerUS, tempallUS, by = "hex.id")

## spring
tempspring2 <- left_join(tempspring, tempSp_all, by = "hex.id")
tempspring2US <- left_join(tempspringUS, tempSp_allUS, by = "hex.id")

## winter
tempwinter2 <- left_join(tempwinterAUS, tempWi_all, by = "hex.id")
tempwinter2US <- left_join(tempwinterUS, tempWi_allUS, by = "hex.id")

## fall
tempfall2 <- left_join(tempfallAUS, tempFa_all, by = "hex.id")
tempfall2US <- left_join(tempfallUS, tempFa_allUS, by = "hex.id")



### calculate annual anomaly
# summer
tempsummer2$anol <- tempsummer2$avgTemp - tempsummer2$overallavg
tempsummer2US$anol <- tempsummer2US$avgTemp - tempsummer2US$overallavg

# spring
tempspring2$anol <- tempspring2$avgTemp - tempspring2$overallavg
tempspring2US$anol <- tempspring2US$avgTemp - tempspring2US$overallavg

# fall
tempfall2$anol <- tempfall2$avgTemp - tempfall2$overallavg
tempfall2US$anol <- tempfall2US$avgTemp - tempfall2US$overallavg

# winter
tempwinter2$anol <- tempwinter2$avgTemp - tempwinter2$overallavg
tempwinter2US$anol <- tempwinter2US$avgTemp - tempwinter2US$overallavg

# # remove NAs
# tempnov3 <- na.omit(tempnov2)
# tempsummer3 <- na.omit(tempsummer2)

# #### plot summer temp  ----
# 
# # plot avg temp anomaly boxplots with fill 
# AUSrawsumm <- tempsummer2 %>% 
#   group_by(year) %>% 
#   mutate(mean.temp= mean(avgTemp)) %>% 
#   ggplot( aes(x = year, y = avgTemp, group = year)) +
#   scale_fill_viridis_c(name = "Avg summer temperature", option = "C") +
#   geom_boxplot(aes(fill = mean.temp)) +
#   theme_classic(base_size = 14) +
#   theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
#   xlab("Year") +
#   # ylab("Summer temperature (°C)") +
#   ylab("") +
#   ggtitle("Australia summer temperature")
# print(AUSrawsumm)
# # ggsave(AUSsumm, filename = "figures/AUSsummertemp_box.png", dpi = 300, height = 5, width = 6)
# 
# USrawsumm <- tempsummer2US %>% 
#   group_by(year) %>% 
#   mutate(mean.temp= mean(avgTemp)) %>% 
#   ggplot( aes(x = year, y = avgTemp, group = year)) +
#   geom_boxplot(aes(fill = mean.temp)) +
#   theme_classic(base_size = 14) +
#   theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
#   xlab("Year") +
#   ylab("Summer temperature (°C)") +
#   scale_fill_viridis_c(name = "Avg summer temperature", option = "C") +
#   ggtitle("United States summer temperatures")
# print(USrawsumm)
# # ggsave(USsumm, filename = "figures/USsummertemp_box.png", dpi = 300, height = 5, width = 6)


# ## overall temp
# 
# # AUS
# AUStempUSE2 <- na.omit(AUStempUSE)
# AUSraw <- AUStempUSE2 %>% 
#   group_by(year) %>% 
#   mutate(mean.temp= mean(TempC)) %>% 
#   ggplot( aes(x = year, y = TempC, group = year)) +
#   scale_fill_viridis_c(name = "Avg temperature", option = "C") +
#   geom_boxplot(aes(fill = mean.temp)) +
#   theme_classic(base_size = 14) +
#   theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
#   xlab("Year") +
#   # ylab("Summer temperature (°C)") +
#   ylab("") +
#   ggtitle("Australia temperature")
# print(AUSraw)

#### plot temp anomaly ----

## summer temperature anomaly
# plot avg temp anomaly boxplots with fill 
AUSsumm <- tempsummer2 %>% 
  group_by(year) %>% 
  mutate(mean.anol= mean(anol)) %>% 
  ggplot( aes(x = year, y = anol, group = year)) +
  scale_fill_viridis_c(name = "Temp anomaly", option = "C") +
  geom_boxplot(aes(fill = mean.anol)) +
  theme_classic(base_size = 14) +
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
  xlab("Year") +
  # ylab("Summer temperature anomaly (°C)") +
  ylab("") +
  ggtitle("Australia summer anomaly") +
  geom_hline(yintercept = 0, color = "red", linetype = "dotted", size = 1)
print(AUSsumm)
# ggsave(AUSsumm, filename = "figures/AUSsummertemp_box.png", dpi = 300, height = 5, width = 6)

USsumm <- tempsummer2US %>% 
  group_by(year) %>% 
  mutate(mean.anol= mean(anol)) %>% 
  ggplot( aes(x = year, y = anol, group = year)) +
  geom_boxplot(aes(fill = mean.anol)) +
  theme_classic(base_size = 14) +
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
  xlab("Year") +
  ylab("Summer anomaly (°C)") +
  scale_fill_viridis_c(name = "Avg anomaly", option = "C") +
  ggtitle("United States summer anomaly") +
  geom_hline(yintercept = 0, color = "red", linetype = "dotted", size = 1)
print(USsumm)
# ggsave(USsumm, filename = "figures/USsummertemp_box.png", dpi = 300, height = 5, width = 6)


## make multipanel
tempanolmulti <- ggarrange(labels = c("C", "D"),
                           align = "hv",
                           USsumm, 
                           AUSsumm, 
                           nrow = 1, 
                           common.legend = TRUE, 
                           legend = "bottom") 
print(tempanolmulti)

# ggsave(tempanolmulti, filename = "figures/combined_temp.png", height = 5, width = 12, dpi = 300)



####### spring temperature anomaly
# plot avg temp anomaly boxplots with fill 
AUSsp <- tempspring2 %>% 
  group_by(year) %>% 
  mutate(mean.anol= mean(anol)) %>% 
  ggplot( aes(x = year, y = anol, group = year)) +
  scale_fill_viridis_c(name = "Temp anomaly", option = "C") +
  geom_boxplot(aes(fill = mean.anol)) +
  theme_classic(base_size = 14) +
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
  xlab("Year") +
  # ylab("Summer temperature anomaly (°C)") +
  ylab("") +
  ggtitle("Australia spring anomaly") +
  geom_hline(yintercept = 0, color = "red", linetype = "dotted", size = 1)
print(AUSsp)
# ggsave(AUSsp, filename = "figures/AUSspringtemp_box.png", dpi = 300, height = 5, width = 6)

USsp <- tempspring2US %>% 
  group_by(year) %>% 
  mutate(mean.anol= mean(anol)) %>% 
  ggplot( aes(x = year, y = anol, group = year)) +
  geom_boxplot(aes(fill = mean.anol)) +
  theme_classic(base_size = 14) +
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
  xlab("Year") +
  ylab("Spring anomaly (°C)") +
  scale_fill_viridis_c(name = "Avg anomaly", option = "C") +
  ggtitle("United States spring anomaly") +
  geom_hline(yintercept = 0, color = "red", linetype = "dotted", size = 1)
print(USsp)
# ggsave(USsumm, filename = "figures/USsummertemp_box.png", dpi = 300, height = 5, width = 6)

# spmod <- lm(anol ~ year, data = tempspring2)
# summary(spmod)
# plot(spmod)

## make multipanel
tempanolspring <- ggarrange(labels = c("A", "B"),
                           align = "hv",
                           USsp, 
                           AUSsp, 
                           nrow = 1, 
                           common.legend = TRUE, 
                           legend = "bottom") 
print(tempanolspring)


####### fall temperature anomaly
# plot avg temp anomaly boxplots with fill 
AUSfa <- tempfall2 %>% 
  group_by(year) %>% 
  mutate(mean.anol= mean(anol)) %>% 
  ggplot( aes(x = year, y = anol, group = year)) +
  scale_fill_viridis_c(name = "Temp anomaly", option = "C") +
  geom_boxplot(aes(fill = mean.anol)) +
  theme_classic(base_size = 14) +
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
  xlab("Year") +
  # ylab("Fall temperature anomaly (°C)") +
  ylab("") +
  ggtitle("Australia fall anomaly") +
  geom_hline(yintercept = 0, color = "red", linetype = "dotted", size = 1)
print(AUSfa)
# ggsave(AUSfa, filename = "figures/AUSfalltemp_box.png", dpi = 300, height = 5, width = 6)

USfa <- tempfall2US %>% 
  group_by(year) %>% 
  mutate(mean.anol= mean(anol)) %>% 
  ggplot( aes(x = year, y = anol, group = year)) +
  geom_boxplot(aes(fill = mean.anol)) +
  theme_classic(base_size = 14) +
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
  xlab("Year") +
  ylab("Fall anomaly (°C)") +
  scale_fill_viridis_c(name = "Avg anomaly", option = "C") +
  ggtitle("United States fall anomaly") +
  geom_hline(yintercept = 0, color = "red", linetype = "dotted", size = 1)
print(USfa)
# ggsave(USfa, filename = "figures/USfalltemp_box.png", dpi = 300, height = 5, width = 6)

####### winter temperature anomaly
# plot avg temp anomaly boxplots with fill 
AUSwi <- tempwinter2 %>% 
  group_by(year) %>% 
  mutate(mean.anol= mean(anol)) %>% 
  ggplot( aes(x = year, y = anol, group = year)) +
  scale_fill_viridis_c(name = "Temp anomaly", option = "C") +
  geom_boxplot(aes(fill = mean.anol)) +
  theme_classic(base_size = 14) +
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
  xlab("Year") +
  # ylab("Winter temperature anomaly (°C)") +
  ylab("") +
  ggtitle("Australia winter anomaly") +
  geom_hline(yintercept = 0, color = "red", linetype = "dotted", size = 1)
print(AUSwi)
# ggsave(AUSwi, filename = "figures/AUSwintertemp_box.png", dpi = 300, height = 5, width = 6)

USwi <- tempwinter2US %>% 
  group_by(year) %>% 
  mutate(mean.anol= mean(anol)) %>% 
  ggplot( aes(x = year, y = anol, group = year)) +
  geom_boxplot(aes(fill = mean.anol)) +
  theme_classic(base_size = 14) +
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
  xlab("Year") +
  ylab("Winter anomaly (°C)") +
  scale_fill_viridis_c(name = "Avg anomaly", option = "C") +
  ggtitle("United States winter anomaly") +
  geom_hline(yintercept = 0, color = "red", linetype = "dotted", size = 1)
print(USwi)
# ggsave(USfa, filename = "figures/USfalltemp_box.png", dpi = 300, height = 5, width = 6)



## multimulti ----
## Multipanel figure
tempall <- ggarrange(
  USsp + theme(axis.title.x=element_blank()),
  AUSsp + theme(axis.title.x=element_blank()),
  USsumm + theme(axis.title.x=element_blank()),
  AUSsumm + theme(axis.title.x=element_blank()),
  USfa + theme(axis.title.x=element_blank()),
  AUSfa + theme(axis.title.x=element_blank()), 
  USwi,
  AUSwi,
  nrow = 4,
  ncol = 2,
  labels = "AUTO",
  align = "hv",
  legend = "bottom",
  common.legend = TRUE
)
tempall
ggsave(tempall, filename = "figures/alltemp.png", dpi = 600, height = 16, width = 14)



########
## seasonal Mann-Kendall trend test ----
# https://www.rdocumentation.org/packages/EnvStats/versions/2.3.1/topics/kendallSeasonalTrendTest
library(EnvStats)

## make dataframe for model
AUStempUSE2

# assign season
library(tidyverse)    
AUStempSeason <- AUStempUSE2 %>%
  mutate(Season = as.factor(case_when(month %in% 9:11 ~ 'Spring',
                                      month %in% 6:8 ~ 'Winter',
                                      month %in% 3:5 ~ 'Fall',
                                      TRUE ~ 'Summer')))

UStempSeason <- UStempUSE %>%
  mutate(Season = as.factor(case_when(month %in% 9:11 ~ 'Fall',
                                      month %in% 6:8 ~ 'Summer',
                                      month %in% 3:5 ~ 'Spring',
                                      TRUE ~ 'Summer')))

# reformat data for analysis
# hex.id = center of polygon
# Precip = daily precip
# Season = season as factor
# month and year
AUStempF <- AUStempSeason %>%
  select(hex.id, TempC, month, year, Season) %>%
  group_by(hex.id, month, Season, year) %>%
  summarize(TempF = mean(TempC*9/5+32)
  )
UStempF <- UStempSeason %>%
  select(hex.id, TempC, month, year, Season) %>%
  group_by(hex.id, month, Season, year) %>%
  summarize(TempF = mean(TempC*9/5+32)
  )


# make row a month/year
library(zoo)
AUStempF$Date <- zoo::as.yearmon(paste(AUStempF$year, AUStempF$month), "%Y %m")
UStempF$Date <- zoo::as.yearmon(paste(UStempF$year, UStempF$month), "%Y %m")


# convert into a time-series xts object
library(xts)

# AUS
str(AUStempF)
str(UStempF)

# make AUS tempF into xts object
AUStempF3 <- AUStempF[,-c(2:4)]
Faus<- read.zoo(file = AUStempF3, index.column = "Date", split = "hex.id")
Faus2 <- as.ts(Faus)

UStempF2 <- na.omit(UStempF)
UStempF3 <- UStempF2[,-c(2:4)]
Fus<- read.zoo(file = UStempF3, index.column = "Date", split = "hex.id")
Fus2 <- as.ts(Fus)

# another option
# https://jsta.github.io/wql/reference/seaKen.html
library(wql)

# test Seasonal Kendall test by site in AUS
seaKen(Faus2)
seaKen(
  Faus2,
  plot = TRUE,
  type = "slope",
  order = TRUE,
  xlab = "Sen slope for temperature",
  ylab = "AUS Hexagon ID"
)

# US
seaKen(Fus2)
seaKen(
  Fus2,
  plot = TRUE,
  type = "relative",
  order = TRUE,
  xlab = "Sen slope for temperature",
  ylab = "US Hexagon ID",
  yaxt="n"
)

# # Seasonal Kendall test:
# chl <- sfbayChla # monthly chlorophyll at 16 stations in San Francisco Bay
# # seaKen(sfbayChla[, 's27']) # results for a single series at station 27
# # seaKen(sfbayChla) # results for all stations
# # seaKen(sfbayChla, plot=TRUE, type="relative", order=TRUE)

# Regional Kendall test:
# Use mts2ts to change 16 series into a single series with 16 "seasons"
seaKen(mts2ts(Faus2))  # AUS
seaKen(mts2ts(Fus2)) # US

# Separate by seasons
seaKen(mts2ts(Faus2, seas = c(12,1:2))) # summer AUS
seaKen(mts2ts(Faus2, seas = c(9:11))) # spring AUS
seaKen(mts2ts(Faus2, seas = c(3:5))) # fall AUS
seaKen(mts2ts(Faus2, seas = c(6:8))) # winter AUS


seaKen(mts2ts(Fus2, seas = c(3:5))) # spring US
seaKen(mts2ts(Fus2, seas = c(6:8))) # summer US
seaKen(mts2ts(Fus2, seas = c(9:11))) # fall US
seaKen(mts2ts(Fus2, seas = c(12,1:2))) # winter US



## want to use this: https://pubs.acs.org/doi/full/10.1021/es051650b


#### ----
# check out 'trend' package