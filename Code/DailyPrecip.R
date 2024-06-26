##### Precipitation data
## Code by FER
## last edit April 2024

# load data
dailyprecip <- read.csv("~/Repositories/LargeData/Total_daily_precip_Feb2024.csv", header = TRUE)

# load libraries
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(ggridges)
library(ggpubr)

## Data cleaning ----

# remove 2022 data since it is incomplete
dailyprecip2 <- dailyprecip %>%
  select(-contains('X2022'))

# separate US and AUS gages
AUSprecip <- dailyprecip2 %>%
  filter(longitude > 0)

USprecip <- dailyprecip2 %>%
  filter(longitude < 0)


# convert from wide to long format using dplyr
preciplongAUS <- AUSprecip %>%
  tidyr::pivot_longer(
    cols = c("X1979.01.01":"X2021.12.31"),
    names_to = "date",
    values_to = "Precip"
  )
preciplongUS <- USprecip %>%
  tidyr::pivot_longer(
    cols = c("X1979.01.01":"X2021.12.31"),
    names_to = "date",
    values_to = "Precip"
  )

# new dataframe
AUSprecipUSE <- preciplongAUS
USprecipUSE <- preciplongUS


# make habitat and site factors
AUSprecipUSE$hex.id <- as.factor(AUSprecipUSE$X)
USprecipUSE$hex.id <- as.factor(USprecipUSE$X)

# extract month from date 
# format = X1979.01.01
AUSprecipUSE$month <- as.numeric(str_sub(AUSprecipUSE$date, 7, 8))
USprecipUSE$month <- as.numeric(str_sub(USprecipUSE$date, 7, 8))

# extract year
AUSprecipUSE$year <- as.numeric(str_sub(AUSprecipUSE$date, 2, 5))
USprecipUSE$year <- as.numeric(str_sub(USprecipUSE$date, 2, 5))

# remove NA values
AUSprecipUSE2 <- na.omit(AUSprecipUSE)
USprecipUSE2 <- na.omit(USprecipUSE)

### calculate precipitation variables of interest ----
AUSprecipUSE3 <- AUSprecipUSE2 %>%
  group_by(hex.id, longitude, year) %>%
  summarise(total = sum(Precip, na.rm = TRUE),
            intensity = sum(Precip/n(), na.rm = TRUE),
            zeroDays = sum(Precip == 0, na.rm = TRUE))
AUS2007 <- AUSprecipUSE3 %>%
  filter(year > 2006)

USprecipUSE3 <- USprecipUSE2 %>%
  group_by(hex.id, longitude, year) %>%
  summarise(total = sum(Precip, na.rm = TRUE),
            intensity = sum(Precip/n(), na.rm = TRUE),
            zeroDays = sum(Precip == 0, na.rm = TRUE))
US2007 <- USprecipUSE3 %>%
  filter(year > 2006)

## overall averages
AUSall <- AUSprecipUSE3 %>%
  group_by(hex.id) %>%
  summarize(overallavg = mean(total, na.rm = TRUE),
            avginten = mean(intensity, na.rm = TRUE))

USall <- USprecipUSE3 %>%
  group_by(hex.id) %>%
  summarize(overallavg = mean(total, na.rm = TRUE),
            avginten = mean(intensity, na.rm = TRUE))

# merge overall avg with annual average
AUSprecipUSE4 <- left_join(AUSprecipUSE3, AUSall, by = "hex.id")
USprecipUSE4 <- left_join(USprecipUSE3, USall, by = "hex.id")

# calculate annual precipitation total anomaly
AUSprecipUSE4$anol <- AUSprecipUSE4$total - AUSprecipUSE4$overallavg
USprecipUSE4$anol <- USprecipUSE4$total - USprecipUSE4$overallavg



## plot annual precipitation change ----
AUSprecip <- AUSprecipUSE4 %>% 
  group_by(year) %>% 
  mutate(mean.precip= mean(total)) %>% 
  ggplot( aes(x = year, y = total, group = year)) +
  scale_fill_viridis_c(name = "Total precipitation (mm)", direction = -1) +
  geom_boxplot(aes(fill = mean.precip)) +
  theme_classic(base_size = 14) +
  ylim(0,2400) +
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
  xlab("Year") +
  #ylab("Total annual precip (mm)") +
  ylab("") +
  ggtitle("Australia annual precipitation")
print(AUSprecip)
# ggsave(AUSprecip, filename = "figures/AUSannualprecip_box.png", dpi = 300, height = 6, width = 8)


USprecip <- USprecipUSE3 %>% 
  group_by(year) %>% 
  mutate(mean.precip= mean(total)) %>% 
  ggplot( aes(x = year, y = total, group = year)) +
  scale_fill_viridis_c(name = "Total precipitation (mm)", direction = -1) +
  geom_boxplot(aes(fill = mean.precip)) +
  ylim(0,2400) +
  theme_classic(base_size = 14) +
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
  xlab("Year") +
  ylab("Total annual precip (mm)") +
  ggtitle("United States annual precipitation")
print(USprecip)
# ggsave(USprecip, filename = "figures/USannualprecip_box.png", dpi = 300, height = 6, width = 8)

totalprecip <- ggarrange(
  USprecip,
  AUSprecip,
  nrow = 1,
  ncol = 2,
  align = "hv",
  legend = "bottom",
  common.legend = TRUE,
  labels = "AUTO"
)
totalprecip

## plot anomaly in annual precip
AUSanol <- AUSprecipUSE4 %>% 
  group_by(year) %>% 
  mutate(mean.anol= mean(anol)) %>% 
  ggplot( aes(x = year, y = anol, group = year)) +
  scale_fill_viridis_c(name = "Precip anomaly", direction = -1) +
  geom_boxplot(aes(fill = mean.anol)) +
  theme_classic(base_size = 14) +
  ylim(-800, 1250) +
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
  xlab("Year") +
  #ylab("Total precip anomaly (mm)") +
  ylab("") +
  ggtitle("Australia anomaly") +
  geom_hline(yintercept = 0, color = "red", linetype = "dotted", size = 1)
print(AUSanol)
# ggsave(AUSanol, filename = "figures/AUSprecipanol_box.png", dpi = 300, height = 6, width = 8)


USanol <- USprecipUSE4 %>% 
  group_by(year) %>% 
  mutate(mean.anol= mean(anol)) %>% 
  ggplot( aes(x = year, y = anol, group = year)) +
  scale_fill_viridis_c(name = "Avg anomaly (mm)", direction = -1) +
  geom_boxplot(aes(fill = mean.anol)) +
  ylim(-800, 1250) +
  theme_classic(base_size = 14) +
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
  xlab("Year") +
  ylab("Annual precipitation anomaly (mm)") +
  ggtitle("United States anomaly") +
  geom_hline(yintercept = 0, color = "red", linetype = "dotted", size = 1)
print(USanol)
# ggsave(USanol, filename = "figures/USprecipanol_box.png", dpi = 300, height = 6, width = 8)

precip_anol <- ggarrange(
  USanol,
  AUSanol,
  common.legend = TRUE,
  legend = "bottom",
  align = "hv",
  labels = c("C", "D")
)
precip_anol

## plot zero precip days
AUSprecip2 <- AUSprecipUSE3 %>% 
  group_by(year) %>% 
  mutate(mean.zero= mean(zeroDays)) %>% 
  ggplot( aes(x = year, y = zeroDays, group = year)) +
  scale_fill_viridis_c(name = "Avg zero days", direction = 1) +
  geom_boxplot(aes(fill = mean.zero)) +
  theme_classic(base_size = 14) +
  ylim(0, 350) +
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
  xlab("Year") +
  # ylab("Total number of zero precip days") +
  ylab("") +
  ggtitle("Australia zero precipitation days")
print(AUSprecip2)
# ggsave(AUSprecip2, filename = "figures/AUSprecipzero_box.png", dpi = 300, height = 6, width = 8)

USprecip2 <- USprecipUSE3 %>% 
  group_by(year) %>% 
  mutate(mean.zero= mean(zeroDays)) %>% 
  ggplot( aes(x = year, y = zeroDays, group = year)) +
  scale_fill_viridis_c(name = "Avg zero days", direction = 1) +
  geom_boxplot(aes(fill = mean.zero)) +
  ylim(0, 350) +
  theme_classic(base_size = 14) +
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
  xlab("Year") +
  ylab("Annual zero precipitation days") +
  ggtitle("United States zero precipitation days")
print(USprecip2)
# ggsave(USprecip2, filename = "figures/USprecipzero_box.png", dpi = 300, height = 6, width = 8)

zero <- ggarrange(
  USprecip2,
  AUSprecip2,
  nrow = 1,
  ncol = 2,
  align = "hv",
  legend = "bottom",
  common.legend = TRUE,
  labels = c("E", "F")
)
zero

## intensity
## annual precip/number of days with rain
AUSprecipUSE3$intensity <- AUSprecipUSE3$total/(365.25-AUSprecipUSE3$zeroDays)
USprecipUSE3$intensity <- USprecipUSE3$total/(365.25-USprecipUSE3$zeroDays)

# AUS intensity
AUSprecipint <- AUSprecipUSE3 %>% 
  group_by(year) %>% 
  mutate(mean.zero= mean(intensity)) %>% 
  ggplot( aes(x = year, y = intensity, group = year)) +
  scale_fill_viridis_c(name = "Average intensity", direction = 1) +
  geom_boxplot(aes(fill = mean.zero)) +
  theme_classic(base_size = 14) +
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
  xlab("Year") +
  ylim(0, 18) +
  # ylab("Precipitation intensity (total/days with rain)") +
  ylab("") +
  ggtitle("Australia precipitation intensity")
print(AUSprecipint)
# ggsave(AUSprecipint, filename = "figures/AUSprecipint_box.png", dpi = 300, height = 6, width = 8)

# US intensity
USprecipint <- USprecipUSE3 %>% 
  group_by(year) %>% 
  mutate(mean.zero= mean(intensity)) %>% 
  ggplot( aes(x = year, y = intensity, group = year)) +
  scale_fill_viridis_c(name = "Avg intensity", direction = 1) +
  geom_boxplot(aes(fill = mean.zero)) +
  theme_classic(base_size = 14) +
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
  xlab("Year") +
  ylim(0, 18) +
  ylab("Intensity (total mm/days with rain)") +
  ggtitle("United States precipitation intensity")
print(USprecipint)
# ggsave(USprecipint, filename = "figures/USprecipint_box.png", dpi = 300, height = 6, width = 8)

multi_intensity <- ggarrange(
  USprecipint,
  AUSprecipint,
  ncol = 2,
  nrow = 1,
  common.legend = TRUE,
  legend = "bottom",
  labels = c("G", "H")
)
multi_intensity

## Multipanel figure
precipall <- ggarrange(
  totalprecip,
  precip_anol,
  zero,
  multi_intensity,
  nrow = 4,
  ncol = 1,
  align = "hv",
  legend = "bottom",
  common.legend = FALSE
)
precipall
ggsave(precipall, filename = "figures/allprecip.png", dpi = 300, height = 17, width = 15)

## summer precip ----

## seasonal Mann-Kendall trend test ----
# https://www.rdocumentation.org/packages/EnvStats/versions/2.3.1/topics/kendallSeasonalTrendTest
library(EnvStats)
# head(EPA.09.Ex.14.8.df)
# 
# 
# 
# # Plot the data
# #--------------
# Unadj.Conc <- EPA.09.Ex.14.8.df$Unadj.Conc
# Adj.Conc   <- EPA.09.Ex.14.8.df$Adj.Conc
# Month      <- EPA.09.Ex.14.8.df$Month
# Year       <- EPA.09.Ex.14.8.df$Year
# Time       <- paste(substring(Month, 1, 3), Year - 1900, sep = "-")
# n          <- length(Unadj.Conc)
# Three.Yr.Mean <- mean(Unadj.Conc)
# 
# dev.new()
# par(mar = c(7, 4, 3, 1) + 0.1, cex.lab = 1.25)
# plot(1:n, Unadj.Conc, type = "n", xaxt = "n",
#      xlab = "Time (Month)", 
#      ylab = "ANALYTE CONCENTRATION (mg/L)", 
#      main = "Figure 14-15. Seasonal Time Series Over a Three Year Period",
#      cex.main = 1.1)
# axis(1, at = 1:n, labels = rep("", n))
# at <- rep(c(1, 5, 9), 3) + rep(c(0, 12, 24), each = 3)
# axis(1, at = at, labels = Time[at])
# points(1:n, Unadj.Conc, pch = 0, type = "o", lwd = 2)
# points(1:n, Adj.Conc, pch = 3, type = "o", col = 8, lwd = 2)
# abline(h = Three.Yr.Mean, lwd = 2)
# legend("topleft", c("Unadjusted", "Adjusted", "3-Year Mean"), bty = "n", 
#        pch = c(0, 3, -1), lty = c(1, 1, 1), lwd = 2, col = c(1, 8, 1),
#        inset = c(0.05, 0.01))
# 
# # run model
# kendallSeasonalTrendTest(Unadj.Conc ~ Month + Year, 
#                          data = EPA.09.Ex.14.8.df)
# 
# rm(Unadj.Conc, Adj.Conc, Month, Year, Time, n, Three.Yr.Mean, at)
# graphics.off()

AUSprecipUSE2

# assign season
library(tidyverse)    
AUSprecipSeason <- AUSprecipUSE2 %>%
  mutate(Season = as.factor(case_when(month %in% 9:11 ~ 'Spring',
                            month %in% 6:8 ~ 'Winter',
                            month %in% 3:5 ~ 'Fall',
                            TRUE ~ 'Summer')))

# reformat data for analysis
# hex.id = center of polygon
# Precip = daily precip
# Season = season as factor
# month and year
AUSrain <- AUSprecipSeason %>%
  select(hex.id, Precip, month, year, Season) %>%
  group_by(hex.id, month, Season, year) %>%
  summarize(monthlyPrec = sum(Precip),
)

USrain <- USprecipUSE2 %>%
  select(hex.id, Precip, month, year) %>%
  group_by(hex.id, month, year) %>%
  summarize(monthlyPrec = sum(Precip))

# restructure data for seaKen
# col = sites
# rows = month and year
AUSrain2 <- AUSrain %>%
  pivot_wider(names_from = hex.id, values_from = monthlyPrec)

# USrain2 <- USrain %>%
  # pivot_wider(names_from = hex.id, values_from = monthlyPrec)

# start with subset
USrainsub <- USrain2 %>% 
  filter('hex.id' < 100)

# make row a month/year
library(zoo)
AUSrain$Date <- zoo::as.yearmon(paste(AUSrain$year, AUSrain$month), "%Y %m")
USrain$Date <- zoo::as.yearmon(paste(USrain$year, USrain$month), "%Y %m")


# # make row names the month and year
# AUSrain3 <- data.frame(AUSrain2, row.names = 4)

# # remove old month, season, year columns
# AUSrain4 <- AUSrain3[,-c(1:3)]
# 
# test <- kendallSeasonalTrendTest(monthlyPrec ~ Season + year, data = AUSprecipSeason2)
# summary(test)

# convert into a time-series xts object
library(xts)

# AUS
str(AUSrain)
AUSrain3 <- AUSrain[,-c(2:4)]
AUSrain3 
# AUSrainUSE <- xts(x = AUSrain$monthlyPrec, order.by = AUSrain$Date)
# AUSrainUSE  
z <- read.zoo(file = AUSrain3, index.column = "Date", split = "hex.id")
z
# convert to ts object for analysis
z2 <- as.ts(z)

## make US data into xts object
str(USrain)
USrain2 <- USrain[,-c(2:3)]
y <- read.zoo(file = USrain2, index.column = "Date", split = "hex.id")
y
y2 <- as.ts(y)

# another option
# https://jsta.github.io/wql/reference/seaKen.html
library(wql)

# test Seasonal Kendall test
seaKen(z2)
seaKen(z2, plot = TRUE, type = "slope", order = TRUE, xlab = "Sen slope", ylab = "AUS Hexagon ID")

seaKen(y2)
seaKen(
  y2,
  plot = TRUE,
  type = "relative",
  order = TRUE,
  xlab = "Sen slope",
  ylab = "US Hexagon ID",
  yaxt="n"
)

# Seasonal Kendall test:
chl <- sfbayChla # monthly chlorophyll at 16 stations in San Francisco Bay
# seaKen(sfbayChla[, 's27']) # results for a single series at station 27
# seaKen(sfbayChla) # results for all stations
# seaKen(sfbayChla, plot=TRUE, type="relative", order=TRUE)

# Regional Kendall test:
# Use mts2ts to change 16 series into a single series with 16 "seasons"
seaKen(mts2ts(z2))  # AUS
seaKen(mts2ts(y2)) # US

# Separate by seasons
seaKen(mts2ts(z2, seas = c(12,1:2))) # summer AUS
seaKen(mts2ts(z2, seas = c(9:11))) # spring AUS
seaKen(mts2ts(z2, seas = c(3:5))) # fall AUS
seaKen(mts2ts(z2, seas = c(6:8))) # winter AUS

seaKen(mts2ts(y2, seas = c(12,1:2))) # winter US
seaKen(mts2ts(y2, seas = c(9:11))) # fall US
seaKen(mts2ts(y2, seas = c(3:5))) # spring US
seaKen(mts2ts(y2, seas = c(6:8))) # summer US




seaKen(mts2ts(AUSrain4))
## want to use this: https://pubs.acs.org/doi/full/10.1021/es051650b


#### ----
# check out 'trend' package

