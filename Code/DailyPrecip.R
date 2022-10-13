##### Precipitation data
## Code by FER
## last edit 9/9/2022

# load data
dailyprecip <- read.csv("~/Repositories/LargeData/Total_daily_precip_hex_centroids_1979-2022_0.1dd.csv", header = TRUE)

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
  filter(latitude < 0)

USprecip <- dailyprecip2 %>%
  filter(latitude > 0)

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
AUSprecipUSE$hex.id <- as.factor(AUSprecipUSE$hex.id)
USprecipUSE$hex.id <- as.factor(USprecipUSE$hex.id)

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
  group_by(hex.id, latitude, year) %>%
  summarise(total = sum(Precip, na.rm = TRUE),
            intensity = sum(Precip/n(), na.rm = TRUE),
            zeroDays = sum(Precip == 0, na.rm = TRUE))

USprecipUSE3 <- USprecipUSE2 %>%
  group_by(hex.id, latitude, year) %>%
  summarise(total = sum(Precip, na.rm = TRUE),
            intensity = sum(Precip/n(), na.rm = TRUE),
            zeroDays = sum(Precip == 0, na.rm = TRUE))

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

# number of zero precip days?
# total precip


## plot annual precipitation change ----
AUSprecip <- AUSprecipUSE4 %>% 
  group_by(year) %>% 
  mutate(mean.precip= mean(total)) %>% 
  ggplot( aes(x = year, y = total, group = year)) +
  scale_fill_viridis_c(name = "Total precipitation (mm)", direction = -1) +
  geom_boxplot(aes(fill = mean.precip)) +
  theme_classic(base_size = 14) +
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
  xlab("Year") +
  ylab("Total annual precip (mm)") +
  ggtitle("Australia precipitation")
print(AUSprecip)
# ggsave(AUSprecip, filename = "figures/AUSannualprecip_box.png", dpi = 300, height = 6, width = 8)


USprecip <- USprecipUSE3 %>% 
  group_by(year) %>% 
  mutate(mean.precip= mean(total)) %>% 
  ggplot( aes(x = year, y = total, group = year)) +
  scale_fill_viridis_c(name = "Total precipitation (mm)", direction = -1) +
  geom_boxplot(aes(fill = mean.precip)) +
  theme_classic(base_size = 14) +
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
  xlab("Year") +
  ylab("Total annual precip (mm)") +
  ggtitle("US precipitation")
print(USprecip)
# ggsave(USprecip, filename = "figures/USannualprecip_box.png", dpi = 300, height = 6, width = 8)


## plot anomaly in annual precip
AUSanol <- AUSprecipUSE4 %>% 
  group_by(year) %>% 
  mutate(mean.anol= mean(anol)) %>% 
  ggplot( aes(x = year, y = anol, group = year)) +
  scale_fill_viridis_c(name = "Precip anomaly", direction = -1) +
  geom_boxplot(aes(fill = mean.anol)) +
  theme_classic(base_size = 14) +
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
  xlab("Year") +
  ylab("Total precip anomaly (mm)") +
  ggtitle("Australia precip anomaly") +
  geom_hline(yintercept = 0, color = "red", linetype = "dotted", size = 1)
print(AUSanol)
ggsave(AUSanol, filename = "figures/AUSprecipanol_box.png", dpi = 300, height = 6, width = 8)


USanol <- USprecipUSE4 %>% 
  group_by(year) %>% 
  mutate(mean.anol= mean(anol)) %>% 
  ggplot( aes(x = year, y = anol, group = year)) +
  scale_fill_viridis_c(name = "Precip anomaly", direction = -1) +
  geom_boxplot(aes(fill = mean.anol)) +
  theme_classic(base_size = 14) +
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
  xlab("Year") +
  ylab("Total precip anomaly (mm)") +
  ggtitle("United States") +
  geom_hline(yintercept = 0, color = "red", linetype = "dotted", size = 1)
print(USanol)
ggsave(USanol, filename = "figures/USprecipanol_box.png", dpi = 300, height = 6, width = 8)


## plot zero precip days
AUSprecip2 <- AUSprecipUSE3 %>% 
  group_by(year) %>% 
  mutate(mean.zero= mean(zeroDays)) %>% 
  ggplot( aes(x = year, y = zeroDays, group = year)) +
  scale_fill_viridis_c(name = "No. of zero precip days", direction = 1) +
  geom_boxplot(aes(fill = mean.zero)) +
  theme_classic(base_size = 14) +
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
  xlab("Year") +
  ylab("Total number of zero precip days") +
  ggtitle("Australia zero precipitation")
print(AUSprecip2)
ggsave(AUSprecip2, filename = "figures/AUSprecipzero_box.png", dpi = 300, height = 6, width = 8)

USprecip2 <- USprecipUSE3 %>% 
  group_by(year) %>% 
  mutate(mean.zero= mean(zeroDays)) %>% 
  ggplot( aes(x = year, y = zeroDays, group = year)) +
  scale_fill_viridis_c(name = "No. of zero precip days", direction = 1) +
  geom_boxplot(aes(fill = mean.zero)) +
  theme_classic(base_size = 14) +
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
  xlab("Year") +
  ylab("Total number of zero precip days") +
  ggtitle("United States zero precipitation")
print(USprecip2)
ggsave(USprecip2, filename = "figures/USprecipzero_box.png", dpi = 300, height = 6, width = 8)


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
  ylab("Precipitation intensity (total/days with rain)") +
  ggtitle("Australia precipitation intensity")
print(AUSprecipint)
ggsave(AUSprecipint, filename = "figures/AUSprecipint_box.png", dpi = 300, height = 6, width = 8)

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
  mutate(Season = case_when(month %in% 9:11 ~ 'Spring',
                            month %in% 6:8 ~ 'Winter',
                            month %in% 3:5 ~ 'Fall',
                            TRUE ~ 'Summer'))

kendallSeasonalTrendTest(Precip ~ Season + year, data = AUSprecipSeason)

# another option
library(wql)

# examples
# Seasonal Kendall test:
chl <- sfbayChla # monthly chlorophyll at 16 stations in San Francisco Bay
seaKen(sfbayChla[, 's27']) # results for a single series at station 27
seaKen(sfbayChla) # results for all stations
seaKen(sfbayChla, plot=TRUE, type="relative", order=TRUE)

# Regional Kendall test:
# Use mts2ts to change 16 series into a single series with 16 "seasons"
seaKen(mts2ts(chl))  # too many missing data
# better when just Feb-Apr, spring bloom period,
# but last 4 stations still missing too much.
seaKen(mts2ts(chl, seas = 2:4)) 
seaKen(mts2ts(chl[, 1:12], 2:4)) # more reliable result

# restructure data for seaKen
# col = sites
# rows = dates

## want to use this: https://pubs.acs.org/doi/full/10.1021/es051650b
