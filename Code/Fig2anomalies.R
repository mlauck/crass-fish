# Figure 2 code
# annual precip, temp, and discharge

# require
# DailyTemp.R
# DailyPrecip.R

# libraries
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(ggridges)
library(ggpubr)

## annual temperature anomaly ----
# climate data
dailytemp <- read.csv("~/Repositories/LargeData/Avg_daily_temp_Feb2024_NAremoved.csv", header = TRUE)
dailyprecip <- read.csv("~/Repositories/LargeData/Total_daily_precip_Feb2024.csv", header = TRUE)

# huge data frame. Try filtering out some values up front
dailytemp2 <- dailytemp %>%
  select(-contains('X2023'))

# separate US and AUS gages
AUStemp <- dailytemp2 %>%
  filter(longitude > 0)

UStemp <- dailytemp2 %>%
  filter(longitude < 0)

# remove 2022 data since it is incomplete
dailyprecip2 <- dailyprecip %>%
  select(-contains('X2022'))

# separate US and AUS gages
AUSprecip <- dailyprecip2 %>%
  filter(longitude > 0)

USprecip <- dailyprecip2 %>%
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

## make average monthly temperature by site

### Annual
AUStemp <- AUStempUSE2 %>%
  group_by(hex.id, longitude, year) %>%
  summarise(avgTemp = mean(TempC, na.rm = TRUE),
            sdTemp = sd(TempC, na.rm = TRUE),
            n = n())

AUStempavg <- AUStemp |>
  group_by(year) |>
  summarize(avgTempAll = mean(avgTemp, na.rm = TRUE))

UStemp <- UStempUSE2 %>%
  group_by(hex.id, longitude, year) %>%
  summarise(avgTemp = mean(TempC, na.rm = TRUE),
            sdTemp = sd(TempC, na.rm = TRUE),
            n = n())

#### calculate annual avgs
tempall <- AUStemp %>%
  group_by(hex.id) %>%
  summarize(overallavg = mean(avgTemp, na.rm = TRUE))
tempallUS <- UStemp %>%
  group_by(hex.id) %>%
  summarize(overallavg = mean(avgTemp, na.rm = TRUE))

### merge overall avg with annual average
temp <- left_join(AUStemp, tempall, by = "hex.id")
UStemp <- left_join(UStemp, tempallUS, by = "hex.id")

### calculate annual anomaly
temp$anol <- temp$avgTemp - temp$overallavg
UStemp$anol <- UStemp$avgTemp - UStemp$overallavg

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

USprecipUSE3 <- USprecipUSE2 %>%
  group_by(hex.id, longitude, year) %>%
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
















### plots a and b - temperature anomalies ----
AUSanol<- temp %>% 
  group_by(year) %>% 
  mutate(mean.anol= mean(anol)) %>% 
  ggplot( aes(x = year, y = anol, group = year)) +
  scale_fill_viridis_c(name = "Anomaly", option = "C") +
  geom_boxplot(aes(fill = mean.anol)) +
  theme_classic(base_size = 14) +
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
  xlab("Year") +
  ylim(-3, 4) +
  # ylab("Summer temperature anomaly (°C)") +
  ylab("") +
  ggtitle("Australia") +
  geom_hline(yintercept = 0, color = "red", linetype = "dotted", size = 1)
print(AUSanol)


USanol<- UStemp %>% 
  group_by(year) %>% 
  mutate(mean.anol= mean(anol)) %>% 
  ggplot( aes(x = year, y = anol, group = year)) +
  scale_fill_viridis_c(name = "Anomaly", option = "C") +
  geom_boxplot(aes(fill = mean.anol)) +
  theme_classic(base_size = 14) +
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
  xlab("Year") +
  ylim(-3, 4) +
  ylab("Temperature anomaly (°C)") +
  ggtitle("United States") +
  geom_hline(yintercept = 0, color = "red", linetype = "dotted", size = 1)
print(USanol)

## make multipanel of temp
anoltempmulti <- ggarrange(labels = "auto",
                       align = "hv",
                       USanol, 
                       AUSanol, 
                       nrow = 1, 
                       common.legend = TRUE, 
                       legend = "right") 
print(anoltempmulti)



## plots c and d = precipitation anomalies
## plot anomaly in annual precip
AUSprecipanol <- AUSprecipUSE4 %>% 
  group_by(year) %>% 
  mutate(mean.anol= mean(anol)/10) %>% 
  ggplot( aes(x = year, y = anol/10, group = year)) +
  scale_fill_viridis_c(name = "Anomaly", direction = -1) +
  geom_boxplot(aes(fill = mean.anol)) +
  theme_classic(base_size = 14) +
  ylim(-80, 125) +
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
  xlab("Year") +
  #ylab("Total precip anomaly (mm)") +
  ylab("") +
  # ggtitle("Australia anomaly") +
  geom_hline(yintercept = 0, color = "red", linetype = "dotted", size = 1)
print(AUSprecipanol)
# ggsave(AUSanol, filename = "figures/AUSprecipanol_box.png", dpi = 300, height = 6, width = 8)


USprecipanol <- USprecipUSE4 %>% 
  group_by(year) %>% 
  mutate(mean.anol= mean(anol)/10) %>% 
  ggplot( aes(x = year, y = anol/10, group = year)) +
  scale_fill_viridis_c(name = "Anomaly", direction = -1) +
  geom_boxplot(aes(fill = mean.anol)) +
  ylim(-80.0, 125.0) +
  theme_classic(base_size = 14) +
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
  xlab("Year") +
  ylab("Annual precipitation anomaly (cm)") +
  # ggtitle("United States anomaly") +
  geom_hline(yintercept = 0, color = "red", linetype = "dotted", size = 1)
print(USprecipanol)
# ggsave(USanol, filename = "figures/USprecipanol_box.png", dpi = 300, height = 6, width = 8)





## make multipanel
anolprecipmulti <- ggarrange(labels = c("c", "d"),
                           align = "hv",
                           USprecipanol, 
                           AUSprecipanol, 
                           nrow = 1, 
                           common.legend = TRUE, 
                           legend = "right") 
print(anolprecipmulti)





### multi of multi

# work on cowplot https://genchanghsu.github.io/ggGallery/posts/2021-12-20-post-9-arrange-multiple-ggplots/
## make multipanel
anolmulti <- ggarrange(anoltempmulti + labs(x = ""),
                       anolprecipmulti,
                       align = "hv",
                       nrow = 2, 
                       # ncol = 2,
                       common.legend = FALSE,
                       legend = "right") 
print(anolmulti)
ggsave(anolmulti, filename = "figures/Fig2.png", dpi = 450, height = 9, width = 14)
