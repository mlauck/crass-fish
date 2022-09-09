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

# number of zero precip days?
# total precip
# summer precip?
# intensity

AUSprecipUSE2 <- AUSprecipUSE2 %>%
  group_by(hex.id, latitude, year) %>%
  summarise(total = sum(Precip, na.rm = TRUE),
            intensity = sum(Precip/n(), na.rm = TRUE))

USprecipUSE2 <- USprecipUSE2 %>%
  group_by(hex.id, latitude, year) %>%
  summarise(total = sum(Precip, na.rm = TRUE),
            intensity = sum(Precip/n(), na.rm = TRUE))


## plot annual precipitation change ----
AUSprecip <- AUSprecipUSE2 %>% 
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
ggsave(AUSprecip, filename = "figures/AUSannualprecip_box.png", dpi = 300, height = 5, width = 6)


USprecip <- USprecipUSE2 %>% 
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
ggsave(USprecip, filename = "figures/USannualprecip_box.png", dpi = 300, height = 5, width = 6)
