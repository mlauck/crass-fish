# climate data for AUS
dailytemp <- read.csv("Data/Avg_daily_temp_fish_sites_1979-2022_NAremoved.csv", header = TRUE)

# libraries
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(ggridges)

# convert from wide to long format using dplyr
templong <- dailytemp %>%
  tidyr::pivot_longer(
    cols = c("X1979.01.01":"X2022.01.01"),
    names_to = "date",
    values_to = "TempC"
  )

summary(templong)

# new dataframe
tempUSE <- templong[,c(5:8, 125:126)]

# make habitat and site factors
tempUSE$Site <- as.factor(tempUSE$Site)
tempUSE$habitat <- as.factor(tempUSE$habitat)

# extract month from date 
# format = X1979.01.01
tempUSE$month <- as.numeric(str_sub(tempUSE$date, 7, 8))

# extract year
tempUSE$year <- as.numeric(str_sub(tempUSE$date, 2, 5))

# eliminate 2022 because fewer data points
tempUSE <- filter(tempUSE, year != 2022)

# ## save this as rda
# saveRDS(tempUSE, "Data/tempUSE.rda")
# 
# # load data
# load(file = 'Data/tempUSE.rda')

## make average monthly temperature by site
## for Dec-Feb
tempsummer <- tempUSE %>%
  group_by(Site, decimalLatitude, year) %>%
  filter(month == 12 | month == 1 | month == 2) %>%
  summarise(avgTemp = mean(TempC, na.rm = TRUE),
            sdTemp = sd(TempC, na.rm = TRUE),
            n = n())

## for Nov-Feb
tempsummer <- tempUSE %>%
  group_by(Site, decimalLatitude, year) %>%
  filter(month == 11 | month == 12 | month == 1 | month == 2) %>%
  summarise(avgTemp = mean(TempC, na.rm = TRUE),
            sdTemp = sd(TempC, na.rm = TRUE),
            n = n())

## Nov only (start of summer -- is it getting earlier?)
tempnov <- tempUSE %>%
  group_by(Site, decimalLatitude, year) %>%
  filter(month == 11) %>%
  summarise(avgTemp = mean(TempC, na.rm = TRUE),
            sdTemp = sd(TempC, na.rm = TRUE),
            n = n())

# # temp year
# tempyear <- tempUSE %>%
#   group_by(Site, year) %>%
#   summarise(avgTemp = mean(TempC, na.rm = TRUE),
#             sdTemp = sd(TempC, na.rm = TRUE),
#             n = n())

# All time average summer temp C
tempall <- tempsummer %>%
  group_by(Site) %>%
  summarize(overallavg = mean(avgTemp, na.rm = TRUE))

# All time Nov temp C
tempall <- tempnov %>%
  group_by(Site) %>%
  summarize(overallavg = mean(avgTemp, na.rm = TRUE))

# merge overall avg with annual average
tempsummer2 <- left_join(tempsummer, tempall, by = "Site")
tempnov2 <- left_join(tempnov, tempall, by = "Site")

# calculate annual anomaly
tempsummer2$anol <- tempsummer2$overallavg - tempsummer2$avgTemp
tempnov2$anol <- tempnov2$overallavg - tempnov$avgTemp

## plot temp anomaly
tempanol <- ggplot(tempsummer2, aes(x = year, y = anol, group = year)) +
  # geom_point(pch = 21, fill = "gray50", alpha = 0.5) +
  geom_boxplot() +
  theme_classic(base_size = 14) +
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
  xlab("Year") +
  ylab("Summer temperature anomaly (°C)") +
  ggtitle("Annual summer temperature anomaly") +
  geom_hline(yintercept = 0, color = "red", linetype = "dotted", size = 1)
print(tempanol)


# look at temp by site
ggplot(tempsummer2, aes(x = anol, y = as.factor(year), fill = stat(x))) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, gradient_lwd = 1.) +
  scale_fill_viridis_c(name = "Temp anomaly", option = "C") +
  scale_color_viridis_c(name = "Temp anomaly", option = "C") +
  ylab("Year") +
  xlab("Average summer temperature anomaly for Nov-Feb") +
  labs(
    title = 'Australia summer temperature averages',
    subtitle = 'Mean temperature anomaly (°C) from 1979-2021'
  ) +
  theme_ridges(font_size = 14, grid = FALSE) + 
  theme(axis.title.y = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dotted", size = 1)

## only November temp anomalies
ggplot(tempnov2, aes(x = anol, y = as.factor(year), fill = stat(x))) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, gradient_lwd = 1.) +
  scale_fill_viridis_c(name = "Temp anomaly", option = "C") +
  scale_color_viridis_c(name = "Temp anomaly", option = "C") +
  ylab("Year") +
  xlab("Temperature anomaly for November") +
  labs(
    title = 'Australia November temperatures',
    subtitle = 'Mean temperature anomaly (°C) from 1979-2021'
  ) +
  theme_ridges(font_size = 14, grid = FALSE) + 
  theme(axis.title.y = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dotted", size = 1)
