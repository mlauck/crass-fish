# climate data for AUS
dailytemp <- read.csv("Data/Avg_daily_temp_hex_centroids_1979-2022_0.1dd.csv", header = TRUE)

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
tempUSE <- templong[,c(2:4, 121:122)

# # change NaN into NA
# tempUSE[is.nan(tempUSE$TempC)]<-NA
# View(tempUSE)

# make habitat and site factors
tempUSE$hex.id <- as.factor(tempUSE$hex.id)
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
  group_by(hex.id, latitude, year) %>%
  filter(month == 12 | month == 1 | month == 2) %>%
  summarise(avgTemp = mean(TempC, na.rm = TRUE),
            sdTemp = sd(TempC, na.rm = TRUE),
            n = n())

## for Nov-Feb
tempsummer <- tempUSE %>%
  group_by(hex.id, latitude, year) %>%
  filter(month == 11 | month == 12 | month == 1 | month == 2) %>%
  summarise(avgTemp = mean(TempC, na.rm = TRUE),
            sdTemp = sd(TempC, na.rm = TRUE),
            n = n())
tempsummer$avgTemp[is.nan(tempsummer$avgTemp)]<-NA
View(tempsummer)

## Nov only (start of summer -- is it getting earlier?)
tempnov <- tempUSE %>%
  group_by(hex.id, latitude, year) %>%
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
## Do I need to take the average of averages?
tempall <- tempsummer %>%
  group_by(hex.id) %>%
  summarize(overallavg = mean(avgTemp, na.rm = TRUE))

# All time Nov temp C
tempallNov <- tempnov %>%
  group_by(hex.id) %>%
  summarize(overallavg = mean(avgTemp, na.rm = TRUE))

# merge overall avg with annual average
tempsummer2 <- left_join(tempsummer, tempall, by = "hex.id")
tempsummer3 <- na.omit(tempsummer2)
tempnov2 <- left_join(tempnov, tempallNov, by = "hex.id")

# calculate annual anomaly
tempsummer2$anol <- tempsummer2$overallavg - tempsummer2$avgTemp
tempnov2$anol <- tempnov2$overallavg - tempnov$avgTemp
tempnov2$avgTemp[is.nan(tempnov2$avgTemp)]<-NA


## plot temp anomaly

# plot avg temp anomaly boxplots with fill
AUSsumm <- tempsummer3 %>% 
  group_by(year) %>% 
  mutate(mean.anol= mean(anol)) %>% 
  ggplot( aes(x = year, y = anol, group = year)) +
  # stat_summary(fun.y= "mean",
  #              aes(y=mean.anol,color=mean.anol)) +
  scale_fill_viridis_c(name = "Temp anomaly", option = "C") +
  geom_boxplot(aes(fill = mean.anol)) +
  theme_classic(base_size = 14) +
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
  xlab("Year") +
  ylab("Summer temperature anomaly (°C)") +
  scale_fill_viridis_c(name = "Temp anomaly", option = "C") +
  ggtitle("AUS summer temperature anomaly") +
  geom_hline(yintercept = 0, color = "red", linetype = "dotted", size = 1)
ggsave(AUSsumm, filename = "figures/AUSsummertemp_box.png", dpi = 300, height = 5, width = 6)

# look at temp by year with grouped sites
AUStempridges <- ggplot(tempsummer2, aes(x = anol, y = as.factor(year), fill = stat(x))) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, gradient_lwd = 1.) +
  scale_fill_viridis_c(name = "Temp anomaly", option = "C") +
  scale_color_viridis_c(name = "Temp anomaly", option = "C") +
  ylab("Year") +
  xlab("Temperature anomaly for Dec-Feb") +
  labs(
    title = 'Australia summer temperature averages',
    subtitle = 'Mean temperature anomaly (°C) from 1979-2021'
  ) +
  theme_ridges(font_size = 14, grid = FALSE) + 
  theme(axis.title.y = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dotted", size = 1)
ggsave(AUStempridges, filename = "figures/AUS_summertemp_ridges.png", dpi = 300, height = 10, width = 5)

## only November temp anomalies
AUStempNov <- ggplot(tempnov2, aes(x = anol, y = as.factor(year), fill = stat(x))) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, gradient_lwd = 1.) +
  scale_fill_viridis_c(name = "Temp anomaly", option = "C") +
  scale_color_viridis_c(name = "Temp anomaly", option = "C") +
  ylab("Year") +
  xlab("Temperature anomaly") +
  labs(
    title = 'AUS November temperatures',
    subtitle = 'Mean temperature anomaly (°C) from 1979-2021'
  ) +
  theme_ridges(font_size = 14, grid = FALSE) + 
  theme(axis.title.y = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dotted", size = 1)
ggsave(AUStempNov, filename = "figures/AUS_Novtemp_ridges.png", dpi = 300, height = 10, width = 5)