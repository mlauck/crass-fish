# climate data for AUS
dailytemp <- read.csv("~/Repositories/LargeData/Avg_daily_temp_fish_sites_1979-2022_NAremoved.csv", header = TRUE)

# libraries
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(ggridges)
library(ggpubr)

# huge data frame. Try filtering out some values up front
dailytemp2 <- dailytemp %>%
  select(-contains('X2022'))

# separate US and AUS gages
AUStemp <- dailytemp2 %>%
  filter(latitude < 0)

UStemp <- dailytemp2 %>%
  filter(latitude > 0)

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
AUStempUSE$hex.id <- as.factor(AUStempUSE$hex.id)
UStempUSE$hex.id <- as.factor(UStempUSE$hex.id)

# extract month from date 
# format = X1979.01.01
AUStempUSE$month <- as.numeric(str_sub(AUStempUSE$date, 7, 8))
UStempUSE$month <- as.numeric(str_sub(UStempUSE$date, 7, 8))

# extract year
AUStempUSE$year <- as.numeric(str_sub(AUStempUSE$date, 2, 5))
UStempUSE$year <- as.numeric(str_sub(UStempUSE$date, 2, 5))


# ## save this as rda
# saveRDS(tempUSE, "Data/tempUSE.rda")
# 
# # load data
# load(file = 'Data/tempUSE.rda')

## make average monthly temperature by site
## for Dec-Feb
tempsummer <- AUStempUSE %>%
  group_by(hex.id, latitude, year) %>%
  filter(month == 12 | month == 1 | month == 2) %>%
  summarise(avgTemp = mean(TempC, na.rm = TRUE),
            sdTemp = sd(TempC, na.rm = TRUE),
            n = n())

tempsummerUS <- UStempUSE %>%
  group_by(hex.id, latitude, year) %>%
  filter(month == 6 | month == 7 | month == 8) %>%
  summarise(avgTemp = mean(TempC, na.rm = TRUE),
            sdTemp = sd(TempC, na.rm = TRUE),
            n = n())

## for Nov-Feb and May - August
# tempsummer <- tempUSE %>%
#   group_by(hex.id, latitude, year) %>%
#   filter(month == 11 | month == 12 | month == 1 | month == 2) %>%
#   summarise(avgTemp = mean(TempC, na.rm = TRUE),
#             sdTemp = sd(TempC, na.rm = TRUE),
#             n = n())
# tempsummer$avgTemp[is.nan(tempsummer$avgTemp)]<-NA
# View(tempsummer)
# 
# tempsummerUS <- UStempUSE %>%
#   group_by(hex.id, latitude, year) %>%
#   filter(month == 6 | month == 7 | month == 8 | month == 5) %>%
#   summarise(avgTemp = mean(TempC, na.rm = TRUE),
#             sdTemp = sd(TempC, na.rm = TRUE),
#             n = n())
# tempsummer$avgTemp[is.nan(tempsummer$avgTemp)]<-NA

# ## Nov only (start of summer -- is it getting earlier?)
# tempnov <- tempUSE %>%
#   group_by(hex.id, latitude, year) %>%
#   filter(month == 11) %>%
#   summarise(avgTemp = mean(TempC, na.rm = TRUE),
#             sdTemp = sd(TempC, na.rm = TRUE),
#             n = n())
# 
# # # temp year
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
tempallUS <- tempsummerUS %>%
  group_by(hex.id) %>%
  summarize(overallavg = mean(avgTemp, na.rm = TRUE))

# # All time Nov temp C
# tempallNov <- tempnov %>%
#   group_by(hex.id) %>%
#   summarize(overallavg = mean(avgTemp, na.rm = TRUE))

# merge overall avg with annual average
tempsummer2 <- left_join(tempsummer, tempall, by = "hex.id")
# tempnov2 <- left_join(tempnov, tempallNov, by = "hex.id")

tempsummer2US <- left_join(tempsummerUS, tempallUS, by = "hex.id")

# calculate annual anomaly
tempsummer2$anol <- tempsummer2$avgTemp - tempsummer2$overallavg
# tempnov2$anol <- tempnov$avgTemp - tempnov2$overallavg 
tempnov2$avgTemp[is.nan(tempnov2$avgTemp)]<-NA

tempsummer2US$anol <- tempsummer2US$avgTemp - tempsummer2US$overallavg

# remove NAs
tempnov3 <- na.omit(tempnov2)
tempsummer3 <- na.omit(tempsummer2)

#### plot temp anomaly ----

# plot avg temp anomaly boxplots with fill 
AUSsumm <- tempsummer3 %>% 
  group_by(year) %>% 
  mutate(mean.anol= mean(anol)) %>% 
  ggplot( aes(x = year, y = anol, group = year)) +
  scale_fill_viridis_c(name = "Temp anomaly", option = "C") +
  geom_boxplot(aes(fill = mean.anol)) +
  theme_classic(base_size = 14) +
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
  xlab("Year") +
  ylab("Summer temperature anomaly (°C)") +
  ggtitle("Australia") +
  geom_hline(yintercept = 0, color = "red", linetype = "dotted", size = 1)
print(AUSsumm)
ggsave(AUSsumm, filename = "figures/AUSsummertemp_box.png", dpi = 300, height = 5, width = 6)

USsumm <- tempsummer2US %>% 
  group_by(year) %>% 
  mutate(mean.anol= mean(anol)) %>% 
  ggplot( aes(x = year, y = anol, group = year)) +
  geom_boxplot(aes(fill = mean.anol)) +
  theme_classic(base_size = 14) +
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
  xlab("Year") +
  ylab("Summer temperature anomaly (°C)") +
  scale_fill_viridis_c(name = "Temp anomaly", option = "C") +
  ggtitle("United States") +
  geom_hline(yintercept = 0, color = "red", linetype = "dotted", size = 1)
print(USsumm)
ggsave(USsumm, filename = "figures/USsummertemp_box.png", dpi = 300, height = 5, width = 6)


## make multipanel
tempmulti <- ggarrange(labels = c("(a)", "(b)"),
                           align = "hv",
                           AUSsumm, 
                           USsumm, 
                           nrow = 1, 
                           common.legend = TRUE, 
                           legend = "right") 
print(tempmulti)

ggsave(tempmulti, filename = "figures/combined_temp.png", height = 5, width = 12, dpi = 300)



#### plot sd across time ----
AUSsd <- tempsummer %>% 
  group_by(year) %>% 
  mutate(mean.sd = mean(sdTemp)) %>% 
  ggplot( aes(x = year, y = sdTemp, group = year)) +
  scale_fill_viridis_c(name = "Temp stdev", option = "C") +
  geom_boxplot(aes(fill = mean.sd)) +
  theme_classic(base_size = 14) +
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
  xlab("Year") +
  ylab("Summer temperature standard deviation") +
  ggtitle("Australia temperature standard deviation") 
print(AUSsd)
ggsave(AUSsd, filename = "figures/AUSsdtemp_box.png", dpi = 300, height = 5, width = 6)

USsd <- tempsummer2US %>% 
  group_by(year) %>% 
  mutate(mean.sd= mean(sdTemp)) %>% 
  ggplot( aes(x = year, y = sdTemp, group = year)) +
  scale_fill_viridis_c(name = "Temp stdev", option = "C") +
  geom_boxplot(aes(fill = mean.sd)) +
  theme_classic(base_size = 14) +
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
  xlab("Year") +
  ylab("Summer temperature standard deviation") +
  ggtitle("United States")
print(USsd)
ggsave(USsd, filename = "figures/USsdtemp_box.png", dpi = 300, height = 5, width = 6)


## make multipanel
tempsdmulti <- ggarrange(labels = c("(a)", "(b)", "(c)", "(d)"),
                       align = "hv",
                       AUSsumm, 
                       USsumm,
                       AUSsd,
                       USsd,
                       nrow = 2, 
                       ncol = 2,
                       common.legend = TRUE, 
                       legend = "right") 
print(tempsdmulti)

ggsave(tempsdmulti, filename = "figures/combined_temp&sd.png", height = 12, width = 12, dpi = 300)


















## old, probably delete later
# # plot avg November temp anomaly boxplots with fill
# AUSnov <- tempnov3 %>% 
#   group_by(year) %>% 
#   mutate(mean.anol= mean(anol)) %>% 
#   ggplot( aes(x = year, y = anol, group = year)) +
#   # stat_summary(fun.y= "mean",
#   #              aes(y=mean.anol,color=mean.anol)) +
#   scale_fill_viridis_c(name = "Temp anomaly", option = "C") +
#   geom_boxplot(aes(fill = mean.anol)) +
#   theme_classic(base_size = 14) +
#   theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
#   xlab("Year") +
#   ylab("November temperature anomaly (°C)") +
#   scale_fill_viridis_c(name = "Temp anomaly", option = "C") +
#   ggtitle("AUS November temperature anomaly") +
#   geom_hline(yintercept = 0, color = "red", linetype = "dotted", size = 1)
# print(AUSnov)
# ggsave(AUSnov, filename = "figures/AUSnovtemp_box.png", dpi = 300, height = 5, width = 6)
# 
# # look at temp by year with grouped sites
# AUStempridges <- ggplot(tempsummer2, aes(x = anol, y = as.factor(year), fill = stat(x))) +
#   geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, gradient_lwd = 1.) +
#   scale_fill_viridis_c(name = "Temp anomaly", option = "C") +
#   scale_color_viridis_c(name = "Temp anomaly", option = "C") +
#   ylab("Year") +
#   xlab("Temperature anomaly for Dec-Feb") +
#   labs(
#     title = 'Australia summer temperature averages',
#     subtitle = 'Mean temperature anomaly (°C) from 1979-2021'
#   ) +
#   theme_ridges(font_size = 14, grid = FALSE) + 
#   theme(axis.title.y = element_blank()) +
#   geom_vline(xintercept = 0, linetype = "dotted", size = 1)
# ggsave(AUStempridges, filename = "figures/AUS_summertemp_ridges.png", dpi = 300, height = 10, width = 5)
# 
# ## only November temp anomalies
# AUStempNov <- ggplot(tempnov2, aes(x = anol, y = as.factor(year), fill = stat(x))) +
#   geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, gradient_lwd = 1.) +
#   scale_fill_viridis_c(name = "Temp anomaly", option = "C") +
#   scale_color_viridis_c(name = "Temp anomaly", option = "C") +
#   ylab("Year") +
#   xlab("Temperature anomaly") +
#   labs(
#     title = 'AUS November temperatures',
#     subtitle = 'Mean temperature anomaly (°C) from 1979-2021'
#   ) +
#   theme_ridges(font_size = 14, grid = FALSE) + 
#   theme(axis.title.y = element_blank()) +
#   geom_vline(xintercept = 0, linetype = "dotted", size = 1)
# ggsave(AUStempNov, filename = "figures/AUS_Novtemp_ridges.png", dpi = 300, height = 10, width = 5)
