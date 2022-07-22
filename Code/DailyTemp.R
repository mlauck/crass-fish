# climate data

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

# ## save this as rda
# saveRDS(tempUSE, "Data/tempUSE.rda")
# 
# # load data
# load(file = 'Data/tempUSE.rda')

# look at temp by site
ggplot(tempUSE, aes(y = Site, x = TempC, color = as.factor(month), fill = as.factor(month))) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, gradient_lwd = 1.) +
  scale_fill_viridis_d(name = "Month", option = "C") +
  scale_color_viridis_d(name = "Month", option = "C") +
  ylab("Site No.") +
  xlab("Temp in C") +
  labs(
    title = 'Temperatures across years',
    subtitle = 'Mean temperatures (C) by month from 1980-2022'
  ) +
  theme_ridges(font_size = 13, grid = TRUE) + 
  theme(axis.title.y = element_blank())
