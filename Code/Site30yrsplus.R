# 30 year data
# script by FER
# December 2022

# load data
longfish <- read.csv("Data/fish_data_30yr_site_subset.csv", header = TRUE)
head(longfish)

# site as factor
longfish$hexID <- as.factor(longfish$hexID)

# libraries
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(ggpubr)

summary(longfish)
summary <- longfish %>%
  group_by(year, hexID) %>%
  summarize(speciescount = count(species, na.rm = TRUE))
