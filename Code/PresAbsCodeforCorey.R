# Sites with 10+ years
# Script by FER
# Last update May 2023

# libraries
library(tidyr)
library(stringr)
library(ggplot2)
library(ggpubr)
library(viridis)
library(lme4)
library(lattice)
library(tidyverse)


# load data
allfish <- read.csv("Data/fish_by_hexbin_all.csv", header = TRUE, fileEncoding="UTF-8-BOM")
head(allfish)

# site as factor
allfish$hexID <- as.factor(allfish$hexID)
allfish$species <- as.factor(allfish$species)

# group by hexID
by_hex <- allfish %>% group_by(hexID)

# Get names of hexIDs with long-term data
# Corey's method
hex.list <- split(allfish, f = allfish$hexID)
hex.years <-
  allfish[!duplicated(allfish[, c("year", "hexID")]), ] #1379 hexes
years.per.hex <-
  table(hex.years$hexID) #mean of 4.38 years per hex, 3 is median
hex.5yr <- years.per.hex[years.per.hex > 4] #5 or more sample years 399
hex.10yr <-
  years.per.hex[years.per.hex > 9] #10 or more sample years 147
names.10yr <- names(hex.10yr)
hex.20yr <-
  years.per.hex[years.per.hex > 19] #20 or more sample years 45
names.20yr <- names(hex.20yr)

# create list to filter larger dataset
filterID <- list(names.10yr)

# filter only sites with 10+ years of data
library(data.table)
setDT(allfish)
tenfish_filt <- allfish[filterID, on = "hexID"]

pres <- tenfish_filt %>%
  group_by(hexID, species, year) %>%
  summarize(count = n())

# species matrix of presence absence
presmat <- pivot_wider(pres,
                       names_from = species,
                       values_from = count,
                       values_fill = 0)


# replace anything > 1 with 1
# note: if you want to use year, change it to a factor or do it a less clunky way
presmat2 <- presmat %>%
  mutate_if(is.numeric, ~1 * (. > 0))



# richness by year
# species richness matrix
rich <- tenfish_filt %>% 
  group_by(hexID, year) %>% 
  # distinct() %>%
  summarize(richness = length(unique(species)))