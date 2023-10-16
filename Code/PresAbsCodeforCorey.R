# Sites with 10+ years
# Script by FER
# Last update Oct 2023

# libraries
library(tidyr)
library(stringr)
library(ggplot2)
library(ggpubr)
library(viridis)
library(lme4)
library(lattice)
# library(tidyverse)
library(dplyr)

# Jane has version machine issues, so this next line is for her
#.libPaths(new = "E:\\Users\\Administrator\\Documents\\OneDrive - Texas Tech University\\Documents\\R\\win-library\\4.0")

# load data
allfish <- read.csv("Data/fish_flow/fish_occurrences_hexIDs_Sep2023.csv", row.names = 1, header = TRUE, fileEncoding="UTF-8-BOM")
head(allfish)

# site as factor
allfish$hex_id <- as.factor(allfish$hex_id)
allfish$species <- as.factor(allfish$species)

# group by hexID
by_hex <- allfish %>% group_by(hex_id)

# Get names of hexIDs with long-term data
# Corey's method
hex.list <- split(allfish, f = allfish$hex_id)
hex.years <-
  allfish[!duplicated(allfish[, c("year", "hex_id")]), ] #3956 hexes
years.per.hex <-
  table(hex.years$hex_id) #median(years.per.hex) mean of 2.83 years per hex, 1 is median
hex.5yr <- years.per.hex[years.per.hex > 4] #5 or more sample years 176
hex.10yr <-
  years.per.hex[years.per.hex > 9] #10 or more sample years 79
names.10yr <- names(hex.10yr)
hex.20yr <-
  years.per.hex[years.per.hex > 19] #20 or more sample years 25
names.20yr <- names(hex.20yr)

# create list to filter larger dataset
filterID <- list(names.10yr)

# filter only sites with 10+ years of data
library(data.table)
setDT(allfish)
tenfish_filt <- allfish[filterID, on = "hex_id"]

pres <- tenfish_filt %>%
  group_by(hex_id, species, year) %>%
  summarize(count = n())

presnotNULL <- pres[pres$species !="",]

# species matrix of presence absence
presmat <- pivot_wider(presnotNULL,
                       names_from = species,
                       values_from = count,
                       names_repair = "unique",
                       values_fill = 0)


# replace anything > 1 with 1
# note: if you want to use year, change it to a factor or do it a less clunky way
presmat2 <- presmat %>%
  mutate_if(is.numeric, ~1 * (. > 0))



# richness by year
# species richness matrix
rich <- tenfish_filt %>% 
  group_by(hex_id, year) %>% 
  # distinct() %>%
  summarize(richness = length(unique(species)))
