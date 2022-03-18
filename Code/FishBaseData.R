# Get new fishbase data

library(rfishbase)
library(dplyr)

# load traits data
allarid <- read.csv("Data/fish_traits.csv", header = TRUE)

# make a vector of fish names
fish <- allarid[,1]

# fb_tbl("species") %>% 
#   mutate(sci_name = paste(Genus, Species)) %>%
#   filter(sci_name %in% fish) %>% 
#   select(sci_name, FBname, Length)

## Get the data for trophic level
# FoodTroph = MonteCarlo estimate of trophic level based on known food items. 
# DietTroph = the mean or median of trophic levels derived from actual diet composition studies

food <- ecology(fish,
        fields=c("SpecCode", "FoodTroph", "FoodSeTroph", "DietTroph", "DietSeTroph"))
