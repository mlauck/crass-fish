# Get new fishbase data

# # to download most recent version of fishbase
# remotes::install_github("ropensci/rfishbase")

# libraries
library(rfishbase)
library(dplyr)

# load traits data
allarid <- read.csv("Data/fish_traits.csv", header = TRUE)

# make a vector of fish names
fish <- allarid[,1]

## figure out column names within fishabse
fish.data <- getData(1:5)

# fb_tbl("species") %>% 
#   mutate(sci_name = paste(Genus, Species)) %>%
#   filter(sci_name %in% fish) %>% 
#   select(sci_name, FBname, Length)


## exploring where data are stored
ecofish <- ecology(fish)
ecosfish <- ecosystem(fish)
popfish <- popchar(fish) # Length/weight relationships
repfish <- reproduction(fish) # eh, nothing here we don't have already
estfish <- estimate(fish) #MaxLengthTL, Troph, seTroph, TempPrefMin, TempPrefMax, TempPrefMean, but the temp pref are rarer



## Get the data for trophic level
# FoodTroph = MonteCarlo estimate of trophic level based on known food items. 
# DietTroph = the mean or median of trophic levels derived from actual diet composition studies

food <- ecology(fish,
        fields=c("SpecCode", "FoodTroph", "FoodSeTroph", "DietTroph", "DietSeTroph"))

## get status (introduced, native, endemic, etc.)
ecosfish <- ecosystem(fish,
                      fields = c("Status"))
