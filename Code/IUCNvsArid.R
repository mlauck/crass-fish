# Comparing IUCN lists of all fishes vs. arid fishes
# FER
# Last edit 21 Jan 2022

## next week to do:
# 1) figure out how to merge - may need to append
# 2) figure with grouped boxplot
# 3) ordinal regression on status vs. arid or not

# libraries
library(dplyr)
library(ggplot2)
library(stringr)
library(glue)

# load data
allIUCN <- read.csv("Data/IUCNassessments.csv", header = TRUE)
allarid <- read.csv("Data/fish_traits.csv", header = TRUE)

# make columns categories
allIUCN$redListCategory <- as.factor(allIUCN$redlistCategory)
allIUCN$realm <- as.factor(allIUCN$realm)

allarid$IUCNstatus <- as.factor(allarid$IUCNstatus)

# make figure of the IUCN all fishes and status ----
# summarize data by status out of 11211 species in database
IUCN_df<- allIUCN %>%
  group_by(redListCategory) %>%
  summarise(n = n(),
            prop = n()/11211)

# eliminate categories not in other dataset
IUCN_df2 <- IUCN_df[-c(7:8),]

# rename column because R defeated me
# IUCN_df2$IUCNstatus <- IUCN_df2$redListCategory
IUCN_df2 <- IUCN_df2 %>%
  dplyr::rename(IUCNstatus = redListCategory)

# make a df
IUCN_df2 <- as.data.frame(IUCN_df2)

# arid fishes
# rename arid fish columns
allarid$IUCNstatus <- recode_factor(allarid$IUCNstatus, 
                                    CR = "Critically Endangered", 
                                    DD = "Data Deficient",
                                    EN = "Endangered",
                                    LC = "Least Concern",
                                    NE = "Not Evaluated",
                                    NT = "Near Threatened",
                                    VU = "Vulnerable",
                                    EX = "Extinct",
                                    EW = "Extinct in the Wild",
                                    CE = "Critically Endangered")

# total fishes = length(!is.na(allarid$IUCNstatus)) = 428 - 12 in weird row = 416
arid_df <- allarid %>%
  group_by(IUCNstatus) %>%
  summarize(n = n(),
            prop = n()/416)

arid_df2 <- arid_df[-10, ] # empty cells excluded in row 10

arid_df2 <- as.data.frame(arid_df2)

## merge df together
alldata <- rbind(IUCN_df2, arid_df2)

## add a column for source
alldata$source <- "arid"
alldata[1:min(8, nrow(alldata)),]$source <- "IUCN"
alldata

## reorder data for plotting and ordinal regression
alldata$IUCNstatus <- factor(
  alldata$IUCNstatus,
  levels = c(
    "Extinct",
    "Extinct in the Wild",
    "Critically Endangered",
    "Endangered",
    "Vulnerable",
    "Near Threatened",
    "Least Concern",
    "Data Deficient",
    "Not Evaluated"
  )
)

alldata <- alldata[order(alldata$IUCNstatus), ]

# grouped barplot
barplot <- ggplot(alldata, aes(x = IUCN, y = prop, fill = prop, group = source)) +
  #geom_bar(stat = "identity") +
  geom_col(
    aes(
      x = reorder(str_wrap(IUCNstatus, 5), n),
      y = prop,
      fill = n
    ),
    position = "dodge2",
    show.legend = TRUE,
    alpha = .9
  ) +
  scale_fill_viridis_c(option = "magma") +
  theme_bw(base_size = 14)
print(barplot)

