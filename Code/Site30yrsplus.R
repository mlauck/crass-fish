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
library(viridis)
library(ggridges)

summary(longfish)

# species per site
longfish %>%                                        # Specify data frame
  group_by(year, hexID) %>%                         # Specify group indicator
  summarise_at(vars(species),              # Specify column
               list(name = count))                # Specify function

ggplot(aes(x = species, y = year, fill = stat(x)), data = longfish) +
  geom_point(pch = 21) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, gradient_lwd = 1.)
  scale_fill_viridis_d() +
  theme_bw()
  
  TQPlot2 <- ggplot(data = longfish, aes(x = year, y = species, group = hexID)) +
    scale_fill_viridis_c(option = "magma", name = "year") +
    geom_col(aes(fill = year)) +
    theme_bw(base_size = 8) +
    # geom_hline(yintercept = 1,
    #            color = "black",
    #            linetype = "dashed") +
    xlab("Number of times detected") +
    ylab("Species") +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    ) +
    ggtitle("Species presence in data") +
    facet_wrap(~hexID)
  print(TQPlot2)

library(tidyverse)
rich <- longfish %>% 
    group_by(hexID, year) %>% 
    # summarise_all(sum) %>%
    # ungroup %>% 
    mutate(Richness = count(species)) %>%
    select(hexID, year, Richness)
