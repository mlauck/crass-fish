### Jane S. Rogosch
### Created 28 Jul 2023
### This code is to examine relationships between fish and flow

### Load libraries--------------------------------------------------------------


### Load data ------------------------------------------------------------------
fish <- read.csv("Data/fish_flow/fish_by_hexbin_all.csv", row.names = 1)
head(fish)
flow <- read.csv("Data/fish_flow/Gauges_hexIDs_list.csv")
head(flow)
