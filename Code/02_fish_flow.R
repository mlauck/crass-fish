### Jane S. Rogosch
### Created 28 Jul 2023
### This code is to examine relationships between fish and flow

### Load libraries--------------------------------------------------------------
library(dplyr)

### Load data ------------------------------------------------------------------
fish <- read.csv("Data/fish_flow/fish_occurrences_hexIDs_Sep2023.csv", row.names = 1)
head(fish)
# Freya has code for fish richness and presence absence already. See "PresAbsCodeforCorey.R" for sites with 10+ years of data
# Can also make similar for 30yrs. but today (9/22), not yet updated with new Hex_IDs. Relatedly may need to update
# Code "Site30yrsplus.R" and "Site10yrsplus.R"
flow <- read.csv("Data/fish_flow/gauges_with_hexIDs_Sep2023.csv")
head(flow)
names.10yr # the Hex IDs for hexes with 10 or more years of fish data from "PresAbsCodeforCorey.R"
names.20yr # the Hex IDs for hexes with 20 or more years of fish data from "PresAbsCodeforCorey.R"
presmat # "community" data for Hex's with at least 10 years of data from "PresAbsCodeforCorey.R"
rich # richness data for Hex's with at least 10 years of data "PresAbsCodeforCorey.R"
