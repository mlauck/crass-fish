### Jane S. Rogosch
### Created 28 Jul 2023
### This code is to examine relationships between fish and flow

### Load libraries--------------------------------------------------------------
library(dplyr)
#update.packages(checkBuilt=TRUE, ask=FALSE)
### Load data ------------------------------------------------------------------
fish <- read.csv("Data/fish_flow/fish_occurrences_hexIDs_Sep2023.csv", row.names = 1)
head(fish)
# Freya has code for fish richness and presence absence already. See "PresAbsCodeforCorey.R" for sites with 10+ years of data
# Can also make similar for 30yrs. but today (9/22), not yet updated with new Hex_IDs. Relatedly may need to update
# Code "Site30yrsplus.R" and "Site10yrsplus.R"
gage_hex <-  read.csv("Data/fish_flow/gauges_with_hexIDs_Sep2023.csv", row.names = 1,
                      colClasses = c(rep(NA, 4), "character", rep(NA, 4)))

head(gage_hex)
### Load source code --------------------------------------------------------------
source("Code/PresAbsCodeforCorey.R")
names.10yr #79 the Hex IDs for hexes with 10 or more years of fish data from "PresAbsCodeforCorey.R"
names.20yr #25 the Hex IDs for hexes with 20 or more years of fish data from "PresAbsCodeforCorey.R"
presmat # "community" data for Hex's with at least 10 years of data from "PresAbsCodeforCorey.R"
rich # richness data for Hex's with at least 10 years of data "PresAbsCodeforCorey.R

### Organize flow and fish data by hexID ----------------------------------------------------------------
head(presmat)
presmat_wgage <- presmat[presmat$hex_id %in% gage_hex$hex_id, ]
rich_wgage <- rich[rich$hex_id %in% gage_hex$hex_id, ]
str(presmat_wgage)

# join flow metrics to hex_id by site_no
# from 01c_get_flows_for_fishy_subset
# oy the old one is in the environment, not the new one. 
# Rerun 01c_get_flows_for_fishy_subset.R starting at line 107 to line 220

daily_data_metrics <- read.csv('Output/daily_data_metrics_USA.csv')
head(daily_data_metrics)

signal_daily <- read.csv("Output/NAA/USA_signal_daily_Sep23.csv", row.names = 1, colClasses = c(rep(NA,12), "character") )
NAA_all <- signal_daily %>% 
  group_by(site_no, year) %>% 
  summarise(NAA= sum(resid.sig, na.rm = TRUE))

# Join flow metrics to hex location by gage site number
flowmet_hex <- inner_join(daily_data_metrics, gage_hex, by = "site_no") #noflow_periods_metrics
NAA_hex <- inner_join(NAA_all, gage_hex, by = "site_no")
head(NAA_hex)
NAA_hex$hex_id <- as.character(NAA_hex$hex_id)

rich_NAA <- inner_join(rich, NAA_hex, by = c("hex_id", "year"))
yr_count <- rich_NAA %>% group_by(site_no) %>% summarise(yrs_record = n_distinct(year))
yr_count[yr_count$site_no == "10020100", ] #above reservoir
yr_count[yr_count$site_no == "10020300", ] #below reservoir
  # where are fish?
  allfish[allfish$hex_id == "100762", ] # in reservoir? 
rich_NAA2 <- rich_NAA[rich_NAA$site_no != "10020300", ]
# similar for sulphur creek. Can't think of how not to do this by hand...
rich_NAA3 <- rich_NAA2[rich_NAA2$site_no != "10015900", ] # again the below reservoir gage

# 09442960 only from '99 to '08 - use this one
# 09442956 also '99 to '08
# 09442958 '03 to '08
# Next
# 09419658 '88 to '13 this is little trib wash
# 09419679 '90 to '23 so this is the main LV wash -use this one
# 09419696 '21 to '23 this is different creek. where are fish?
# 09419698 below confluence with different creek
#fish are in different places too. But all points I checked (6) are in the main wash.
allfish[allfish$hex_id == "64251", ]
# Next set
# 10171000 '86 to '23 #jordan river
# 10168000 '85 to '23 # cottonwood creek
# 10170500 canal. where are the fish
allfish[allfish$hex_id == "93006", ] # most are in the creek (all rivfishtime and a few GBIF checked)

rich_NAA4 <- rich_NAA3[rich_NAA3$site_no != "10015900" & 
                         rich_NAA3$site_no != "09442956" &
                         rich_NAA3$site_no != "09442958" &
                         rich_NAA3$site_no != "09419658" &
                         rich_NAA3$site_no != "09419696" &
                         rich_NAA3$site_no != "09419698" &
                         rich_NAA3$site_no != "10171000" &
                       rich_NAA3$site_no != "10170500", ]

# model?
library(lme4)
library(lattice)
library(mgcv)
oo <- options(repos = "https://cran.r-project.org/")
install.packages("Matrix")
install.packages("lme4")
options(oo)

length(unique(rich_NAA4$hex_id))
xyplot(richness~NAA|hex_id, data = rich_NAA4, xlim = c(-150,150))
rich_NAA4$NAAscale <- scale(rich_NAA4$NAA)[,1]
rich_NAA4$yearscale <- scale(rich_NAA4$year)[,1]
xyplot(richness~NAAscale|hex_id, data = rich_NAA4)
xyplot(richness~year|hex_id, data = rich_NAA4)
rich.moda <- glm(richness~NAAscale + year, data = rich_NAA4)
summary(rich.mod)
rich.modb <- glmer(richness ~ NAAscale + year + (1|hex_id), 
                  data = rich_NAA4, family = poisson)
summary(rich.modb)
rich.modc <- glmer(richness ~ NAAscale +  (year|hex_id), 
                  data = rich_NAA4, family = poisson)
summary(rich.modc)

summary(rich.mod)
# rich.mod <- nlme(richness ~ NAAscale + year + hex_id,
#                  data = rich_NAA4,
#                  fixed = NAAscale + year,
#                  random = hex_id)
summary(rich.mod)
#accounting for year as autocorrelation
rich.mod1 <- gls(richness ~ NAAscale,
                 correlation = corAR1(form = ~year|hex_id),
                 data = rich_NAA4)
summary(rich.mod1)
rich.mod2 <- gls(richness ~ NAAscale + year,
                 correlation = corAR1(form = ~year|hex_id),
                 data = rich_NAA4)
                 #family = poisson)
summary(rich.mod2)
rich.mod3 <- gamm(richness ~ NAA + 
                    s(year),
                  correlation = corAR1(form =~year|hex_id ), data = rich_NAA4, family = poisson)
summary(rich.mod3$lme)

rich.mod4 <- gamm(richness ~ NAAscale + 
                    s(year),
                  random = list(hex_id=~1),
                  correlation = corAR1(form =~year|hex_id ), data = rich_NAA4, family = poisson)
summary(rich.mod4$lme)
anova(rich.mod4$gam)
plot(rich.mod4$gam)
xyplot(richness~year,data = rich_NAA4)


#### Now with metrics ####
# need to join metrics to hex_id. Just did a copy paste job
flowmet_hex$hex_id <- as.character(flowmet_hex$hex_id)
rich_flowmet <- inner_join(rich, flowmet_hex, by = c("hex_id", "year" = "wyear"))
yr_count <- rich_flowmet %>% group_by(site_no) %>% summarise(yrs_record = n_distinct(year))
yr_count[yr_count$site_no == "10020100", ] #above reservoir
yr_count[yr_count$site_no == "10020300", ] #below reservoir
# where are fish?
allfish[allfish$hex_id == "100762", ] # in reservoir? 
rich_flowmet2 <- rich_flowmet[rich_flowmet$site_no != "10020300", ]
# similar for sulphur creek. Can't think of how not to do this by hand...
rich_flowmet3 <- rich_flowmet2[rich_flowmet2$site_no != "10015900", ] # again the below reservoir gage

# 09442960 only from '99 to '08 - use this one
# 09442956 also '99 to '08
# 09442958 '03 to '08
# Next
# 09419658 '88 to '13 this is little trib wash
# 09419679 '90 to '23 so this is the main LV wash -use this one
# 09419696 '21 to '23 this is different creek. where are fish?
# 09419698 below confluence with different creek
#fish are in different places too. But all points I checked (6) are in the main wash.
allfish[allfish$hex_id == "64251", ]
# Next set
# 10171000 '86 to '23 #jordan river
# 10168000 '85 to '23 # cottonwood creek
# 10170500 canal. where are the fish
allfish[allfish$hex_id == "93006", ] # most are in the creek (all rivfishtime and a few GBIF checked)

rich_flowmet4 <- rich_flowmet3[rich_flowmet3$site_no != "10015900" & 
                         rich_flowmet3$site_no != "09442956" &
                         rich_flowmet3$site_no != "09442958" &
                         rich_flowmet3$site_no != "09419658" &
                         rich_flowmet3$site_no != "09419696" &
                         rich_flowmet3$site_no != "09419698" &
                         rich_flowmet3$site_no != "10171000" &
                         rich_flowmet3$site_no != "10170500", ]



xyplot(richness~noflowdays|hex_id, data = rich_flowmet4, xlim = c(-1,30))
rich_flowmet4$noflowdaysscale <- scale(rich_flowmet4$noflowdays)[,1]
rich_flowmet4$yearscale <- scale(rich_flowmet4$year)[,1]
xyplot(richness~noflowdaysscale|hex_id, data = rich_flowmet4)
xyplot(richness~year|hex_id, data = rich_flowmet4)
rich.moda <- glm(richness~noflowdaysscale + year, data = rich_flowmet4)
summary(rich.moda)
rich.modb <- glmer(richness ~ noflowdaysscale + year + (1|hex_id), 
                   data = rich_flowmet4, family = poisson)
summary(rich.modb)
rich.modc <- glmer(richness ~ noflowdaysscale +  (year|hex_id), 
                   data = rich_flowmet4, family = poisson)
summary(rich.modc)

rich.moda <- glm(richness~zeroflowfirst_wy + year, data = rich_flowmet4)
summary(rich.moda)
rich.modb <- glmer(richness ~ zeroflowfirst_wy + year + (1|hex_id), 
                   data = rich_flowmet4, family = poisson)
summary(rich.modb)
rich.modc <- glmer(richness ~ zeroflowfirst_wy +  (year|hex_id), 
                   data = rich_flowmet4, family = poisson)
summary(rich.modc)


rich.moda <- glm(richness~zeroflowfirst_wy + year, data = rich_flowmet4)
summary(rich.moda)
rich.modb <- glmer(richness ~ zeroflowfirst_wy + year + (1|hex_id), 
                   data = rich_flowmet4, family = poisson)
summary(rich.modb)
rich.modc <- glmer(richness ~ zeroflowfirst_wy +  (year|hex_id), 
                   data = rich_flowmet4, family = poisson)
summary(rich.modc)

#Beware the sketchy coding. I did a copy paste to sub "noflow_periods_metrics" for 
# the daily ones and then ran this.
xyplot(richness~max_length_noflow|hex_id, data = rich_flowmet4)
rich.moda <- glm(richness~max_length_noflow+ year, data = rich_flowmet4)
summary(rich.moda)
rich.modb <- glmer(richness ~ max_length_noflow+ year + (1|hex_id), 
                   data = rich_flowmet4, family = poisson)
summary(rich.modb)
rich.modc <- glmer(richness ~ max_length_noflow +  (year|hex_id), 
                   data = rich_flowmet4, family = poisson)
summary(rich.modc)

#### NEXT STEP ####

# NExt step plot this. Make sure model structure is right for what I want to do, and then run the same with fish metrics
# and then do multivariate versions
# ALSO copy folders to backup and then pull, commmit, push.
??sjp.glmer
#https://www.r-bloggers.com/2014/11/visualizing-generalized-linear-mixed-effects-models-part-2-rstats-lme4/


# With metrics
