### Jane S. Rogosch
### Created 28 Jul 2023
### This code is to examine relationships between fish and flow

### Load libraries--------------------------------------------------------------
library(dplyr)
#update.packages(checkBuilt=TRUE, ask=FALSE)
### Load data ------------------------------------------------------------------
fish <- read.csv("Data/fish_flow/Fish_occurrence_hexID_Jan2024.csv")
head(fish)
# Freya has code for fish richness and presence absence already. See "PresAbsCodeforCorey.R" for sites with 10+ years of data
# Can also make similar for 30yrs. but today (9/22), not yet updated with new Hex_IDs. Relatedly may need to update
# Code "Site30yrsplus.R" and "Site10yrsplus.R"
gage_hex <-  read.csv("Data/fish_flow/Gauges_hexIDs_Jan2024.csv", 
                      colClasses = c(rep(NA, 2), "character", rep(NA, 2)))

head(gage_hex)
### Load source code --------------------------------------------------------------
source("Code/PresAbsCode.R")
names.10yr #79 the Hex IDs for hexes with 10 or more years of fish data from "PresAbsCodeforCorey.R"
names.20yr #25 the Hex IDs for hexes with 20 or more years of fish data from "PresAbsCodeforCorey.R"
names.30yr #5 
presmat # "community" data for Hex's with at least 10 years of data from "PresAbsCodeforCorey.R"
rich # richness data for Hex's with at least 10 years of data "PresAbsCodeforCorey.R
hist(as.data.frame(presmat$hex_id))
length(presmat30$hex_id)
richthirty
### Organize flow and fish data by hexID ----------------------------------------------------------------
head(presmat)
presmat_wgage <- presmat[presmat$hex_id %in% gage_hex$hex_id, ]
rich_wgage <- rich[rich$hex_id %in% gage_hex$hex_id, ]
str(presmat_wgage)

# join flow metrics to hex_id by site_no
# from 01c_get_flows_for_fishy_subset
# oy the old one is in the environment, not the new one. 
# Rerun 01c_get_flows_for_fishy_subset.R starting at line 107 to line 220

daily_data_metrics_all <- read.csv('Output/ALL_daily_data_metrics_USA_Jan24.csv')
head(daily_data_metrics_all)

noflow_periods_metrics_all <- read.csv( file = 'Output/ALL_noflow_periods_metrics_USA_Jan24.csv')
head(noflow_periods_metrics)

signal_daily <- read.csv("Output/NAA/USA_signal_daily_Jan24.csv", row.names = 1, colClasses = c(rep(NA,12), "character") )
NAA_all <- signal_daily %>% 
  group_by(site_no, year) %>% 
  summarise(NAA= sum(resid.sig, na.rm = TRUE))

NAA_1980 <- signal_daily[signal_daily$year >= 1980, ] %>% 
  group_by(site_no, hex_id, year) %>% 
  summarise(NAA= sum(resid.sig, na.rm = TRUE)) %>%
  group_by(hex_id, year) %>%
  summarise(med_NAA = median(NAA))

NAA_1980$hex_id <- as.character(NAA_1980$hex_id)
# Join flow metrics to hex location by gage site number 
allflowmet <- left_join(daily_data_metrics_all, noflow_periods_metrics_all, by = c("hex_id", "wyear")) #maybe left join- don't want only no flow years that would bias data
allflowmet$hex_id <- as.character(allflowmet$hex_id)
allflowmet_NAA <- inner_join(allflowmet, NAA_1980, by = c("hex_id", "wyear" = "year"))

# flowmet_hex <- inner_join(daily_data_metrics, gage_hex, by = "hex_id") #noflow_periods_metrics 
#   #do I need gage_hex even to do what I want? I don't think so! might be left over from first time
#   # when I didn't put things by hex_id originally. try recoding from here to set up analyses and RDA
# noflowmet_hex <- inner_join(noflow_periods_metrics, gage_hex, by = "hex_id")
# NAA_hex <- inner_join(NAA_all, gage_hex, by = "site_no")
# head(NAA_hex)
# NAA_hex %>% group_by(year, hex_id,  site_no)


rich_NAA <- inner_join(rich, allflowmet_NAA, by = c("hex_id", "year" = "wyear"))
richthirty_NAA <- inner_join(richthirty, NAA_1980, by = c("hex_id", "year"))



# model?
library(lme4)
library(lattice)
library(sjPlot)
library(mgcv)
  # oo <- options(repos = "https://cran.r-project.org/")
  # install.packages("Matrix")
  # install.packages("lme4")
  # options(oo)

# Example lattice
# trellis plot
# a nice way to look at likely estimates but in frequentist frameworks

?scale
length(unique(rich_NAA$hex_id))
xyplot(richness~med_NAA|hex_id, data = rich_NAA, xlim = c(-150,150))

rich_NAA$NAAscale <- scale(rich_NAA$med_NAA)[,1]
rich_NAA$yearscale <- scale(rich_NAA$year)[,1]
rich_NAA$noflowdaysscale <- scale(rich_NAA$noflowdays)[,1]
rich_NAA$maxlengthscale <- scale(rich_NAA$max_length_noflow)[,1]
rich_NAA$zerofirstscale <- scale(rich_NAA$zeroflowfirst_wy)[,1]

xyplot(richness~NAAscale|hex_id, data = rich_NAA)
xyplot(richness~year|hex_id, data = rich_NAA)
plot(richness ~ med_NAA, data = rich_NAA)

xyplot(richness~NAAscale|hex_id, data = rich_NAA, panel = function(x,y,...){
  panel.xyplot(x, y, ...)
  panel.lmline(x, y, ...)}, ylab = "richness", xlab = "NAA")

xyplot(richness~year|hex_id, data = rich_NAA, panel = function(x,y,...){
  panel.xyplot(x, y, ...)
  panel.lmline(x, y, ...)}, ylab = "richness", xlab = "year")


fixed.rich <- lm(richness ~ med_NAA + year + hex_id, data = rich_NAA)
summary(fixed.rich)
fix.ignore <- glm(richness ~ year, data = rich_NAA, family = poisson)
summary(fix.ignore)
plot(richness ~ year, data = rich_NAA)
plot_model(fix.ignore)

basicrich <- glmer(richness ~ 1 + (1 | hex_id) + (1| year), data = rich_NAA, family = poisson)
summary(basicrich)
plot_model(basicrich, show.values = TRUE, type = "std")
plot_model(basicrich, show.values = FALSE, type = "re")
#  AIC      BIC   logLik deviance df.resid 
# 1427.9   1439.2   -711.0   1421.9      320  

basicyear<- glmer(richness ~ year + (1 | hex_id), data = rich_NAA, family = poisson)
summary(basicyear)
?plot_model
plot_model(basicyear, show.values = TRUE, type = "eff", pred.type = "re")
plot_model(basicyear, show.values = FALSE, type = "re")

rich.mod1 <- glmer(richness ~ NAAscale +  (1 | hex_id) + (1| year), data = rich_NAA, family = poisson)#random slope for year by hex_id
summary(rich.mod1)
plot_model(rich.mod1, type = "pred")
plot_model(rich.mod1, show.values = TRUE, type = "std")
plot_model(rich.mod1, show.values = FALSE, type = "re")
#  AIC      BIC   logLik deviance df.resid 
# 1429.9   1445.0   -710.9   1421.9      319 

rich.mod2 <- glmer(richness ~ noflowdaysscale +  (1 | hex_id) + (1| year), data = rich_NAA, family = poisson)#random slope for year by hex_id
summary(rich.mod2)
plot_model(rich.mod2, show.values = TRUE, type = "std")
plot_model(rich.mod2, show.values = FALSE, type = "re")
#     AIC      BIC   logLik deviance df.resid 
# 1429.9   1445.0   -710.9   1421.9      319 

rich.mod_1.1 <- glmer(richness ~ NAAscale + noflowdaysscale + maxlengthscale + zerofirstscale + (1 | hex_id) + (1| year), data = rich_NAA, family = poisson)#random slope for year by hex_id
summary(rich.mod_1.1)
plot_model(rich.mod_1.1, show.values = TRUE, type = "std")
plot_model(rich.mod_1.1, show.values = FALSE, type = "re")
#  AIC      BIC   logLik deviance df.resid 
# 255.1    267.3   -121.6    243.1       50 


#####################only hex random #################
basicrich <- glmer(richness ~ 1 + (1 | hex_id) , data = rich_NAA, family = poisson)
summary(basicrich)
plot_model(basicrich, show.values = TRUE, type = "std")
plot_model(basicrich, show.values = FALSE, type = "re")
#  AIC      BIC   logLik deviance df.resid 
#  1427.2   1434.8   -711.6   1423.2      321 

# This is the best model plot this
df_rich_NAA <- as.data.frame(rich_NAA)
basicyear<- glmer(richness ~ year + (1 | hex_id), data = df_rich_NAA, family = poisson)
summary(basicyear)
str(basicyear)
maincoef <- exp(fixef(basicyear))
confint(basicyear)
preds <- exp(predict(basicyear)) # Need to back transform from logit link. #Alternatively exp(preds)
coefs <- exp(coef(basicyear)$hex_id)
df_rich_NAA$preds <- preds


# Next time figure out how to make nice plot.
?sjPlot
plot_model(basicyear, show.values = TRUE, type = "eff", pred.type = "re", grid = FALSE,
           group.terms = hex_id)
plot_model(basicyear, show.values = FALSE, type = "re")
# AIC      BIC   logLik deviance df.resid 
# 1413.8   1425.2   -703.9   1407.8      320
get_model_data(basicyear, type = "eff")
get_model_data(basicyear, type = "slope")

?geom_abline
richpred <- ggplot(aes(x = year, y = preds, color = hex_id, group = hex_id), data = df_rich_NAA) +
  geom_point(data = df_rich_NAA, aes(year, richness, fill = hex_id), pch = 21, size = 1) +
  geom_abline(aes(slope = maincoef[2], intercept = maincoef[1]), col = "black") +
  theme_classic(base_size = 14) +
  xlab("Year") +
  ylab("Fish species richness") +
  scale_fill_viridis_d() 
print(richpred)

richpred <- ggplot(aes(x = year, y = preds), data = df_rich_NAA) +
  geom_point(data = df_rich_NAA, aes(year, richness, fill = hex_id), pch = 21, size = 1) +
  geom_abline(aes(slope = coefs[1], intercept = coefs[2])) +
  theme_classic(base_size = 14) +
  xlab("Year") +
  ylab("Fish species richness") +
  scale_fill_viridis_d() 
print(richpred)


?geom_abline
?get_model_data
preds_sj <- get_model_data(basicyear, type = "eff", pred.type = "re")
get_model_data(basicyear, type = "pred", terms = "year")

# get_model_data(basicyear, type = "est", pred.type = "re")
# richpred2 <- ggplot(aes(x = year, y = pred, color = hex_id, group = hex_id), data = df_rich_NAA) +
#   geom_point(data = df_rich_NAA, aes(year, richness, fill = hex_id), pch = 21, size = 1) +
#   scale_fill_viridis_d() +
#   theme_classic(base_size = 14) +
#   xlab("Year") +
#   ylab("Fish species richness") +
#   geom_abline(slope = -0.9870545, intercept = 27) +
#   geom_ribbon(aes(ymin = -0.9870545 -1.96*0.001287831, ymax =  -0.9870545 +1.96*0.001287831))
# print(richpred2)


str(preds_sj)
df_preds_sj <- as.data.frame(preds_sj)
# richpred3 <- ggplot(aes(x = x, y = predicted), data = df_preds_sj) +
#   geom_point(data = df_rich_NAA, aes(year, richness, fill = hex_id), pch = 21, size = 1) +
#   scale_fill_viridis_d() +
#   theme_classic(base_size = 14) +
#   xlab("Year") +
#   ylab("Fish species richness") +
#   geom_smooth(aes(x = x, y = predicted), data = preds_sj) +
# geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.3)
# print(richpred3) # Marginal effects plot

library(modelr)
library(ggplot2)
library(dplyr)
?data_grid
predicted_values <- modelr::data_grid(df_rich_NAA, year = seq_range(year,39), hex_id) %>%
  modelr::add_predictions(basicyear)
predicted_values$preds <- exp(predicted_values$pred)

main_values <- modelr::data_grid(df_rich_NAA, year = seq_range(year, 39)) %>% modelr::add_predictions(basicyear)

#Final plot
fish_richness_plot <- ggplot(aes(x = x, y = predicted), data = df_preds_sj) + # color = hex_id # group = hex_id
  geom_point(aes(year, richness, color = hex_id), data = df_rich_NAA) + 
  geom_line(aes(year, preds, color = hex_id), data = predicted_values) +
  scale_fill_viridis_d(name = "Hex ID",  aesthetics = "colour") +
  theme_classic(base_size = 14) +
  xlab("Year") +
  ylab("Fish species richness") +
  xlim(1980, 2021)+
  ylim(0, 15)+
  geom_line(aes(x, predicted), data = df_preds_sj, linewidth = 1.5, linetype = 2, color = "grey40")+ #"#3366ff" # color = "#1F9E89FF"
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.3, fill = "grey40") # fill = "grey60"
print(fish_richness_plot)
#ggsave(fish_richness_plot, filename = "figures/fish_richness_time_Apr24.pdf", dpi = 300, height = 5, width = 7)

# 
# 
# predicted_values %>%
#   ggplot(aes(year, preds,  color = hex_id)) + # color = hex_id # group = hex_id
#   geom_point(data = df_rich_NAA, aes(year, richness)) + # color = hex_id, fill = hex_id
#   scale_fill_viridis_d(name = "Hex ID",  aesthetics = "colour") +
#   theme_classic(base_size = 14) +
#   xlab("Year") +
#   ylab("Fish species richness") +
#   xlim(1980, 2021)+
#   ylim(1, 15)+
#   geom_smooth(method = "lm") 
#   
# 
#  geom_abline(intercept = maincoef[1], slope = -maincoef[2])  #+ #Why no work?!
# 
#   #geom_ribbon(aes(x = df_preds_sj$x, ymin = df_preds_sj$conf.low, ymax = df_preds_sj$conf.high), alpha = 0.3)
#     
 
# ggplot(df_rich_NAA,aes(year, richness, color = hex_id)) + # color = hex_id # group = hex_id
#     geom_point() + 
#     scale_fill_viridis_d(name = "Hex ID",  aesthetics = "colour") +
#     theme_classic(base_size = 14) +
#     xlab("Year") +
#     ylab("Fish species richness") +
#     xlim(1980, 2021)+
#     ylim(0, 15)+
#   geom_line(aes(year, preds), data = predicted_values)+
#   geom_abline(aes(slope = (-0.01303), intercept = (27.77342)))



# Example
sleepstudy
# model<-lmer(Reaction ~ Days + (Days + 1|Subject), data = sleepstudy)
# 
# predicted_values<- modelr::data_grid(sleepstudy, Days, Subject) %>% 
#   modelr::add_predictions(model)
# 
# 
# predicted_values %>% 
#   ggplot(aes(Days, pred, color = Subject))+
#   geom_line()+
#   geom_point(data = sleepstudy, aes(Days, Reaction, color = Subject))
ss <- predict(model)
# predicted_values <- predict(basicyear)






rich.mod1 <- glmer(richness ~ med_NAA +  (1 | hex_id) , data = rich_NAA, family = poisson)#random slope for year by hex_id
summary(rich.mod1)
plot_model(rich.mod1, type = "pred")
plot_model(rich.mod1, show.values = TRUE, type = "std")
plot_model(rich.mod1, show.values = FALSE, type = "re")
#  AIC      BIC   logLik deviance df.resid 
# 1429.2   1440.6   -711.6   1423.2      320 


rich.mod2 <- glmer(richness ~ noflowdays +  (1 | hex_id) , data = rich_NAA, family = poisson)#random slope for year by hex_id
summary(rich.mod2)
plot_model(rich.mod2, show.values = TRUE, type = "std")
plot_model(rich.mod2, show.values = FALSE, type = "re")
#     AIC      BIC   logLik deviance df.resid 
#  1429.2   1440.6   -711.6   1423.2      320 

rich.mod3 <- glmer(richness ~ max_length_noflow +  (1 | hex_id) , data = rich_NAA, family = poisson)#random slope for year by hex_id
summary(rich.mod3)
plot_model(rich.mod3, show.values = TRUE, type = "std")
plot_model(rich.mod3, show.values = FALSE, type = "re")
#     AIC      BIC   logLik deviance df.resid 
#  1429.0   1440.3   -711.5   1423.0      320 

rich.mod4 <- glmer(richness ~ zeroflowfirst_wy  +  (1 | hex_id) , data = rich_NAA, family = poisson)#random slope for year by hex_id
summary(rich.mod4)
plot_model(rich.mod4, show.values = TRUE, type = "std")
plot_model(rich.mod4, show.values = FALSE, type = "re")
#     AIC      BIC   logLik deviance df.resid 
#   1429.2   1440.5   -711.6   1423.2      320 

rich.mod_1.1 <- glmer(richness ~ NAAscale + noflowdaysscale + maxlengthscale + zerofirstscale + yearscale+ (1 | hex_id) , data = rich_NAA, family = poisson)#random slope for year by hex_id
summary(rich.mod_1.1)
plot_model(rich.mod_1.1, show.values = TRUE, type = "std")
plot_model(rich.mod_1.1, show.values = FALSE, type = "re")
#  AIC      BIC   logLik deviance df.resid 
#  1420.0   1446.4   -703.0   1406.0      316 


##################################################
rich.mod30 <- glmer(richness ~ NAA +  (1 | hex_id) + (1| year), data = richthirty_NAA, family = poisson)#random slope for year by hex_id
summary(rich.mod30)
plot_model(rich.mod30, show.values = TRUE, type = "std")
plot_model(rich.mod30, show.values = FALSE, type = "re")

# rich.mod2 <- glmer(richness ~ NAAscale +  (NAAscale| hex_id) + (1| year), data = rich_NAA, family = poisson)#random slope for year by hex_id
# summary(rich.mod2)
# plot_model(rich.mod2, show.values = TRUE, type = "std")
# plot_model(rich.mod2, show.values = FALSE, type = "re")
# 
# rich.mod <- glmer(richness ~ NAAscale + yearscale + (yearscale | hex_id), data = rich_NAA, family = poisson)#random slope for year by hex_id
# summary(rich.mod)
# 
# plot_model(rich.mod, show.values = TRUE, type = "std")
# plot_model(rich.mod, show.values = FALSE, type = "re")


#### Now with metrics ####
# need to join metrics to hex_id.
# flowmet_hex$hex_id <- as.character(flowmet_hex$hex_id)
# noflowmet_hex$hex_id <- as.character(noflowmet_hex$hex_id)


#### COmmunity stuff ###-------------------
library(vegan)

presmat_flowmet <- inner_join(presmat, allflowmet_NAA, by = c("hex_id", "year" = "wyear"))


presMANOVA1 <- adonis2(presmat_flowmet[3:85] ~ med_NAA +year + noflowdays + zeroflowfirst_wy + max_length_noflow , 
                       data =presmat_flowmet)
presMANOVA1

apply(presmat_flowmet[,3:85], 1, sum) # all zeros - weird?
presmat_flowmet[-304,3:85]

presmod <- metaMDS(presmat_flowmet[-304,3:85])
?plot
plot(presmod, cex=0)
text(presmod, "site")
presmod

decorana(presmat_flowmet[-304,3:85])
# NExt time CCA with species and environment.
fish_flow.cca <- cca(presmat_flowmet[ -304,3:85]~med_NAA + year + noflowdays + zeroflowfirst_wy , data = presmat_flowmet[-304,], 
                     na.action = na.exclude)
fish_flow.cca
plot(fish_flow.cca, choices = c(1,2), display = c("wa", "sp"))
text(fish_flow.cca,choices=c(1,2),display='bp',col='blue', labels = c("NAA", "year", "no flow days", "zero flow first"))


plot(fish_flow.cca, choices=c(1,2),display=c('wa','sp','bp'),scaling=2, type = "text")
anova(fish_flow.cca)
anova(fish_flow.cca, by = 'axis')
anova(fish_flow.cca, by = 'terms')
summary(fish_flow.cca)


###### Richness time data Australia ----------------------------------------
rich
fish
str(fish)
fishAUS <- fish[fish$decimalLatitude <0, ]
fishUSA <- fish[fish$decimalLatitude >0, ]

richAUS <- rich[rich$hex_id %in% fishAUS$hex_id,] # Great white shark. Haha. Guess I should have filtered rows
# where the habitat column is NA
richUSA <- rich[rich$hex_id %in% fishUSA$hex_id,]

unique(richAUS$hex_id)
unique(richUSA$hex_id) #78


basicyear_USA <- glmer(richness ~ year + (1 | hex_id), data = richUSA, family = poisson)
summary(basicyear_USA)
str(basicyear_USA)


preds_sj_USA <- get_model_data(basicyear_USA, type = "eff", pred.type = "re")
get_model_data(basicyear_USA, type = "pred", terms = "year")
preds_sj_USA <- as.data.frame(preds_sj_USA)

predicted_values_USA <- modelr::data_grid(richUSA, year = seq_range(year,39), richUSA$hex_id) %>%
  modelr::add_predictions(basicyear_USA)
predicted_values_USA$preds <- exp(predicted_values_USA$pred)

fish_richness_plot_USA <- ggplot(aes(x = x, y = predicted), data = preds_sj_USA) + # color = hex_id # group = hex_id
  geom_point(aes(year, richness, color = hex_id), data = richUSA) + 
  geom_line(aes(year, preds, color = hex_id), data = predicted_values_USA) +
  scale_fill_viridis_d(name = "Hex ID",  aesthetics = "colour", guide = "none") +
  theme_classic(base_size = 14) +
  xlab("Year") +
  ylab("Fish species richness") +
  xlim(1980, 2021)+
  ylim(0, 15)+
  geom_line(aes(x, predicted), data = preds_sj_USA, linewidth = 1.5, linetype = 2, color = "grey40")+ #"#3366ff" # color = "#1F9E89FF"
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.3, fill = "grey40") # fill = "grey60"
print(fish_richness_plot_USA)
#ggsave(fish_richness_plot, filename = "figures/fish_richness_time_Apr24.jpg", dpi = 300, height = 5, width = 7)