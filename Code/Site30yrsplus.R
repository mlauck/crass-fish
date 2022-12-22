# 30 year data
# script by FER
# December 2022

# load data
longfish <- read.csv("Data/fish_data_30yr_site_subset.csv", header = TRUE)
head(longfish)

# site as factor
longfish$hexID <- as.factor(longfish$hexID)
longfish$species <- as.factor(longfish$species)


# libraries
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(ggpubr)
library(viridis)
library(ggridges)
library(lme4)
library(brms)
library(magrittr)
library(rstan)
library(tidybayes)
library(emmeans)
library(broom)
library(modelr)
library(forcats)
library(RColorBrewer)
library(lattice)

summary(longfish)

barcodeplot <- ggplot(data = longfish, aes(x = year, y = species, group = hexID)) +
  scale_fill_viridis_c(option = "magma", name = "year") +
  geom_col(aes(fill = year)) +
  theme_bw(base_size = 8) +
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
print(barcodeplot)

# unique species by site and year
library(tidyverse)
rich <- longfish %>% 
    group_by(hexID, year) %>% 
    # distinct() %>%
    summarize(richness = length(unique(species)))

# plot up richness
richplot <- ggplot(aes(x = year, y = richness, fill = hexID), data = rich) +
  geom_point(pch = 21, size = 2) +
  scale_fill_viridis_d() +
  theme_bw(base_size = 14) +
  xlab("Year") +
  ylab("Fish species richness") +
  geom_smooth(method = "loess")
print(richplot)

richmod <- lmer(richness ~ year + (1|hexID), data = rich)
print(richmod)
summary(richmod)



# trellis plot
# a nice way to look at likely estimates but in frequentist frameworks

# trellis of fish richness vs. TP by month
# non-pooled
xyplot(richness ~ year|as.factor(hexID), data = rich, panel=function(x,y,...){
  panel.xyplot(x, y, ...)
  panel.lmline(x,y,...)}, ylab="Richness", xlab="Year")

# why won't this work?
mod_rich1 <- lmer(richness ~ year + (1|hexID), data = rich)
summary(mod_rich1)

mod_rich <- brm(
  richness ~ year + (1 + year|hexID), 
  data = rich, 
  family = gaussian,
  iter = 10000,
  chains = 4,
  thin = 5000
)
summary(mod_rich)

rich %>%
  data_grid(year = seq_range(year, n = 101)) %>%    # add am to the prediction grid
  add_predicted_draws(mod_rich) %>%
  ggplot(aes(x = year, y = richness)) +
  theme_bw() +
  stat_lineribbon(aes(y = .prediction), .width = c(.95, .8, .5), color = "#08519C") +
  geom_point(data = rich) +
  scale_fill_brewer(palette = "BuGn") +
  facet_wrap(~ hexID)                                  # facet by hexID


## hierarchical model
## libraries
library(rstanarm)


b <- stan_glmer(
  richness ~ year + (year | hexID),
  family = gaussian(),
  data = rich,
  prior = normal(0, 2, autoscale = TRUE),
  prior_intercept = normal(0, 5, autoscale = TRUE),
  prior_covariance = decov(regularization = 2),
  prior_aux = cauchy(0, 1, autoscale = TRUE), 
  chains = 4,
  iter = 10000,
  cores = 3,
  # reproducible blogging
  seed = 20211116
)

# print model
summary(b, digits = 4)

# Get a dataframe: One row per posterior sample
df_posterior <- b %>% 
  as.data.frame() %>% 
  as_tibble()

## Manipulate data into a usable form for plotting
# For each sample, add the average intercept and average slope values to each
# participant's deviation from that average. These yields the intercept and
# slope parameters for each participant.
df_effects <- df_posterior %>%
  mutate(
    # Find all the columns with the pattern "b[(Intercept". Add the column
    # `(Intercept)` to each of those columns.
    across(
      .cols = matches("b\\[\\(Intercept"), 
      .fns = ~ . + `(Intercept)`
    ),
    # Again for slope
    across(
      .cols = matches("b\\[year"), 
      .fns = ~ . + year
    )
  )

# Convert to a long format
df_long_effects <- df_effects %>%
  select(matches("b\\[")) %>%
  rowid_to_column("draw") %>%
  tidyr::pivot_longer(
    cols = c(-draw),
    # when we make new columns with pivot_ functions, the
    # they get quotes
    names_to = "Parameter", 
    values_to = "Value"
  )

df_long_effects <- df_effects %>% 
  dplyr::select(starts_with("b[")) %>% 
  rownames_to_column("draw") %>%
  tidyr::gather(Parameter, Value, -draw)

# Extract the effect type and subject number from each parameter name
df_long_effects$Type <- df_long_effects$Parameter %>%
  stringr::str_detect("Intercept") %>%
  ifelse(., "Intercept", "Slope_year")

df_long_effects$hexID <- df_long_effects$Parameter %>%
  stringr::str_extract(c("\\d+|other"))

df_long_effects <- df_long_effects %>% 
  dplyr::select(draw, hexID, Effect = Type, Value)

# choose 50 posterior samples
df_samples <- df_long_effects %>%
  filter(draw %in% sample(1:4000, size = 50)) %>%
  tidyr::spread(Effect, Value)
df_samples

# # Extract the effect type and subject number from each parameter name
# df_long_effects <- df_long_effects %>% 
#   mutate(
#     Effect = Parameter %>% 
#       stringr::str_detect("Intercept") %>%
#       ifelse(., "Intercept", "Slope_year"),
#     hexID = Parameter %>%
#       stringr::str_extract("\\d\\d\\d")
#   ) %>% 
#   select(draw, hexID, Effect, Value)

# # Finally!
# df_long_effects
# #> # A tibble: 160,000 × 4
# #>     draw Subject Effect     Value
# #>    <int> <chr>   <chr>      <dbl>
# #>  1     1 308     Intercept 256.  
# #>  2     1 308     Slope_Day  19.5 
# #>  3     1 309     Intercept 208.  
# #>  4     1 309     Slope_Day   2.49
# #>  5     1 310     Intercept 197.  
# #>  6     1 310     Slope_Day   9.00
# #>  7     1 330     Intercept 281.  
# #>  8     1 330     Slope_Day   5.21
# #>  9     1 331     Intercept 307.  
# #> 10     1 331     Slope_Day   1.16
# #> # … with 159,990 more rows

# df_long_effects2 <- df_long_effects %>%
#   tidyr::pivot_wider(names_from = Effect, values_from = Value)

# # For reproducibility
# set.seed(20220330)
# 
# ## Choose 50 posterior samples for plotting
# df_samples <- df_long_effects %>%
#   group_by(draw, hexID) %>%
#   filter(draw %in% sample(1:4000, size = 50)) %>%
#   mutate(row = row_number()) %>%
#   tidyr::pivot_wider(names_from = Effect, values_from = Value) %>%
#   select(-row)

df_samples

# # plot up slope and intercept estimates
# ggplot(df_long_effects %>% tidyr::spread(Effect, Value)) + 
#   aes(x = Intercept, y = Slope_year) + 
#   stat_density_2d(aes(fill = ..level..), geom = "polygon") +
#   facet_wrap("hexID") + 
#   xlab("Intercept estimate") + 
#   ylab("Slope estimate") +
#   theme(legend.position = "bottom") +
#   guides(fill = "none")

# get rid of NAs
df_samples <- sapply(df_samples, as.character)
df_samples[is.na(df_samples)] <- " "
df_samples <- as.data.frame(df_samples)

df_samples2 <- df_samples %>% 
  group_by(draw, hexID) %>% 
  summarise_all(funs(trimws(paste(., collapse = '')))) -> df

df_samples2 %>%
  mutate(Intercept = as.numeric(Intercept),
         Slope_year = as.numeric(Slope_year))

## plot with posteriors for each site
# ggplot(rich) +
#   aes(x = year, y = richness) +
#   geom_abline(
#     aes(intercept = Intercept, slope = Slope_year), 
#     data = df_samples2, 
#     color = "#3366FF", 
#     alpha = .1
#   ) +
#   geom_point() +
#   facet_wrap("hexID") + 
#   scale_x_continuous(breaks = 0:4 * 2) + 
#   labs(x = xlab, y = ylab) 

# Figure 3 final
# plot all stations with posterior draws
richplot2 <- ggplot(rich) +
  aes(x = year, y = richness) +
  geom_abline(aes(intercept = Intercept, slope = Slope_year), 
              data = df_samples, color = "dark gray", alpha = .2) +
  # geom_abline(aes(intercept = b(Intercept), slope = b(Slope)), data = rich) + 
  geom_point(alpha = 0.7, pch = 21, aes(fill = as.factor(hexID)), size = 2) +
  scale_fill_brewer(palette = "BrBG") +
  theme_classic() + 
  facet_wrap("hexID", ncol = 5) +
  xlab("Year") +
  ylab("Fish species richness") +
  # theme(axis.title.x = element_blank(), 
  #       axis.title.y = element_blank(),
  #       axis.text.x = element_blank(),
  #       axis.text.y = element_blank()) +
  #ylab(expression(paste("ln(Chl ", italic("a"),") (", mu,g," ", L^-1,")"))) +
  guides(color = guide_legend(title = "Year")) + # to make three columns, ncol = 3)) + 
  theme(legend.position = "none") # to move to bottom: c(.75,.1)) +
print(richplot2)

ggsave(richplot2, filename = "figures/longfishrichness.png", dpi = 300, width = 10, height = 5)



# moving window manual
# trellis of fish richness vs. TP by month
# non-pooled
xyplot(richness ~ year|as.factor(hexID), data = rich, panel=function(x,y,...){
  panel.xyplot(x, y, ...)
  panel.lmline(x,y,...)}, ylab="Richness", xlab="Year")
lm <- lmer(richness ~ year + (1|hexID), data = rich)
summary(lm)

rich1980 <- subset(rich, year >1979)
xyplot(richness ~ year|as.factor(hexID), data = rich1980, panel=function(x,y,...){
  panel.xyplot(x, y, ...)
  panel.lmline(x,y,...)}, ylab="Richness", xlab="Year")

rich1990 <- subset(rich, year >1989)
xyplot(richness ~ year|as.factor(hexID), data = rich1990, panel=function(x,y,...){
  panel.xyplot(x, y, ...)
  panel.lmline(x,y,...)}, ylab="Richness", xlab="Year")

rich2000 <- subset(rich, year >1999)
xyplot(richness ~ year|as.factor(hexID), data = rich2000, panel=function(x,y,...){
  panel.xyplot(x, y, ...)
  panel.lmline(x,y,...)}, ylab="Richness", xlab="Year")

rich2010 <- subset(rich, year >2009)
xyplot(richness ~ year|as.factor(hexID), data = rich2010, panel=function(x,y,...){
  panel.xyplot(x, y, ...)
  panel.lmline(x,y,...)}, ylab="Richness", xlab="Year")

library(nlme)
fit <-lme(richness ~ year, random = ~1 + year | hexID, data= rich)
summary(fit)

# make predictions dataframe
new_data <- data.frame(expand.grid(year = unique(rich$year), hexID = unique(rich$hexID), stringsAsFactors = FALSE))

# add predictions 
new_data$richness <- predict(fit, newdata = new_data, re.form=NA)

# # OPTIONAL EXTRA
# # get confidence and prediction intervals around mean prediction 
# # get model matrix
# mm <- model.matrix(terms(fit_lme), new_data)
# 
# ## or newdat$distance <- mm %*% fixef(fm1)
# # variance of fixed effect
# pvar <- diag(mm %*% tcrossprod(vcov(fit_lme), mm))
# # variance of random effects
# tvar <- pvar + VarCorr(fit_lme)$individual[1]  
# cmult <- 1.96
# 
# # add columns to dataframes
# # CI gives uncertainty around fixed effects only
# # PI gives uncertainty based on random effects as well!
# new_data <- mutate(new_data,
#                    PI_low = Hue - cmult*sqrt(tvar),
#                    PI_high = Hue + cmult*sqrt(tvar),
#                    CI_low = Hue - cmult*sqrt(pvar), 
#                    CI_high = Hue + cmult*sqrt(pvar))
# 
# # create dataframe for new_data for each individual ####
# new_data_ind <- select(d, time, prawn_col, individual)
# # predict each individuals relationship including the random effects
# new_data_ind$Hue <- predict(fit_lme, new_data_ind, re.form = NULL)

# option 2
# plots the mean relationship and each individual random relationship
ggplot(rich) +
  geom_point(aes(year, richness, fill = hexID), alpha = 0.5, pch = 21, size = 2, rich) +
  # geom_line(aes(year, richness, col = hexID, group = hexID), alpha = 0.25, new_data_ind) +
  geom_line(aes(year, richness, col = hexID), new_data) +
  ylab('Fish species richness') +
  xlab('Years') +
  # facet_wrap(~ prawn_col) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  theme_bw(base_size = 14)

## package rshift
library(rshift)

# how long for each regime shift?
# start with 5
# next try 8
# end with 10

## window = 8 ----
## hexID 216
# first attempt!
hex216 <- filter(rich, hexID == "216")
hex216red <- hex216[,-1]
# for answer
mod1 <- Rodionov(hex216red, col = "richness", time = "year", l = 8)
mod1
# for figure
mod1 <- Rodionov(hex216red, col = "richness", time = "year", l = 8, merge = TRUE)
mod1
RSI_graph(mod1, col = "richness", time = "year", rsi ="RSI")

## hexID 217
hex217 <- filter(rich, hexID == "217")
hex217red <- hex217[,-1]
# for answer
mod2 <- Rodionov(hex217red, col = "richness", time = "year", l = 8)
mod2
# for figure
mod2 <- Rodionov(hex217red, col = "richness", time = "year", l = 8, merge = TRUE)
RSI_graph(mod2, col = "richness", time = "year", rsi ="RSI")

## hexID 247
hex247 <- filter(rich, hexID == "247")
hex247red <- hex247[,-1]
# for answer
mod3 <- Rodionov(hex247red, col = "richness", time = "year", l = 8)
mod3
# for figure
mod3 <- Rodionov(hex247red, col = "richness", time = "year", l = 8, merge = TRUE)
RSI_graph(mod3, col = "richness", time = "year", rsi ="RSI")


## hexID 248
hex248 <- filter(rich, hexID == "248")
hex248red <- hex248[,-1]
# for answer
mod4 <- Rodionov(hex248red, col = "richness", time = "year", l = 8)
mod4


## hexID 262
hex262 <- filter(rich, hexID == "262")
hex262red <- hex262[,-1]
# for answer
mod5 <- Rodionov(hex262red, col = "richness", time = "year", l = 8)
mod5


## hexID 263
hex263 <- filter(rich, hexID == "263")
hex263red <- hex263[,-1]
# for answer
mod6 <- Rodionov(hex263red, col = "richness", time = "year", l = 8)
mod6


## hexID 279
hex279 <- filter(rich, hexID == "279")
hex279red <- hex279[,-1]
# for answer
mod7 <- Rodionov(hex279red, col = "richness", time = "year", l = 8)
mod7


## hexID 344
hex344 <- filter(rich, hexID == "344")
hex344red <- hex344[,-1]
# for answer
mod8 <- Rodionov(hex344red, col = "richness", time = "year", l = 8)
mod8


## hexID 1370
hex1370 <- filter(rich, hexID == "1370")
hex1370red <- hex1370[,-1]
# for answer
mod9 <- Rodionov(hex1370red, col = "richness", time = "year", l = 8)
mod9


## hexID 1372
hex1372 <- filter(rich, hexID == "1372")
hex1372red <- hex1372[,-1]
# for answer
mod10 <- Rodionov(hex1372red, col = "richness", time = "year", l = 8)
mod10




## window = 10 ----
## hexID 216
# first attempt!
hex216 <- filter(rich, hexID == "216")
hex216red <- hex216[,-1]
# for answer
mod1 <- Rodionov(hex216red, col = "richness", time = "year", l = 10)
mod1
# for figure
mod1 <- Rodionov(hex216red, col = "richness", time = "year", l = 10, merge = TRUE)
mod1
RSI_graph(mod1, col = "richness", time = "year", rsi ="RSI")

## hexID 217
hex217 <- filter(rich, hexID == "217")
hex217red <- hex217[,-1]
# for answer
mod2 <- Rodionov(hex217red, col = "richness", time = "year", l = 10)
mod2
# for figure
mod2 <- Rodionov(hex217red, col = "richness", time = "year", l = 10, merge = TRUE)
RSI_graph(mod2, col = "richness", time = "year", rsi ="RSI")

## hexID 247
hex247 <- filter(rich, hexID == "247")
hex247red <- hex247[,-1]
# for answer
mod3 <- Rodionov(hex247red, col = "richness", time = "year", l = 10)
mod3
# for figure
mod3 <- Rodionov(hex247red, col = "richness", time = "year", l = 10, merge = TRUE)
RSI_graph(mod3, col = "richness", time = "year", rsi ="RSI")


## hexID 248
hex248 <- filter(rich, hexID == "248")
hex248red <- hex248[,-1]
# for answer
mod4 <- Rodionov(hex248red, col = "richness", time = "year", l = 10)
mod4


## hexID 262
hex262 <- filter(rich, hexID == "262")
hex262red <- hex262[,-1]
# for answer
mod5 <- Rodionov(hex262red, col = "richness", time = "year", l = 10)
mod5


## hexID 263
hex263 <- filter(rich, hexID == "263")
hex263red <- hex263[,-1]
# for answer
mod6 <- Rodionov(hex263red, col = "richness", time = "year", l = 10)
mod6


## hexID 279
hex279 <- filter(rich, hexID == "279")
hex279red <- hex279[,-1]
# for answer
mod7 <- Rodionov(hex279red, col = "richness", time = "year", l = 10)
mod7


## hexID 344
hex344 <- filter(rich, hexID == "344")
hex344red <- hex344[,-1]
# for answer
mod8 <- Rodionov(hex344red, col = "richness", time = "year", l = 10)
mod8


## hexID 1370
hex1370 <- filter(rich, hexID == "1370")
hex1370red <- hex1370[,-1]
# for answer
mod9 <- Rodionov(hex1370red, col = "richness", time = "year", l = 10)
mod9


## hexID 1372
hex1372 <- filter(rich, hexID == "1372")
hex1372red <- hex1372[,-1]
# for answer
mod10 <- Rodionov(hex1372red, col = "richness", time = "year", l = 10)
mod10


# plot as unpooled
library(ggplot2)

#create regression lines for all three groups
longfish2 <- ggplot(rich, aes(x = year, y = richness)) +
  geom_point(pch = 21, size = 2, aes(fill = hexID), alpha = 0.5) +
  geom_smooth(method = "lm", fill = NA) +
  theme_bw(base_size = 14) +
  scale_fill_viridis_d() +
  facet_wrap(~hexID, ncol = 5)
print(longfish2)
