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
xyplot(richness ~ year|as.factor(hexID), data = rich, panel=function(x,y,...){
  panel.xyplot(x, y, ...)
  panel.lmline(x,y,...)}, ylab="Richness", xlab="Year")


mod_rich = brm(
  richness ~ year + (year|hexID), 
  data = rich, 
  family = gaussian,
)
summary(mod_rich)

rich %>%
  data_grid(year = seq_range(year, n = 101)) %>%    # add am to the prediction grid
  add_predicted_draws(mod_rich) %>%
  ggplot(aes(x = year, y = richness)) +
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
  ncore = 3,
  # reproducible blogging
  seed = 20211116
)

# print model
b

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
