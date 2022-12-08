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

library(brms)
library(magrittr)
library(dplyr)
library(ggplot2)
library(rstan)
library(tidybayes)
library(emmeans)
library(broom)
library(modelr)
library(forcats)
library(RColorBrewer)


mod_rich = brm(
  richness ~ year + (1|hexID), 
  data = rich, 
  family = gaussian,
)
summary(mod_rich)

rich %>%
  data_grid(year = seq_range(year, n = 101)) %>%    # add am to the prediction grid
  add_predicted_draws(mod_rich) %>%
  ggplot(aes(x = year, y = richness)) +
  stat_lineribbon(aes(y = .prediction), .width = c(.99, .95, .8, .5), color = "#08519C") +
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
      .cols = matches("b\\[Day"), 
      .fns = ~ . + Days
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

# Extract the effect type and subject number from each parameter name
df_long_effects <- df_long_effects %>% 
  mutate(
    Effect = Parameter %>% 
      stringr::str_detect("Intercept") %>%
      ifelse(., "Intercept", "Slope_Day"),
    Subject = Parameter %>%
      stringr::str_extract("\\d\\d\\d")
  ) %>% 
  select(draw, Subject, Effect, Value)

# Finally!
df_long_effects
#> # A tibble: 160,000 × 4
#>     draw Subject Effect     Value
#>    <int> <chr>   <chr>      <dbl>
#>  1     1 308     Intercept 256.  
#>  2     1 308     Slope_Day  19.5 
#>  3     1 309     Intercept 208.  
#>  4     1 309     Slope_Day   2.49
#>  5     1 310     Intercept 197.  
#>  6     1 310     Slope_Day   9.00
#>  7     1 330     Intercept 281.  
#>  8     1 330     Slope_Day   5.21
#>  9     1 331     Intercept 307.  
#> 10     1 331     Slope_Day   1.16
#> # … with 159,990 more rows


# For reproducibility
set.seed(20220330)

## Choose 50 posterior samples for plotting
df_samples <- df_long_effects %>%
  filter(draw %in% sample(1:4000, size = 50)) %>%
  tidyr::pivot_wider(names_from = Effect, values_from = Value)
df_samples


## plot with posteriors for each site
ggplot(df_sleep) +
  aes(x = Days, y = Reaction) +
  geom_abline(
    aes(intercept = Intercept, slope = Slope_Day), 
    data = df_samples, 
    color = "#3366FF", 
    alpha = .1
  ) +
  geom_point() +
  facet_wrap("Subject") + 
  scale_x_continuous(breaks = 0:4 * 2) + 
  labs(x = xlab, y = ylab) 
