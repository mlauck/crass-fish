# stacked bar graphs
# code created by FER
# Last edit 25 April 2025


# example data
df <- data.frame(
  var1 = c(TRUE, FALSE, NA, TRUE, TRUE, FALSE, NA),
  var2 = c(FALSE, NA, TRUE, FALSE, TRUE, FALSE, NA)
)

library(tidyverse)

df_long <- df %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
  mutate(value = as.character(value)) %>%  # So NA is treated as a category
  replace_na(list(value = "NA")) %>%
  group_by(variable, value) %>%
  summarise(count = n(), .groups = "drop")

ggplot(df_long, aes(x = variable, y = count, fill = value)) +
  geom_bar(stat = "identity") +
  labs(title = "Stacked Bar of TRUE/FALSE/NA", x = "Variable", y = "Count") +
  scale_fill_viridis_d(option = "magma") +
  # scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "tomato", "NA" = "gray")) +
  theme_minimal() +
  coord_flip()

# libraries
library(tidyverse)
library(viridis)
library(ggplot2)

# Get data
allarid <- read.csv("FinalData/Traits_MatchedWithOccurancePlusTrophic.csv", header = TRUE)
arid_long <- allarid %>%
  pivot_longer(cols = algphyto:other, names_to = "food", values_to = "value") %>%
  mutate(value = as.character(value)) %>%  # So NA is treated as a category
  replace_na(list(value = "NA")) %>%
  group_by(food, value) %>%
  summarise(count = n(), .groups = "drop")

# Compute proportion of TRUE for ordering
prop_true <- arid_long %>%
  filter(value != "NA") %>%
  group_by(food) %>%
  summarise(prop_true = mean(value == "TRUE"))  # proportion of TRUEs

# Join back to reorder variable
arid_long <- arid_long %>%
  left_join(prop_true, by = "food") %>%
  mutate(food = fct_reorder(food, prop_true))

# plot
ggplot(arid_long, aes(x = food, y = count, fill = value)) +
  geom_bar(stat = "identity") +
  # scale_y_continuous(labels = scales::percent) +
  labs(title = "Stacked Bar of TRUE/FALSE/NA", x = "Diet", y = "Count") +
  scale_fill_viridis_d(option = "magma") +
  # scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "tomato", "NA" = "gray")) +
  theme_minimal() +
  coord_flip()

