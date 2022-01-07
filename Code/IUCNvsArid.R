# Comparing IUCN lists of all fishes vs. arid fishes
# FER
# Last edit 7 Jan 2022

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
IUCN_df2$IUCNstatus <- IUCN_df2$redListCategory

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

arid_df <- as.data.frame(arid_df)

## merge df together
alldata <- dplyr::left_join(IUCN_df2,
                 arid_df,
                 by = "IUCNstatus")


# regular barplot
barplot <- ggplot(plot_df2, aes(x = redListCategory, y = prop, fill = n)) +
  #geom_bar(stat = "identity") +
  geom_col(
    aes(
      x = reorder(str_wrap(redListCategory, 5), n),
      y = prop,
      fill = n
    ),
    position = "dodge2",
    show.legend = TRUE,
    alpha = .9
  ) +
  geom_col(
    data = arid_df,
    aes(x = IUCNstatus,
    y = prop,
    fill = n
  ),
  alpha = 0.9,
  show.legend = TRUE) +
  scale_fill_viridis_c(option = "magma") +
  theme_classic(base_size = 14)
print(barplot)

# circular barplot ----
plt <- ggplot(plot_df2) +
  # Make custom panel grid
  geom_hline(
    aes(yintercept = y), 
    data.frame(y = c(0:6) * 1000),
    color = "lightgrey"
  ) + 
  # Add bars to represent the cumulative track lengths
  # str_wrap(region, 5) wraps the text so each line has at most 5 characters
  # (but it doesn't break long words!)
  geom_col(
    aes(
      x = reorder(str_wrap(redListCategory, 5), n),
      y = n,
      fill = n
    ),
    position = "dodge2",
    show.legend = TRUE,
    alpha = .9
  ) +
  
  # Add dots to represent the mean gain
  geom_point(
    aes(
      x = reorder(str_wrap(redListCategory, 5),n),
      y = n
    ),
    size = 3,
    color = "gray12"
  ) +
  
  # Lollipop shaft for mean gain per region
  geom_segment(
    aes(
      x = reorder(str_wrap(redListCategory, 5), n),
      y = 0,
      xend = reorder(str_wrap(redListCategory, 5), n),
      yend = 6000
    ),
    linetype = "dashed",
    color = "gray12"
  ) + 
  
  # Make it circular!
  coord_polar()

plt

# change it up a bit
IUCNfishes <- plt + theme_bw(base_size = 10) +
  # Scale y axis so bars don't start in the center
  scale_y_continuous(
    limits = c(-500, 6500),
    expand = c(0, 0),
    breaks = c(0, 1000, 2000, 3000, 4000, 5000, 6000)
  ) + 
  # New fill and legend title for number of tracks per region
  scale_fill_gradientn(
    "Number of fish species",
    colours = c( "#6C5B7B","#C06C84","#F67280","#F8B195")
  ) +
  # Make the guide for the fill discrete
  guides(
    fill = guide_colorsteps(
      barwidth = 15, barheight = .5, title.position = "top", title.hjust = .5
    )
  ) +
  theme(
    # Remove axis ticks and text
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    # Use gray text for the region names
    axis.text.x = element_text(color = "gray12", size = 10),
    # Move the legend to the bottom
    legend.position = "bottom"
  )

#ggsave(IUCNfishes, filename = glue("figures/alldishes_IUCNstatus_{Sys.Date()}.png"), width = 6, height = 6, dpi = 300)
