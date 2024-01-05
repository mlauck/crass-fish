# Venn diagram

# # devtools install
# if (!require(devtools)) install.packages("devtools")
# devtools::install_github("yanlinlin82/ggvenn")
# 
# # library
# library(devtools)
# library(ggplot2)
# library(dplyr)
# 
# set.seed(20190708)
# genes <- paste("gene",1:1000,sep="")
# x <- list(
#   A = sample(genes,300), 
#   B = sample(genes,525), 
#   C = sample(genes,440),
#   D = sample(genes,350)
# )
# 
# # Change category names
# # Change fill color
# display_venn(
#   x,
#   category.names = c("Set 1" , "Set 2 " , "Set 3", "Set 4"),
#   fill = c("#999999", "#E69F00", "#56B4E9", "#009E73")
# )
# 
# 
# # Further customization
# display_venn(
#   x,
#   category.names = c("Set 1" , "Set 2 " , "Set 3", "Set 4"),
#   # Circles
#   lwd = 2,
#   lty = 'blank',
#   fill = c("#999999", "#E69F00", "#56B4E9", "#009E73"),
#   # Numbers
#   cex = .9,
#   fontface = "italic",
#   # Set names
#   cat.cex = 1,
#   cat.fontface = "bold",
#   cat.default.pos = "outer",
#   cat.dist = c(0.055, 0.055, 0.1, 0.1)
# )


# try to make a list with arid data
# load traits data
traits <- read.csv("Data/allaridtraits.csv", header = TRUE)

# make a danger fish vector
traits$danger <- NA
traits$danger <-
  ifelse(traits$IUCNstatus == "LC" |
           traits$IUCNstatus == "NE"|
           traits$IUCNstatus == "DD", 0, 1)
head(traits)

# only danger
dangerfish <- traits %>%
  filter(danger == 1)

# rest of the fishes
rest <- traits %>%
  filter(danger == 0)


# make histograms comparing
length <- ggplot(rest, aes(x = maxtl_cm), fill = "darkblue") +
  geom_histogram(binwidth = 5) +
  xlab("Maximum total length (cm)") +
  geom_histogram(
    data = dangerfish,
    aes(x = maxtl_cm),
    fill = "gold",
    binwidth = 5
  ) +
  theme_bw(base_size = 14) +
  scale_x_continuous(limits = c(0, 200))
length


TLplot <- ggplot(rest, aes(x = FoodTroph), fill = "darkblue") +
  geom_histogram(binwidth = 0.2) +
  xlab("Trophic level estimate") +
  geom_histogram(
    data = dangerfish,
    aes(x = FoodTroph),
    fill = "gold",
    binwidth = 0.2
  ) +
  theme_bw(base_size = 14)
TLplot


ageplot <- ggplot(rest, aes(x = longevity), fill = "darkblue") +
  geom_histogram(binwidth = 5) +
  xlab("Maximum longevity (yrs)") +
  geom_histogram(
    data = dangerfish,
    aes(x = longevity),
    fill = "gold",
    binwidth = 5
  ) +
  scale_x_continuous(limits = c(0,110)) +
  theme_bw(base_size = 14)
ageplot

# combined traits
arid_v_danger_hist <- ggpubr::ggarrange(
  ageplot,
  length,
  TLplot,
  nrow = 3,
  ncol = 1,
  align = "hv",
  legend = "none",
  common.legend = TRUE,
  labels = "AUTO"
)
arid_v_danger_hist

ggsave(arid_v_danger_hist, filename = "figures/traithistograms_onlyarid.png", dpi = 300, height = 12, width = 5)

# select only the species names and IUCN reasons
IUCN <- subset(dangerfish, select = c (GenusSpecies, list1, list2, list3, list4, list5))

# # make vectors for each listing reason
# IUCN1 <- IUCN %>%
#   group_by(GenusSpecies) %>%
#   filter(list1 == TRUE) %>%
#   select(GenusSpecies)
# 
# IUCN2 <- IUCN %>%
#   group_by(GenusSpecies) %>%
#   filter(list2 == TRUE) %>%
#   select(GenusSpecies)
# 
# IUCN3 <- IUCN %>%
#   group_by(GenusSpecies) %>%
#   filter(list3 == TRUE) %>%
#   select(GenusSpecies)
# 
# IUCN4 <- IUCN %>%
#   group_by(GenusSpecies) %>%
#   filter(list4 == TRUE) %>%
#   select(GenusSpecies)

# # make list
# x <- list(IUCN1, IUCN2, IUCN3, IUCN4)
# 
# list_venn <- list(
#   A = IUCN1[,1], 
#   B = IUCN2[,1], 
#   C = IUCN3[,1],
#   D = IUCN4[,1]
# )


# libraries
library(ggplot2)
library(ggvenn)

# ggvenn
# make data_venn
str(IUCN)
IUCN$list1 <- IUCN$Habitat

IUCN$GenusSpecies <- as.factor(IUCN$GenusSpecies)
ggplot(IUCN, aes(
  A = list1,
  B = list5,
  C = list3,
  D = list4
)) +
  geom_venn(fill_color = c("#ffffcc",
    "#a1dab4",
    "#41b6c4",
    "#225ea8"),
    set_names = c("Habitat","Small range", "Disease", "Other factors")) +
  theme_void()

# library("ggVennDiagram")
# ggVennDiagram(IUCN, aes(
#   A = list1,
#   B = list5,
#   C = list3,
#   D = list4
# )) +
#   # category.names = c("Habitat", "Small range", "Disease", "Other factors") +
#   geom_venn() +
#   theme_void()
