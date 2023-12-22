# Venn diagram

# devtools install
if (!require(devtools)) install.packages("devtools")
devtools::install_github("yanlinlin82/ggvenn")

# library
library(devtools)
library(ggplot2)
library(dplyr)

set.seed(20190708)
genes <- paste("gene",1:1000,sep="")
x <- list(
  A = sample(genes,300), 
  B = sample(genes,525), 
  C = sample(genes,440),
  D = sample(genes,350)
)

# Change category names
# Change fill color
display_venn(
  x,
  category.names = c("Set 1" , "Set 2 " , "Set 3", "Set 4"),
  fill = c("#999999", "#E69F00", "#56B4E9", "#009E73")
)


# Further customization
display_venn(
  x,
  category.names = c("Set 1" , "Set 2 " , "Set 3", "Set 4"),
  # Circles
  lwd = 2,
  lty = 'blank',
  fill = c("#999999", "#E69F00", "#56B4E9", "#009E73"),
  # Numbers
  cex = .9,
  fontface = "italic",
  # Set names
  cat.cex = 1,
  cat.fontface = "bold",
  cat.default.pos = "outer",
  cat.dist = c(0.055, 0.055, 0.1, 0.1)
)


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

# select only the species names and IUCN reasons
IUCN <- subset(dangerfish, select = c (GenusSpecies, list1, list2, list3, list4, list5))

# make vectors for each listing reason
IUCN1 <- IUCN %>%
  group_by(GenusSpecies) %>%
  filter(list1 == TRUE) %>%
  select(GenusSpecies)

IUCN2 <- IUCN %>%
  group_by(GenusSpecies) %>%
  filter(list2 == TRUE) %>%
  select(GenusSpecies)

IUCN3 <- IUCN %>%
  group_by(GenusSpecies) %>%
  filter(list3 == TRUE) %>%
  select(GenusSpecies)

IUCN4 <- IUCN %>%
  group_by(GenusSpecies) %>%
  filter(list4 == TRUE) %>%
  select(GenusSpecies)

# make list
x <- list(IUCN1, IUCN2, IUCN3, IUCN4)

list_venn <- list(
  A = IUCN1[,1], 
  B = IUCN2[,1], 
  C = IUCN3[,1],
  D = IUCN4[,1]
)

# Further customization
display_venn(
  x,
  category.names = c("Set 1" , "Set 2 " , "Set 3", "Set 4"),
  # Circles
  lwd = 2,
  lty = 'blank',
  fill = c("#999999", "#E69F00", "#56B4E9", "#009E73"),
  # Numbers
  cex = .9,
  fontface = "italic",
  # Set names
  cat.cex = 1,
  cat.fontface = "bold",
  cat.default.pos = "outer",
  cat.dist = c(0.055, 0.055, 0.1, 0.1)
)

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
