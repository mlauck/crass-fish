# Venn diagrams and danger fish vs. rest

# libraries
library(ggplot2)
library(ggvenn)
library(coin)
library(rcompanion)
library(FSA)

# load traits data
# traits <- read.csv("Data/allaridtraits.csv", header = TRUE)

traits <- read.csv("FinalData/Traits_MatchedWithOccurrence.csv", header = TRUE)

# IUCN status as factor
traits$IUCNstatus <- as.factor(traits$IUCNstatus)

# make a danger fish vector
traits$danger <- NA
traits$danger <-
  ifelse(traits$IUCNstatus == "LC" |
           traits$IUCNstatus == "NE" |
           traits$IUCNstatus == "DD",
         0,
         1)
head(traits)

# only danger
dangerfish <- traits %>%
  filter(danger == 1)

# rest of the fishes
rest <- traits %>%
  filter(danger == 0)


# make histograms comparing
# length ----
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
  theme (panel.grid.major = element_blank (),
         panel.grid.minor = element_blank()) +
  scale_x_continuous(limits = c(0, 200))
length

# danger fish are smaller!
ggplot(aes(y = maxtl_cm, x = danger, group = danger), data = traits) +
  geom_boxplot() +
  theme_bw(base_size = 14)
summary(lm(maxtl_cm ~ danger, traits))

# permutation test
permtest <- oneway_test(maxtl_cm ~ as.factor(danger), data = traits, nresample = 10000)
permtest


PT <- pairwisePermutationTest(maxtl_cm ~ as.factor(danger),
                              data = traits,
                              method="fdr")

PT

cldList(p.adjust ~ Comparison,
        data = PT,
        threshold  = 0.05)

# trophic level ----
TLplot <- ggplot(rest, aes(x = FoodTroph), fill = "darkblue") +
  geom_histogram(binwidth = 0.2) +
  xlab("Trophic level estimate") +
  geom_histogram(
    data = dangerfish,
    aes(x = FoodTroph),
    fill = "gold",
    binwidth = 0.2
  ) +
  theme_bw(base_size = 14) +
  theme (panel.grid.major = element_blank(), panel.grid.minor = element_blank())
TLplot

# danger fish do not differ in diet
ggplot(aes(y = FoodTroph, x = danger, group = danger), data = traits) +
  geom_boxplot() +
  theme_bw(base_size = 14)
summary(lm(FoodTroph ~ danger, traits))

permtest <- oneway_test(FoodTroph ~ as.factor(danger), data = traits, nresample = 10000)
permtest

# longevity ----
ageplot <- ggplot(rest, aes(x = longevity), fill = "darkblue") +
  geom_histogram(binwidth = 5) +
  xlab("Maximum longevity (yrs)") +
  geom_histogram(
    data = dangerfish,
    aes(x = longevity),
    fill = "gold",
    binwidth = 5
  ) +
  scale_x_continuous(limits = c(0, 110)) +
  theme_bw(base_size = 14) +
  theme (panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ageplot

# danger fish do not differ in longevity
ggplot(aes(y = longevity, x = danger, group = danger), data = traits) +
  geom_boxplot() +
  theme_bw(base_size = 14)
summary(lm(log(longevity) ~ danger, traits))

permtest <- oneway_test(longevity ~ as.factor(danger), data = traits, nresample = 10000)
permtest

# combined traits
arid_v_danger_hist <- ggpubr::ggarrange(
  length,
  ageplot,
  TLplot,
  nrow = 3,
  ncol = 1,
  align = "hv",
  legend = "none",
  common.legend = TRUE,
  labels = "AUTO"
)
arid_v_danger_hist

ggsave(
  arid_v_danger_hist,
  filename = "figures/traithistograms_onlyarid.png",
  dpi = 600,
  height = 12,
  width = 5
)


## Make Venn diagrams
# select only the species names and IUCN reasons
IUCN <-
  subset(dangerfish,
         select = c (GenusSpecies, list1, list2, list3, list4, list5))

# figure out how many have multiple reasons
reasons <- IUCN %>%
  mutate(reason_sum = rowSums(across(c(list1, list2, list3, list4, list5))))
  # group_by(GenusSpecies) %>%
  # mutate(sum = rowSums(.[,2:6]))

# ggvenn
str(IUCN)
# IUCN$list1 <- IUCN$Habitat

IUCN$GenusSpecies <- as.factor(IUCN$GenusSpecies)
venn <- ggplot(IUCN, aes(
  A = list1,
  B = list5,
  C = list3,
  D = list4
)) +
  geom_venn(
    fill_color = c("#ffffcc",
                   "#a1dab4",
                   "#41b6c4",
                   "#225ea8"),
    set_names = c("Habitat", "Small range", "Disease", "Other factors")
  ) +
  theme_void()
ggsave(venn, filename = "figures/VennDiagram.png", dpi = 400, width = 9, height = 6)
