# Venn diagrams and danger fish vs. rest

# libraries
library(ggplot2)
library(ggvenn)


# load traits data
traits <- read.csv("Data/allaridtraits.csv", header = TRUE)

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
  scale_x_continuous(limits = c(0, 200))
length

# danger fish are smaller!
ggplot(aes(y = maxtl_cm, x = danger, group = danger), data = traits) +
  geom_boxplot() +
  theme_bw(base_size = 14)
summary(lm(maxtl_cm ~ danger, traits))

# try a ggridge plot for kicks
traits %>%
  mutate(IUCNstatus = reorder(c("LC", "NE", "VU", "EN", "CR", "EX"))) %>%
  ggplot(aes(y = IUCNstatus)) +
  geom_density_ridges(
    aes(x = maxtl_cm, fill = paste(YearFct, Option)), 
    alpha = .8, color = "white", from = 0, to = 100
  ) +
  labs(
    x = "Vote (%)",
    y = "Election Year",
    title = "Indy vs Unionist vote in Catalan elections",
    subtitle = "Analysis unit: municipalities (n = 949)",
    caption = "Marc Belzunces (@marcbeldata) | Source: Idescat"
  ) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_cyclical(
    breaks = c("1980 Indy", "1980 Unionist"),
    labels = c(`1980 Indy` = "Indy", `1980 Unionist` = "Unionist"),
    values = c("#ff0000", "#0000ff", "#ff8080", "#8080ff"),
    name = "Option", guide = "legend"
  ) +
  coord_cartesian(clip = "off") +
  theme_ridges(grid = FALSE)

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
  theme_bw(base_size = 14)
TLplot

# danger fish do not differ in diet
ggplot(aes(y = FoodTroph, x = danger, group = danger), data = traits) +
  geom_boxplot() +
  theme_bw(base_size = 14)
summary(lm(FoodTroph ~ danger, traits))

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
  theme_bw(base_size = 14)
ageplot

# danger fish do not differ in longevity
ggplot(aes(y = longevity, x = danger, group = danger), data = traits) +
  geom_boxplot() +
  theme_bw(base_size = 14)
summary(lm(log(longevity) ~ danger, traits))

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

ggsave(
  arid_v_danger_hist,
  filename = "figures/traithistograms_onlyarid.png",
  dpi = 300,
  height = 12,
  width = 5
)


## Make Venn diagrams
# select only the species names and IUCN reasons
IUCN <-
  subset(dangerfish,
         select = c (GenusSpecies, list1, list2, list3, list4, list5))



# ggvenn
str(IUCN)
# IUCN$list1 <- IUCN$Habitat

IUCN$GenusSpecies <- as.factor(IUCN$GenusSpecies)
ggplot(IUCN, aes(
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