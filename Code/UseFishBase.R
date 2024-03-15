# Make histograms of freshwater fish traits

# libraries
library(rfishbase)
library(dplyr)
library(ggplot2)
library(ggpubr)

# load data to make fish.data
fish.data <- fb_tbl("ecology")
fish.est <- fb_tbl("estimate")


# limit to only stream fish
streamfish <- fish.data %>%
  filter(Stream < 0)

# join them together
allfish <- left_join(streamfish, fish.est, by = "SpecCode")

# load traits data
allarid <- read.csv("FinalData/Traits_MatchedWithOccurancePlusTrophic.csv", header = TRUE)
# make a vector of fish names
fish <- allarid[,1]
unique(fish)

# get TL estimates for arid fishes
estaridfish <- estimate(fish) #MaxLengthTL, Troph, seTroph, TempPrefMin, TempPrefMax, TempPrefMean, but the temp pref are rarer




# make histograms comparing
length <- ggplot(allfish, aes(x = MaxLengthTL)) +
  geom_histogram(binwidth = 5,
                 fill = "#1b0c41") +
  xlab("Maximum total length (cm)") +
  geom_histogram(
    data = allarid,
    aes(x = maxtl_cm),
    fill = "#f7d13d",
    binwidth = 5
  ) +
  ylab("Count") +
  theme_bw(base_size = 14) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  scale_x_continuous(limits = c(0, 200))
length


TLplot <- ggplot(allfish, aes(x = Troph)) +
  geom_histogram(binwidth = 0.2,
                 fill = "#1b0c41") +
  xlab("Trophic level estimate") +
  ylab("Count") +
  geom_histogram(
    data = estaridfish,
    aes(x = Troph),
    fill = "#f7d13d",
    binwidth = 0.2
  ) +
  theme_bw(base_size = 14) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )
TLplot


ageplot <- ggplot(allfish, aes(x = AgeMax)) +
  geom_histogram(binwidth = 5,
                 fill = "#1b0c41") +
  xlab("Maximum longevity (yrs)") +
  geom_histogram(
    data = allarid,
    aes(x = longevity),
    fill = "#f7d13d",
    binwidth = 5
  ) +
  ylab("Count") +
  scale_x_continuous(limits = c(0,110)) +
  theme_bw(base_size = 14) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )
ageplot

# combined traits
combplot <- ggpubr::ggarrange(
  length,
  TLplot,
  ageplot,
  nrow = 3,
  ncol = 1,
  align = "hv",
  legend = "none",
  common.legend = TRUE,
  labels = "AUTO"
)
combplot

ggsave(combplot, filename = "figures/traithistograms.png", dpi = 300, height = 12, width = 5)
