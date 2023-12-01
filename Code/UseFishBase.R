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
allarid <- read.csv("Data/fish_traits.csv", header = TRUE)
# make a vector of fish names
fish <- allarid[,1]
unique(fish)

# get TL estimates for arid fishes
estaridfish <- estimate(fish) #MaxLengthTL, Troph, seTroph, TempPrefMin, TempPrefMax, TempPrefMean, but the temp pref are rarer




# make histograms comparing
length <- ggplot(allfish, aes(x = MaxLengthTL), fill = "darkblue") +
  geom_histogram(binwidth = 5) +
  xlab("Maximum total length (cm)") +
  geom_histogram(
    data = allarid,
    aes(x = maxtl_cm),
    fill = "gold",
    binwidth = 5
  ) +
  theme_bw(base_size = 14) +
  scale_x_continuous(limits = c(0, 200))
length


TLplot <- ggplot(allfish, aes(x = Troph), fill = "darkblue") +
  geom_histogram(binwidth = 0.2) +
  xlab("Trophic level estimate") +
  geom_histogram(
    data = estaridfish,
    aes(x = Troph),
    fill = "gold",
    binwidth = 0.2
  ) +
  theme_bw(base_size = 14)
TLplot


ageplot <- ggplot(allfish, aes(x = AgeMax), fill = "darkblue") +
  geom_histogram(binwidth = 5) +
  xlab("Maximum longevity (yrs)") +
  geom_histogram(
    data = allarid,
    aes(x = longevity),
    fill = "gold",
    binwidth = 5
  ) +
  scale_x_continuous(limits = c(0,110)) +
  theme_bw(base_size = 14)
ageplot

# combined traits
combplot <- ggpubr::ggarrange(
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
combplot

ggsave(combplot, filename = "figures/traithistograms.png", dpi = 300, height = 12, width = 5)
