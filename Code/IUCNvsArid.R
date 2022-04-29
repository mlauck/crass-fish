# Comparing IUCN lists of all fishes vs. arid fishes
# FER
# Last edit 29 April 2022

## next week to do:
# melt data into one file

# libraries
library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)
library(stringr)
library(glue)
library(magrittr)
library(rfishbase)
library(data.table)

# load data
allIUCN <- read.csv("Data/IUCNassessments.csv", header = TRUE)
allarid <- read.csv("Data/fish_traits.csv", header = TRUE)

# get trophic level
fish <- allarid[,1]
food <- ecology(fish,
                fields=c("Species", "SpecCode", "FoodTroph", "FoodSeTroph"))

# join the arid data and trophic level
allarid <- merge(allarid, food, by.x = "GenusSpecies", by.y = "Species")


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
# IUCN_df2$IUCNstatus <- IUCN_df2$redListCategory
IUCN_df2 <- IUCN_df2 %>%
  dplyr::rename(IUCNstatus = redListCategory)

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

arid_df2 <- as.data.frame(arid_df2)

## merge df together
alldata <- rbind(IUCN_df2, arid_df2)

## add a column for source
alldata$source <- "arid"
alldata[1:min(8, nrow(alldata)),]$source <- "IUCN"
alldata

# remove line for not evaluated since it is not present in larger data
alldata <- alldata[alldata$IUCNstatus != "Not Evaluated", ]

# make sure factors are reading as such
alldata$IUCNstatus <- as.factor(alldata$IUCNstatus)
alldata$source <- as.factor(alldata$source)

## reorder data for plotting and ordinal regression

alldata$IUCNstatus <- factor(
  alldata$IUCNstatus,
  levels = c(
    "Extinct",
    "Extinct in the Wild",
    "Critically Endangered",
    "Endangered",
    "Near Threatened",
    "Vulnerable",
    "Least Concern",
    "Data Deficient",
    "Not Evaluated",
    ordered = TRUE
  )
)

alldata <- alldata[order(alldata$IUCNstatus), ]





# alldata2 %>%
#   filter(source == "arid") %>%
#   # group_by(source, IUCNstatus)
#   summarize(sum = sum(n))


## grouped barplot -----
barplot <- ggplot(alldata, aes(x = IUCNstatus, y = prop, group = source)) +
  geom_col(
    aes(
      x = IUCNstatus,
      y = prop,
      fill = source
    ),
    position = "dodge2",
    show.legend = TRUE,
    alpha = .9
  ) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 6)) +
  ylab("proportion of all fishes") +
  xlab("IUCN status") +
  scale_fill_viridis_d(name = "Source",
                       begin = 0.7,
                       end = 0.4,
                       option = "inferno") +
  theme_bw(base_size = 14) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )
print(barplot)

# ggsave(
#   device = "png",
#   plot = barplot,
#   filename = "figures/IUCN_v_arid.png",
#   height = 4,
#   width = 9,
#   dpi = 300
# )

## first attempt at PCA ----
# modeled after this: https://repositories.lib.utexas.edu/bitstream/handle/2152/94746/Perkin%20et%20al%202021.pdf?sequence=3
library(tree)
library(rpart)
library(rpart.plot)

# first filter data based on columns of interest
names(allarid)

aridCARTdat <- select(allarid, 
                      c("GenusSpecies",
                      "habitat",
                      "location",
                      "endemic",
                      "AUSnative",
                      "USAnative",
                      "nonfeed",
                      "algphyto",
                      "macvascu",
                      "detritus",
                      "invlvfsh",
                      "fshcrcrb",
                      "blood",
                      "eggs",
                      "maxtl_cm",
                      "guarder",
                      "open.substratum.spawner",
                      "broodhider",
                      "livebearers",
                      "sprgsubt",
                      "lacustrine",
                      "potanadr",
                      "slowcurr",
                      "modcurr",
                      "fastcurr",
                      "IUCNstatus",
                      "FoodTroph.x"))
str(aridCARTdat)

# make many of these columns factors instead
cols <- c("location", "endemic", "AUSnative", "USAnative",
          "algphyto", "macvascu", "detritus", "invlvfsh",
          "fshcrcrb", "blood", "eggs", "guarder", "open.substratum.spawner",
          "broodhider", "livebearers", "sprgsubt", "lacustrine",
          "potanadr", "slowcurr", "modcurr", "fastcurr")

aridCARTdat %<>%
  mutate_each_(funs(as.factor(.)),cols)
# aridCARTdat %<>%
#   mutate_each_(funs(as.logical(.)),cols)
str(aridCARTdat)
summary(aridCARTdat) # all looks good

# ## to make CART ready elimate NAs
aridCARTdat <- na.omit(aridCARTdat)

## melt dataframe for food, current, and spawning
# made a clunky ifelse statement because that's how I am
aridCARTdat$spawning <- (ifelse(aridCARTdat$open.substratum.spawner == TRUE, "open.substratum",
                               ifelse(aridCARTdat$broodhider == TRUE, "broodhider",
                                      ifelse(aridCARTdat$livebearers == TRUE, "livebearers",
                                             ifelse(aridCARTdat$guarder == TRUE, "guarder",
                                                    NA)))))
aridCARTdat$spawning <- as.factor(aridCARTdat$spawning)
                    


## make a new column for species in trouble
setDT(aridCARTdat)
aridCARTdat$dangerfish <-
  aridCARTdat[, dangerfish := ifelse(IUCNstatus %in%
                                       c("Endangered", 
                                         "Extinct",
                                         "Critically Endangered",
                                         "Extinct in the Wild",
                                         "Near Threatened",
                                         "Vulnerable"), 1, 0)]

# aridCARTdat$dangerfish <-
#   ifelse(
#     aridCARTdat$IUCNstatus == "Endangered" |
#       aridCARTdat$IUCNstatus == "Extinct" |
#       aridCARTdat$IUCNstatus == "Critically Endangered" |
#       aridCARTdat$IUCNstatus == "Extinct in the Wild" |
#       aridCARTdat$IUCNstatus == "Near Threatened" |
#       aridCARTdat$IUCNstatus == "Vulnerable",
#     1,
#     0
#   )


## build a baby model
TheModel <- rpart(dangerfish ~
                   endemic +
                  AUSnative +
                  USAnative +
                  maxtl_cm +
                  spawning + 
                  # slowcurr +
                  # modcurr +
                  # fastcurr +
                  # algphyto +
                  # macvascu +
                  # detritus + 
                  # invlvfsh +
                  # fshcrcrb +
                  FoodTroph.x,
                 dat = aridCARTdat
)
prp(TheModel, yesno = 2, type = 2, digits = 4, extra = 1)
# plot(TheModel)
# text(TheModel, digits = 3)
print(TheModel, digits = 2)

# do we need to prune?
plotcp(TheModel) 

# ggsave(CARTplot, filename = "figures/CARTmodel.png", dpi = 300, height = 7, width = 7)


