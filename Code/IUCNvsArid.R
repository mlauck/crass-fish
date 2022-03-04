# Comparing IUCN lists of all fishes vs. arid fishes
# FER
# Last edit 4 March 2022

## next week to do:
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

## reorder data for plotting and ordinal regression
alldata$IUCNstatus <- factor(
  alldata$IUCNstatus,
  levels = c(
    "Extinct",
    "Extinct in the Wild",
    "Critically Endangered",
    "Endangered",
    "Vulnerable",
    "Near Threatened",
    "Least Concern",
    "Data Deficient",
    "Not Evaluated",
    ordered = TRUE
  )
)

alldata <- alldata[order(alldata$IUCNstatus), ]

# make sure factors are reading as such
alldata$IUCNstatus <- as.factor(alldata$IUCNstatus)
alldata$source <- as.factor(alldata$source)

# remove line for not evaluated since it is not present in larger data
alldata2 <- alldata[alldata$IUCNstatus != "Not Evaluated", ]

## grouped barplot -----
barplot <- ggplot(alldata2, aes(x = IUCN, y = prop, group = source)) +
  #geom_bar(stat = "identity") +
  geom_col(
    aes(
      x = reorder(str_wrap(IUCNstatus, 5), n),
      y = prop,
      fill = source
    ),
    position = "dodge2",
    show.legend = TRUE,
    alpha = .9
  ) +
  ylab("proportion of all fishes") +
  xlab("IUCN status") +
  scale_fill_viridis_d(name = "Source",
                       begin = 0.5,
                       end = 0.1) +
#scale_fill_viridis_d() +
  theme_bw(base_size = 14)
print(barplot)

ggsave(
  device = "png",
  plot = barplot,
  filename = "figures/IUCN_v_arid.png",
  height = 4,
  width = 9,
  dpi = 300
)

## first attempt at ordinal regression ----
library(MASS)

mod1 <- polr(IUCNstatus ~ source + prop, data = alldata2, Hess=TRUE)
summary(mod1)

##             apply pared public  gpa
## 1     very likely     0      0 3.26
## 2 somewhat likely     1      0 3.21
## 3        unlikely     1      1 3.94
## 4 somewhat likely     0      0 2.81
## 5 somewhat likely     0      0 2.53
## 6        unlikely     0      1 2.59

# polr(formula = apply ~ pared + public + gpa, data = dat, Hess = TRUE)

## calculate table
(ctable <- coef(summary(mod1)))

## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

## combined table
(ctable <- cbind(ctable, "p value" = p))

# compute confusion table and misclassiciation error
#Compute confusion table and misclassification error
predictrpurchase = predict(model,datatest)
table(datatest$rpurchase, predictrpurchase)
mean(as.character(datatest$rpurchase) != as.character(predictrpurchase))
