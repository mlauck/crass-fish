# Sites with 10+ years
# Script by FER
# Last update May 2023

# libraries
library(tidyr)
library(stringr)
library(ggplot2)
library(ggpubr)
library(viridis)
library(lme4)
library(lattice)
library(tidyverse)


# load data
allfish <- read.csv("Data/fish_by_hexbin_all.csv", header = TRUE, fileEncoding="UTF-8-BOM")
head(allfish)

# site as factor
allfish$hexID <- as.factor(allfish$hexID)
allfish$species <- as.factor(allfish$species)

# group by hexID
by_hex <- allfish %>% group_by(hexID)

# Get names of hexIDs with long-term data
# Corey's method
hex.list <- split(allfish, f = allfish$hexID)
hex.years <-
  allfish[!duplicated(allfish[, c("year", "hexID")]), ] #1379 hexes
years.per.hex <-
  table(hex.years$hexID) #mean of 4.38 years per hex, 3 is median
hex.3yr <- years.per.hex[years.per.hex > 2] #3 or more sample years 701
names.3yr <- names(hex.3yr)

hex.5yr <- years.per.hex[years.per.hex > 4] #5 or more sample years 399
names.5yr <- names(hex.5yr)

hex.10yr <-
  years.per.hex[years.per.hex > 9] #10 or more sample years 147
names.10yr <- names(hex.10yr)
hex.20yr <-
  years.per.hex[years.per.hex > 19] #20 or more sample years 45
names.20yr <- names(hex.20yr)

# create list to filter larger dataset
filterID <- list(names.10yr)
filterID5 <- list(names.5yr)
filterID3 <- list(names.3yr)

# filter only sites with 10+ years of data
library(data.table)
setDT(allfish)
tenfish_filt <- allfish[filterID, on = "hexID"]
fivefish_filt <- allfish[filterID5, on = "hexID"]
threefish_filt <- allfish[filterID3, on = "hexID"]

pres <- tenfish_filt %>%
  group_by(hexID, species, year) %>%
  summarize(count = n())

# species matrix
presmat <- pivot_wider(pres,
                       names_from = species,
                       values_from = count,
                       values_fill = 0)



# richness by year
# species richness matrix
rich <- tenfish_filt %>% 
  group_by(hexID, year) %>% 
  # distinct() %>%
  summarize(richness = length(unique(species)))

# rich2 <- rich %>% pivot_wider(
#   names_from = year,
#   values_from = richness)


# long10 <- rich %>% group_by(hexID) %>% summarize(count = length(unique(year)))
# long10filt <- filter(long10, count >= 10)





# model

require(lme4)                            ## for lmer()
require(lattice)                         ## for dotplot()

# function for ggCaterpillar in ggplot2
## re = object of class ranef.mer
ggCaterpillar <- function(re, QQ=TRUE, likeDotplot=TRUE) {
  require(ggplot2)
  f <- function(x) {
    pv   <- attr(x, "postVar")
    cols <- 1:(dim(pv)[1])
    se   <- unlist(lapply(cols, function(i) sqrt(pv[i, i, ])))
    ord  <- unlist(lapply(x, order)) + rep((0:(ncol(x) - 1)) * nrow(x), each=nrow(x))
    pDf  <- data.frame(y=unlist(x)[ord],
                       ci=1.96*se[ord],
                       nQQ=rep(qnorm(ppoints(nrow(x))), ncol(x)),
                       ID=factor(rep(rownames(x), ncol(x))[ord], levels=rownames(x)[ord]),
                       ind=gl(ncol(x), nrow(x), labels=names(x)))
    
    if(QQ) {  ## normal QQ-plot
      p <- ggplot(pDf, aes(nQQ, y))
      p <- p + facet_wrap(~ ind, scales="free")
      p <- p + xlab("Standard normal quantiles") + ylab("Random effect quantiles")
    } else {  ## caterpillar dotplot
      p <- ggplot(pDf, aes(ID, y)) + coord_flip()
      if(likeDotplot) {  ## imitate dotplot() -> same scales for random effects
        p <- p + facet_wrap(~ ind)
      } else {           ## different scales for random effects
        p <- p + facet_grid(ind ~ ., scales="free_y")
      }
      p <- p + xlab("Levels") + ylab("Random effects")
    }
    
    p <- p + theme(legend.position="none")
    p <- p + geom_hline(yintercept=0)
    p <- p + geom_errorbar(aes(ymin=y-ci, ymax=y+ci), width=0, colour="black")
    p <- p + geom_point(pch = 21, aes(size=0.5), fill="lightblue")
    return(p)
  }
  
  lapply(re, f)
}


fit <- lmer(richness ~ year + (scale(year)|hexID), rich)
ggCaterpillar(ranef(fit, condVar=TRUE), likeDotplot= TRUE)  ## using ggplot2
ggsave(filename = "figures/10yrsiteslopes.png", dpi = 600, height = 8, width = 12)
qqmath(ranef(fit, condVar=TRUE))         ## for comparison
dotfig<- dotplot(ranef(fit, condVar=TRUE))

# another way to do it, extract simulated fits and plot
# can adjust confidence inverval using "level" parameter in plotREsim
library(merTools)
randoms <- REsim(fit, n.sims = 500)
plotREsim(randoms)
ggsave(filename = "figures/10yrrandoms.png", dpi = 600, width = 8, height = 6)

# plot up richness
richplot <- ggplot(aes(x = year, y = richness, fill = hexID), data = rich) +
  scale_fill_viridis_d() +
  theme_bw(base_size = 14) +
  xlab("Year") +
  ylab("Fish species richness") +
  geom_smooth(method = "lm") +
  # geom_point(pch = 21, size = 2) +
  theme(legend.position="none")
print(richplot)
ggsave(richplot, filename = "figures/lm10yr.png", dpi = 300, width = 8, height = 4)

richmod <- lmer(richness ~ year + (1|hexID), data = rich)
print(richmod)
summary(richmod)


## calculate Jaccard index
library(vegan)
presmat

## keep only the first and last years
library(dplyr)
library(tibble)

firstlast <- presmat %>%
  as.data.frame()%>%
  # rownames_to_column(var = "hexID")%>%
  group_by(hexID)%>%
  filter(year == min(year)|year == max(year))%>%
  ungroup()

# turn year into a factor so it doesn't get mutated
firstlast$year <- as.factor(firstlast$year)

# replace anything > 1 with 1
firstlast2 <- firstlast %>%
  mutate_if(is.numeric, ~1 * (. > 0))

# # turn year into a factor so it doesn't get mutated
# presmat$year <- as.factor(presmat$year)
# 
# # replace anything > 1 with 1
# presmat2 <- presmat %>%
#   mutate_if(is.numeric, ~1 * (. > 0))

# turn year back to numeric
firstlast2$year <- as.numeric(as.character(firstlast2$year))
str(firstlast2) # numeric changes year into numbers but not years, 1 -62

# make a new column name
firstlast2$hexyear <- paste("hex",firstlast2$hexID, firstlast2$year,  sep= "-")

# set hexID column to rownames
firstlast2 <- firstlast2 %>% remove_rownames %>% column_to_rownames(var="hexyear")
firstlast2 <- firstlast2[,-1]

View(firstlast2)

vare.dist <- vegdist(method = "jaccard", firstlast2)

# bray <- vegdist(firstlast2, dmethod = "bray") %>%
#   as.matrix() %>%
#   as_tibble(rownames = "A") %>%
#   pivot_longer(-A, names_to = "B", values_to= "distances")
# 
# bray %>%
#   ggplot(aes(x = A, y = B, fill = distances)) +
#   geom_tile()

jaccard <- vegdist(firstlast2, method = "jaccard", binary = TRUE) %>%
  as.matrix() %>%
  as_tibble(rownames = "A") %>%
  pivot_longer(-A, names_to = "B", values_to= "distances")

summary(jaccard)

## filter the data 

# extract first word of the column in R
jaccard$hexID1 <- str_sub(jaccard$A,1,8) 
jaccard$hexID1

jaccard$hexID2 <- str_sub(jaccard$B,1,8) 
jaccard$hexID2

#filter for rows where hexID matches but is not same year
jaccard2 <- jaccard %>%
  filter(hexID1 == hexID2) %>%
  subset(A != B)

# only keep odd numbered rows
jaccard3 <- jaccard2 %>% filter(row_number() %% 2 == 1) ## Select odd rows

# extract years - some of these are messed up and out of order
jaccard3$year1 <- as.numeric(as.character(str_sub(jaccard3$A,-4)))
jaccard3$year2 <- as.numeric(as.character(str_sub(jaccard3$B,-4))) 
jaccard3$yeardiff <- abs(jaccard3$year2 - jaccard3$year1)

# correct order
plot(jaccard3$year1, jaccard3$yeardiff)
plot(jaccard3$year2, jaccard3$yeardiff)
plot(jaccard3$year2, jaccard3$year1)
jaccard3$year1new <- ifelse(jaccard3$year1>jaccard3$year2, jaccard3$year2, jaccard3$year1)
jaccard3$year2new <- ifelse(jaccard3$year2>jaccard3$year1, jaccard3$year2, jaccard3$year1)

plot(jaccard3$year1new, jaccard3$distances)

# make it so hexIDs are numbers again
jaccard3$hexID <- as.integer(str_extract(jaccard3$hexID1, "[0-9]+"))

# join with richness data and plot jaccard vs. richness?
rich$hexID <- as.numeric(rich$hexID)
richmat <- rich %>%
  as.data.frame()%>%
  # rownames_to_column(var = "hexID")%>%
  group_by(hexID)%>%
  filter(year == min(year))%>%
  ungroup()

jaccard_test <- left_join(jaccard3, richmat, by = "hexID")

write.csv(jaccard_test, "data/jaccardmatrix10yr.csv")

plot(jaccard_test$distances, jaccard_test$richness)

jaccardtime <- ggplot(aes(x = year1new, y = distances, group = hexID1), data = jaccard3) +
  # geom_point(size = 3, pch = 21, aes(fill = distances)) +
  geom_point(size = 3, pch = 21, aes(fill = year2new)) +
  theme_bw(base_size = 14) +
  xlab("Years between first and last survey") +
  ylab("Jaccard distance") +
  scale_fill_viridis(option = "magma", name = "Last survey year") +
  geom_smooth(color = "darkgray")
print(jaccardtime)
ggsave(jaccardtime, filename = "figures/jaccard_v_timediff.png", dpi = 300, width = 5, height = 4.5)

# first year vs. last year with size how different they are
jaccardtime2 <- ggplot(aes(x = year1new, y = year2new), data = jaccard3) +
  # geom_point(size = 3, pch = 21, aes(fill = distances)) +
  geom_point(pch = 21, aes(fill = distances, size = distances)) +
  theme_bw(base_size = 14) +
  xlab("First year of survey") +
  ylab("Last year of survey") +
  scale_fill_viridis(option = "magma", name = "Jaccard index") +
  geom_smooth(color = "darkgray")
print(jaccardtime2)

mod1 <- lm(distances ~ yeardiff, data = jaccard3)
summary(mod1)

mod2 <- lm(distances ~ scale(yeardiff) + scale(year1new), data = jaccard3)
summary(mod2)
plot(mod2)

# plot as connected lines
## duration[, 4] = dummy
## duration[, 2] = start year
## duration[, 3] = end year

library(forcats)


jaccard4 <- jaccard3 %>% mutate(hexID1 = fct_reorder(hexID1, as.numeric(distances)))
jaccard5 <- jaccard3 %>% mutate(hexID1 = fct_reorder(hexID1, as.numeric(year1new)))
jaccard6 <- jaccard3 %>% mutate(hexID1 = fct_reorder(hexID1, as.numeric(year2new)))
jaccard7 <- jaccard_test %>% mutate(hexID1 = fct_reorder(hexID1, as.numeric(richness)))
# jaccard4$yaxis <- seq(1:144)

# plot(
#   1,
#   type = 'n',
#   xlab = "Date Range of Study",
#   ylab = "Jaccard index",
#   yaxt = 'n',
#   xlim = c(1955, 2020),
#   ylim = c(0, 1),
#   main = "Study time frame",
#   cex.lab = 2,
#   cex.axis = 1.25
# )
# 
# segments_gradient(
#   x,
#   y = NULL,
#   col = colorRamp2(c("transparent", "black"), TRUE),
#   lend = 1,
#   ...
# )

# segments(duration[, 2], duration[, 4], duration[, 3], duration[, 4], lwd =
#            2)



segplot <- ggplot(data = jaccard7, aes(y = reorder(hexID1, richness))) +
  geom_segment(aes(
    x = year1new,
    y = hexID1,
    xend = year2new,
    yend = hexID1,
    colour = distances
  ), linewidth = 1.05
  ) +
  scale_color_viridis(option = "viridis", name = "Jaccard index") +
  ylab("HexID (ordered by richness in first year of survey)") +
  xlab("Date range of survey") +
  theme_classic(base_size = 14) +
  theme(axis.text.y=element_blank(),
        axis.ticks.y.left = element_blank()) 
print(segplot)
ggsave(segplot, filename = "figures/segmentplot4.png", dpi = 300, width = 6, height = 6)



jaccardtime2 <- ggplot(aes(x = year1, y = distances), data = jaccard3) +
  geom_point(size = 3, pch = 21, aes(fill = distances)) +
  geom_point(size = 3, pch = 21, aes(x = year2, fill = distances)) +
  geom_line() +
  theme_bw(base_size = 14) +
  xlab("Year of survey (first or last)") +
  ylab("Jaccard distance") +
  scale_fill_viridis(option = "magma", name = "Jaccard") 
print(jaccardtime2)


### bin the time periods
jaccard4$bin <- ifelse(year1new<1970, "<1970",
                       ifelse(1970 < year1new <= 1980), "1971-1980",
                       ifelse(year1new > 1980 & year1new <= 1990), "1981-1990",
                       ifelse(year1new > 1990 & year1new < 2000), "1991-2000", "2001 or later")

jaccard4$bin <- cut(jaccard4$year1new, breaks = c(-Inf, 1970, 1980, 1990, 2000, Inf), 
                    labels = c("<1970", "1971-1980", "1981-1990", 
                              "1991-2000", "2001 or later"))

ggplot(aes(x = bin, y = distances), data = jaccard4) +
  geom_violin() + 
  geom_point(pch = 21, aes(fill = distances))

# plot the data
jac <- jaccard %>%
  ggplot(aes(x = A, y = B, fill = distances)) +
  geom_tile() +
  ggtitle("Jaccard dissimilarity") +
  scale_fill_gradient(low = "#FF0000", high = "#FFFFFF") +
  labs(x = "hexID", y = "hexID") +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1))
print(jac)
# ggsave(jac, filename = "figures/jaccard95.png", dpi = 300, height = 8, width = 10)

# ## 1995 data ----
# # filter to only sites with data in 1995
# presmat95 <- filter(presmat2, year == 1995)
# 
# # set hexID column to rownames
# presmat95 <- presmat95 %>% remove_rownames %>% column_to_rownames(var="hexID")
# presmat95 <- presmat95[,-1]
# 
# View(presmat95)
# 
# vare.dist <- vegdist(method = "jaccard", presmat95)
# 
# bray <- vegdist(presmat95, dmethod = "bray") %>%
#   as.matrix() %>%
#   as_tibble(rownames = "A") %>%
#   pivot_longer(-A, names_to = "B", values_to= "distances")
# 
# bray %>%
#   ggplot(aes(x = A, y = B, fill = distances)) +
#   geom_tile()
# 
# jaccard95 <- vegdist(presmat95, method = "jaccard", binary = TRUE) %>%
#   as.matrix() %>%
#   as_tibble(rownames = "A") %>%
#   pivot_longer(-A, names_to = "B", values_to= "distances")
# 
# summary(jaccard95)
# 
# jac95 <- jaccard95 %>%
#   ggplot(aes(x = A, y = B, fill = distances)) +
#   geom_tile() +
#   ggtitle("Jaccard dissimilarity 1995") +
#   scale_fill_gradient(low = "#FF0000", high = "#FFFFFF") +
#   labs(x = "hexID", y = "hexID") +
#   theme(
#     axis.text.x = element_text(
#       angle = 45,
#       hjust = 1))
# ggsave(jac95, filename = "figures/jaccard95.png", dpi = 300, height = 8, width = 10)




## 2005 data ----
# filter to only sites with data in 1995
presmat05 <- filter(presmat2, year == 2005)

# set hexID column to rownames
presmat05 <- presmat05 %>% remove_rownames %>% column_to_rownames(var="hexID")
presmat05 <- presmat05[,-1]

View(presmat05)

# vare.dist <- vegdist(method = "jaccard", presmat95)
# 
# bray <- vegdist(presmat95, dmethod = "bray") %>%
#   as.matrix() %>%
#   as_tibble(rownames = "A") %>%
#   pivot_longer(-A, names_to = "B", values_to= "distances")
# 
# bray %>%
#   ggplot(aes(x = A, y = B, fill = distances)) +
#   geom_tile()

jaccard05 <- vegdist(presmat05, method = "jaccard", binary = TRUE) %>%
  as.matrix() %>%
  as_tibble(rownames = "A") %>%
  pivot_longer(-A, names_to = "B", values_to= "distances")

summary(jaccard05)

# join9505 <- inner_join(jaccard95, jaccard05, by = c("A", "B"))
# 
# join9505filt <- filter(join9505, A != B)
# 
# #### how did the community shift from 1995 to 2005
# ## plot of pairwise shifts
# shift <- join9505filt %>%
#   ggplot(aes(x = distances.x, y = distances.y)) +
#   geom_point(pch = 21, fill = "grey90", alpha = 0.5, size = 2) +
#   theme_bw(base_size = 14) +
#   xlab("Pairwise Jaccard distances 1995") +
#   ylab("Pairwise Jaccard distances 2005") +
#   geom_abline(slope = 1, intercept = 0, size = 1, linetype = "dashed", color = "black") +
#   geom_smooth(method = "lm")
# print(shift)
# ggsave(shift, filename = "figures/jacshift9505.png", dpi = 300, height = 5, width = 5)
# 
# # histogram of pairwise shifts
# ggplot(join9505, aes(distances.x)) + 
#   geom_density(alpha = 0.2, fill = "pink") +
#   xlab("Jaccard dissimilarity index") +
#   geom_density(aes(distances.y), alpha = 0.2, fill = "purple") +
#   theme_bw()


# lmer model
shift <- lmer(distances.y ~ distances.x + (1|A), 
               family = gaussian(link = "inverse"), 
               data = join9505filt)
summary(shift)
plot(shift)


jac05 <- jaccard05 %>%
  ggplot(aes(x = A, y = B, fill = distances)) +
  geom_tile() +
  ggtitle("Jaccard dissimilarity 2005") +
  scale_fill_gradient(low = "#FF0000", high = "#FFFFFF") +
  labs(x = "hexID", y = "hexID") +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1))
ggsave(jac05, filename = "figures/jaccard05.png", dpi = 300, height = 8, width = 10)


## 2015 data ----
# filter to only sites with data in 2015
presmat15 <- filter(presmat2, year == 2015)

# set hexID column to rownames
presmat15 <- presmat15 %>% remove_rownames %>% column_to_rownames(var="hexID")
presmat15 <- presmat15[,-1]

View(presmat15)

vare.dist <- vegdist(method = "jaccard", presmat15)

bray <- vegdist(presmat15, dmethod = "bray") %>%
  as.matrix() %>%
  as_tibble(rownames = "A") %>%
  pivot_longer(-A, names_to = "B", values_to= "distances")

bray %>%
  ggplot(aes(x = A, y = B, fill = distances)) +
  geom_tile()

jaccard15 <- vegdist(presmat15, method = "jaccard", binary = TRUE) %>%
  as.matrix() %>%
  as_tibble(rownames = "A") %>%
  pivot_longer(-A, names_to = "B", values_to= "distances")
hist(jaccard15$distances)
hist(jaccard95$distances)
summary(jaccard15)

jac15 <- jaccard15 %>%
  ggplot(aes(x = A, y = B, fill = distances)) +
  geom_tile() +
  ggtitle("Jaccard dissimilarity 2015") +
  scale_fill_gradient(low = "#FF0000", high = "#FFFFFF") +
  labs(x = "hexID", y = "hexID") +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1))
ggsave(jac15, filename = "figures/jaccard15.png", dpi = 300, height = 8, width = 10)

