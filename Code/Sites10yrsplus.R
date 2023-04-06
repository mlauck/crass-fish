# Sites with 10+ years
# Script by FER
# Last update April 2023

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

pres <- tenfish %>%
  group_by(hexID, species, year) %>%
  summarize(count = n())

# species matrix
presmat <- pivot_wider(pres,
                       names_from = species,
                       values_from = count,
                       values_fill = 0)



# long10 <- rich %>% group_by(hexID) %>% summarize(count = length(unique(year)))
# long10filt <- filter(long10, count >= 10)

# create list to filter larger dataset
filterID <- list(long10filt$hexID)

# filter only sites with 10+ years of data
library(data.table)
setDT(allfish)
tenfish_filt <- allfish[filterID, on = "hexID"]

# Corey's method
hex.list <- split(allfish, f = allfish$hexID)
hex.years <-
  allfish[!duplicated(allfish[, c("year", "hexID")]), ] #1379 hexes
years.per.hex <-
  table(hex.years$hexID) #mean of 4.38 years per hex, 3 is median
hex.5yr <- years.per.hex[years.per.hex > 4] #5 or more sample years 399
hex.10yr <-
  years.per.hex[years.per.hex > 9] #10 or more sample years 147
names.10yr <- names(hex.10yr)
hex.20yr <-
  years.per.hex[years.per.hex > 19] #20 or more sample years 45
names.20yr <- names(hex.20yr)

# richness by year
# species richness matrix
rich <- tenfish_filt %>% 
  group_by(hexID, year) %>% 
  # distinct() %>%
  summarize(richness = length(unique(species)))

# rich2 <- rich %>% pivot_wider(
#   names_from = year,
#   values_from = richness)

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
  geom_point(pch = 21, size = 2) +
  scale_fill_viridis_d() +
  theme_bw(base_size = 14) +
  xlab("Year") +
  ylab("Fish species richness") +
  geom_smooth(method = "lm")
print(richplot)
ggsave(richplot, filename = "figures/lm10yr.png", dpi = 300, width = 6, height = 4)

richmod <- lmer(richness ~ year + (1|hexID), data = rich)
print(richmod)
summary(richmod)
