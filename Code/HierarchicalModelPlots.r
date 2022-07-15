# Figures for MS1
# December 2018
# FER


## load data ----
test <- read.csv("H:/Erie projects/Eriev2.csv", header=T)
maum <- read.csv("H:/Erie projects/Maumee data/Maumee_Raw_v2.csv", header=T)
daily <- read.csv("H:/Erie projects/Maumee data/master_dailyFWM&TWM_Maumee.csv", header=T)
monthly <- read.csv("H:/Erie projects/Maumee data/master_monthlyFWM&TWM_Maumee.csv", header=T)


# libraries ----
library(ggplot2)
library(scales)
library(tidyr)
library(dplyr)
library(readr)
library(gridExtra)
library(grid)
library(colorspace)
library(lattice)


## TP vs. Chla ----
# Bayesian model for this
library(lme4)
library(brms)
library(Rcpp)

# install.packages("rstanarm", repos = c("http://rstudio.org/_packages", "http://cran.rstudio.com"))
library(tidyselect)
library(MASS)
library(rstanarm)
library(bayesplot)
library(ggplot2)
library(tidyr)
library(tibble)
library(lattice)
library(extrafont)



# data frame
test2 <- test[,c(2,5:7,15,29)]
dim(test2)
head(test2)
test3 <- na.omit(test2)




# trellis plot
# a nice way to look at likely estimates but in frequentist frameworks

# trellis of chla vs. TP by month
xyplot(logChl ~ logTP|as.factor(Month), data = test, panel=function(x,y,...){
  panel.xyplot(x, y, ...)
  panel.lmline(x,y,...)}, ylab="ln(Chl a)", xlab="ln(TP)")

# trellis of chla vs. TSS by station
xyplot(logChl ~ logTSS|as.factor(Station2), data = test, panel=function(x,y,...){
  panel.xyplot(x, y, ...)
  panel.lmline(x,y,...)}, ylab="ln(Chl a)", xlab="ln(TSS)")




## Here is where the Bayesian models start
# Bayesian model TP vs. Chla ----
TPmod <- stan_glmer(
  logChl ~ logTP + (logTP|Station2),
  family = gaussian(),
  data = test
)

# summary
summary(TPmod, digits = 3, pars = "Intercept")



# get coefficients ----
coefs <- coef(TPmod)$Station2 %>%
  rownames_to_column("Station2")

names(coefs) <- c("Station2", "Intercept", "Slope")

posterior_interval(TPmod, regex_pars = "Intercept")

test4 <- left_join(test, coefs, by="Station2")

# Get a dataframe: One row per posterior sample ----
df_posterior <- TPmod %>% 
  as.data.frame() %>% 
  as_tibble()

# graph all effects
ggplot(df_posterior) + 
  aes(x = `(Intercept)`, y = `logTP`) + 
  # Calculate the density
  stat_density_2d(aes(fill = ..level..), geom = "polygon") +
  ggtitle("Where's the average intercept and slope?") + 
  xlab("Estimate for average intercept") + 
  ylab("Estimate for average slope") +
  # Use the same coordinate limits as last plot
  guides(fill = "none")



# set theme ----
theme_coding <- function(){
  theme_bw()+
    theme(axis.text.x=element_text(size=16, vjust=1, hjust=1),
          axis.text.y=element_text(size=16),
          axis.title.x=element_text(size=16, face="plain"),
          axis.title.y=element_text(size=16, face="plain"),
          panel.grid.major.x=element_blank(),
          panel.grid.minor.x=element_blank(),
          panel.grid.minor.y=element_blank(),
          panel.grid.major.y=element_blank(),
          strip.text = element_text(size = 16), # change size of titles in facet wrap
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
          plot.title = element_text(size = 16, vjust = 1, hjust = 0.5),
          legend.text = element_text(size = 16),
          legend.title = element_blank(),
          legend.position = "bottom")
}

# overall plot ----
overall <- ggplot(test4) +
  plot(logChl ~ logTP, data = test) +
  aes(x = logTP, y = logChl) +
  #geom_abline(aes(intercept = Intercept, slope = Slope_logTP), 
  #data = df_samples, color = "#BDBDBD", alpha = .1) +
  geom_abline(aes(intercept = Intercept, slope = Slope), data = test4, color = "grey80", lwd = 0.75) + 
  geom_abline(aes(slope = 0.8, intercept = -0.2), alpha = 1, size = 1) +
  geom_point(aes(colour = as.factor(Year)), size = 2) +
  scale_colour_brewer(palette = "BrBG") +
  #facet_wrap("Station2") + 
  theme(axis.ticks.length = unit(.25, "cm")) +
  theme(text=element_text(family = "sans", size = 20)) +
  #theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  xlab(expression(paste("ln(TP) (", mu,g," ", L^-1,")"))) +
  ylab(expression(paste("ln(Chl ", italic("a"),") (", mu,g," ", L^-1,")"))) +
  theme(legend.position = "right") +
  theme(axis.text.x=element_text(size = 20, vjust = 1, hjust = 1),
        axis.text.y=element_text(size = 20),
        axis.title.x=element_text(size = 20, face="plain"),
        axis.title.y=element_text(size = 20, face="plain")) +
  guides(color=guide_legend(title="Year"), size = 20)

overall + theme(panel.border = element_rect(linetype = "dashed", fill = NA)) 

ggsave(filename = "Overall_ChlTP.png", overall, width = 5, height = 5)



# For each sample, add the average intercept and average slope values to each
# stations's deviation from that average. These yields the intercept and
# slope parameters for each participant.
df_effects <- df_posterior %>%
  # Find all the columns with the pattern "b[(Intercept". Add the column
  # df_posterior$`(Intercept)` to each of those columns.
  mutate_at(
    .vars = vars(matches("b\\[\\(Intercept")), 
    .funs = funs(. + df_posterior$`(Intercept)`)) %>%
  # Again for slope
  mutate_at(
    .vars = vars(matches("b\\[logTP")), 
    .funs = funs(. + df_posterior$logTP))

# Convert to a long format

# use this one
df_long_effects <- df_effects %>% 
  dplyr::select(starts_with("b[")) %>% 
  rownames_to_column("draw") %>%
  tidyr::gather(Parameter, Value, -draw)

# Extract the effect type and subject number from each parameter name
df_long_effects$Type <- df_long_effects$Parameter %>%
  stringr::str_detect("Intercept") %>%
  ifelse(., "Intercept", "Slope_logTP")


df_long_effects$Station2 <- df_long_effects$Parameter %>%
  stringr::str_extract(c("WE\\d+|other"))


df_long_effects <- df_long_effects %>% 
  dplyr::select(draw, Station2, Effect = Type, Value)

# choose 50 posterior samples
df_samples <- df_long_effects %>%
  filter(draw %in% sample(1:4000, size = 50)) %>%
  tidyr::spread(Effect, Value)
df_samples

# plot up slope and intercept estimates
ggplot(df_long_effects %>% tidyr::spread(Effect, Value)) + 
  aes(x = Intercept, y = Slope_logTP) + 
  stat_density_2d(aes(fill = ..level..), geom = "polygon") +
  facet_wrap("Station2") + 
  xlab("Intercept estimate") + 
  ylab("Slope estimate") +
  theme(legend.position = "bottom") +
  guides(fill = "none")

ggplot(df_long_effects %>% tidyr::spread(Effect, Value)) + 
  aes(x = Intercept, y = Slope_logTP) + 
  stat_density_2d(aes(fill = ..level..), geom = "polygon") +
  facet_wrap("Station2") + 
  xlab("Intercept estimate") + 
  ylab("Slope estimate") +
  theme(legend.position = "bottom") +
  guides(fill = "none")

# Figure 3 final
# plot all stations with posterior draws
Fig3 <- ggplot(test4) +
  aes(x = logTP, y = logChl) +
  geom_abline(aes(intercept = Intercept, slope = Slope_logTP), 
              data = df_samples, color = "dark gray", alpha = .2) +
  geom_abline(aes(intercept = Intercept, slope = Slope), data = test4) + 
  geom_point(aes(colour = as.factor(Year)), size = 1) +
  scale_colour_brewer(palette = "BrBG") +
  theme_coding() + 
  facet_wrap("Station2", ncol = 6) + 
  #xlab(expression(paste("ln(TP) (", mu,g," ", L^-1,")"))) +
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank()) +
  #ylab(expression(paste("ln(Chl ", italic("a"),") (", mu,g," ", L^-1,")"))) +
  guides(color = guide_legend(title = "Year")) + # to make three columns, ncol = 3)) + 
  theme(legend.position = "none") # to move to bottom: c(.75,.1)) +

ggsave(filename = "ChlaTP.png", Fig3, height = 12, width = 10)

library(cowplot)
plot.inset <- ggdraw() + draw_plot(Fig3) + draw_plot(overall, x = 0.55, y = 0.05, height = 0.23, width = 0.23)

Fig3new <- ggdraw() + draw_plot(Fig3, x = 0.12, y = 0.5, height = 0.5, width = 0.7) + draw_plot(overall)
ggsave(filename = "ChlaTP_new.png", Fig3new, height = 10, width = 10)
