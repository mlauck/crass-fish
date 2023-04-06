# 30 year data
# script by FER
# Last update February 2023

# load data
longfish <- read.csv("Data/fish_data_30yr_site_subset.csv", header = TRUE, fileEncoding="UTF-8-BOM")
head(longfish)

# site as factor
longfish$hexID <- as.factor(longfish$hexID)
longfish$species <- as.factor(longfish$species)


# libraries
# library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(ggpubr)
library(viridis)
#library(ggridges)
library(lme4)
#library(brms)
#library(magrittr)
#library(rstan)
#library(tidybayes)
#library(emmeans)
#library(broom)
#library(modelr)
#library(forcats)
library(RColorBrewer)
library(lattice)

summary(longfish)

barcodeplot <- ggplot(data = longfish, aes(x = year, y = species, group = hexID)) +
  scale_fill_viridis_c(option = "magma", name = "year") +
  geom_col(aes(fill = year)) +
  theme_bw(base_size = 8) +
  xlab("Number of times detected") +
  ylab("Species") +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  ggtitle("Species presence in data") +
  facet_wrap(~hexID)
print(barcodeplot)

# unique species by site and year
library(tidyverse)
rich <- longfish %>% 
    group_by(hexID, year) %>% 
    distinct() %>%
    summarize(richness = length(unique(species)))
rich2 <- rich %>% pivot_wider(
  names_from = year,
  values_from = richness)


pres <- longfish %>%
  group_by(hexID, year, species) %>%
  summarize(count = n())

# species matrix
presmat <- pivot_wider(pres,
                       names_from = species,
                       values_from = count,
                       values_fill = 0)

# plot up richness
richplot <- ggplot(aes(x = year, y = richness, fill = hexID), data = rich) +
  geom_point(pch = 21, size = 2) +
  scale_fill_viridis_d() +
  theme_bw(base_size = 14) +
  xlab("Year") +
  ylab("Fish species richness") +
  geom_smooth(method = "lm")
print(richplot)
ggsave(richplot, filename = "figures/lm30yr.png", dpi = 300, width = 6, height = 4)

richmod <- lmer(richness ~ year + (1|hexID), data = rich)
print(richmod)
summary(richmod)


## Make NMDS of richness
library(vegan)
summary(rich2)

# filter to remove NA
rich3 <- rich2[-c(9:10),]
rich4 <- rich3[,c(2:9,11:24,26:31)]
summary(rich4)
NMDS <- metaMDS(rich4, distance="bray")
plot(NMDS)

#extract NMDS scores (x and y coordinates) for sites from newer versions of vegan package
data.scores <- as.data.frame(scores(NMDS)$sites,)

# add columns to dataframe
#add columns to data frame 
data.scores$hexID = rich3$hexID

species.scores <- as.data.frame(scores(NMDS, "species"))
species.scores$species <- rownames(species.scores)


head(data.scores)

library(ggplot2)
library(ggrepel)

xx <- ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) +
  geom_point(pch = 21,
             size = 4,
             aes(fill = hexID),
             alpha = 0.7) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  geom_label_repel(
    data = data.scores,
    aes(
      x = NMDS1,
      y = NMDS2,
      label = hexID,
      arrow = TRUE,
      color = as.factor(hexID)
    ),
    vjust = 0
  ) +  # add the site labels
  geom_text(data = species.scores,
            aes(x = NMDS1, y = NMDS2, label = species),
            alpha = 1) +  # add the species labels
  theme(
    axis.text.y = element_text(
      colour = "black",
      size = 12,
      face = "bold"
    ),
    axis.text.x = element_text(
      colour = "black",
      face = "bold",
      size = 12
    ),
    legend.text = element_text(size = 12, colour = "black"),
    legend.position = "right",
    axis.title.y = element_text(face = "bold", size = 14),
    axis.title.x = element_text(
      face = "bold",
      size = 14,
      colour = "black"
    ),
    legend.title = element_text(
      size = 14,
      colour = "black",
      face = "bold"
    ),
    panel.background = element_blank(),
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      size = 1
    ),
    legend.key = element_blank()
  ) +
  labs(
    x = "NMDS1",
    colour = "hexID",
    y = "NMDS2",
    fill = "hexID"
  )
print(xx)
ggsave(xx, filename = "figures/NMDS30yr.png", dpi = 300, height = 6, width = 7)

## what if look at community in 1988, 1998, 2008, 2018 ----
## exclude the 1300s
presmat

## 1988 data ----

# replace > 1 with 1
presmat2 <- presmat %>%
  dplyr::mutate(across(.fns = ~ifelse(. %in% c(2,3,4), 1, 0)))

dat88 <- presmat2 %>% filter(year == 1988)
dat88 <- dat88[-c(9:10),]
dat88use <- dat88[,-1]
# eliminate columns with all zeroes
dat98use2 <- dat98use %>%
  select(where(~ any(. != 0)))

NMDS88 <- metaMDS(dat98use2, distance="bray")
plot(NMDS88)

#extract NMDS scores (x and y coordinates) for sites from newer versions of vegan package
data.scores <- as.data.frame(scores(NMDS88)$sites,)

# add columns to dataframe
#add columns to data frame 
data.scores$hexID = dat88$hexID

species.scores <- as.data.frame(scores(NMDS88, "species"))
species.scores$species <- rownames(species.scores)

xx <- ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) +
  ggtitle("Community presence/absence 1988") +
  geom_point(pch = 21,
             size = 4,
             aes(fill = hexID),
             alpha = 0.7) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  geom_label_repel(
    data = data.scores,
    aes(
      x = NMDS1,
      y = NMDS2,
      label = hexID,
      arrow = TRUE,
      color = as.factor(hexID)
    ),
    vjust = 0
  ) +  # add the site labels
  geom_text(data = species.scores,
            aes(x = NMDS1, y = NMDS2, label = species),
            alpha = 1) +  # add the species labels
  theme(
    axis.text.y = element_text(
      colour = "black",
      size = 12,
      face = "bold"
    ),
    axis.text.x = element_text(
      colour = "black",
      face = "bold",
      size = 12
    ),
    legend.text = element_text(size = 12, colour = "black"),
    legend.position = "right",
    axis.title.y = element_text(face = "bold", size = 14),
    axis.title.x = element_text(
      face = "bold",
      size = 14,
      colour = "black"
    ),
    legend.title = element_text(
      size = 14,
      colour = "black",
      face = "bold"
    ),
    panel.background = element_blank(),
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      size = 1
    ),
    legend.key = element_blank()
  ) +
  labs(
    x = "NMDS1",
    colour = "hexID",
    y = "NMDS2",
    fill = "hexID"
  )
print(xx)
ggsave(xx, filename = "figures/NMDS30yr_1988.png", dpi = 300, height = 6, width = 7)

## 1998 data ----

# replace > 1 with 1
presmat2 <- presmat %>%
  dplyr::mutate(across(.fns = ~ifelse(. %in% c(2,3,4), 1, 0)))

dat98 <- presmat2 %>% filter(year == 1998)
dat98 <- dat98[-c(9:10),]
dat98use <- dat98[,-1]
# eliminate columns with all zeroes
dat98use2 <- dat98use %>%
  select(where(~ any(. != 0)))

NMDS98 <- metaMDS(dat98use2, distance="bray")
plot(NMDS98)

#extract NMDS scores (x and y coordinates) for sites from newer versions of vegan package
data.scores <- as.data.frame(scores(NMDS98)$sites,)

# add columns to dataframe
#add columns to data frame 
data.scores$hexID = dat98$hexID

species.scores <- as.data.frame(scores(NMDS98, "species"))
species.scores$species <- rownames(species.scores)

xx <- ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) +
  ggtitle("Community presence/absence 1998") +
  geom_point(pch = 21,
             size = 4,
             aes(fill = hexID),
             alpha = 0.7) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  geom_label_repel(
    data = data.scores,
    aes(
      x = NMDS1,
      y = NMDS2,
      label = hexID,
      arrow = TRUE,
      color = as.factor(hexID)
    ),
    vjust = 0
  ) +  # add the site labels
  geom_text(data = species.scores,
            aes(x = NMDS1, y = NMDS2, label = species),
            alpha = 1) +  # add the species labels
  theme(
    axis.text.y = element_text(
      colour = "black",
      size = 12,
      face = "bold"
    ),
    axis.text.x = element_text(
      colour = "black",
      face = "bold",
      size = 12
    ),
    legend.text = element_text(size = 12, colour = "black"),
    legend.position = "right",
    axis.title.y = element_text(face = "bold", size = 14),
    axis.title.x = element_text(
      face = "bold",
      size = 14,
      colour = "black"
    ),
    legend.title = element_text(
      size = 14,
      colour = "black",
      face = "bold"
    ),
    panel.background = element_blank(),
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      size = 1
    ),
    legend.key = element_blank()
  ) +
  labs(
    x = "NMDS1",
    colour = "hexID",
    y = "NMDS2",
    fill = "hexID"
  )
print(xx)
ggsave(xx, filename = "figures/NMDS30yr_1998.png", dpi = 300, height = 6, width = 7)


## 1998 data ---

# replace > 1 with 1
presmat2 <- presmat %>%
  dplyr::mutate(across(.fns = ~ifelse(. %in% c(2,3,4), 1, 0)))

dat98 <- presmat2 %>% filter(year == 1998)
dat98 <- dat98[-c(9:10),]
dat98use <- dat98[,-1]
# eliminate columns with all zeroes
dat98use2 <- dat98use %>%
  select(where(~ any(. != 0)))

NMDS98 <- metaMDS(dat98use2, distance="bray")
plot(NMDS98)

#extract NMDS scores (x and y coordinates) for sites from newer versions of vegan package
data.scores <- as.data.frame(scores(NMDS98)$sites,)

# add columns to dataframe
#add columns to data frame 
data.scores$hexID = dat98$hexID

species.scores <- as.data.frame(scores(NMDS98, "species"))
species.scores$species <- rownames(species.scores)

xx <- ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) +
  ggtitle("Community presence/absence 1998") +
  geom_point(pch = 21,
             size = 4,
             aes(fill = hexID),
             alpha = 0.7) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  geom_label_repel(
    data = data.scores,
    aes(
      x = NMDS1,
      y = NMDS2,
      label = hexID,
      arrow = TRUE,
      color = as.factor(hexID)
    ),
    vjust = 0
  ) +  # add the site labels
  geom_text(data = species.scores,
            aes(x = NMDS1, y = NMDS2, label = species),
            alpha = 1) +  # add the species labels
  theme(
    axis.text.y = element_text(
      colour = "black",
      size = 12,
      face = "bold"
    ),
    axis.text.x = element_text(
      colour = "black",
      face = "bold",
      size = 12
    ),
    legend.text = element_text(size = 12, colour = "black"),
    legend.position = "right",
    axis.title.y = element_text(face = "bold", size = 14),
    axis.title.x = element_text(
      face = "bold",
      size = 14,
      colour = "black"
    ),
    legend.title = element_text(
      size = 14,
      colour = "black",
      face = "bold"
    ),
    panel.background = element_blank(),
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      size = 1
    ),
    legend.key = element_blank()
  ) +
  labs(
    x = "NMDS1",
    colour = "hexID",
    y = "NMDS2",
    fill = "hexID"
  )
print(xx)
ggsave(xx, filename = "figures/NMDS30yr_1998.png", dpi = 300, height = 6, width = 7)



# trellis plot ----
# a nice way to look at likely estimates but in frequentist frameworks

# trellis of fish richness vs. TP by month
# non-pooled
xyplot(richness ~ year|as.factor(hexID), data = rich, panel=function(x,y,...){
  panel.xyplot(x, y, ...)
  panel.lmline(x,y,...)}, ylab="Richness", xlab="Year")

# why won't this work?
mod_rich1 <- lmer(richness ~ year + (1|hexID), data = rich)
summary(mod_rich1)

mod_rich <- brm(
  richness ~ year + (1 + year|hexID), 
  data = rich, 
  family = gaussian,
  iter = 10000,
  chains = 4,
  thin = 5000
)
summary(mod_rich)

rich %>%
  data_grid(year = seq_range(year, n = 101)) %>%    # add am to the prediction grid
  add_predicted_draws(mod_rich) %>%
  ggplot(aes(x = year, y = richness)) +
  theme_bw() +
  stat_lineribbon(aes(y = .prediction), .width = c(.95, .8, .5), color = "#08519C") +
  geom_point(data = rich) +
  scale_fill_brewer(palette = "BuGn") +
  facet_wrap(~ hexID)                                  # facet by hexID


# ## hierarchical model
# ## libraries
# library(rstanarm)
# 
# 
# b <- stan_glmer(
#   richness ~ year + (year | hexID),
#   family = gaussian(),
#   data = rich,
#   prior = normal(0, 2, autoscale = TRUE),
#   prior_intercept = normal(0, 5, autoscale = TRUE),
#   prior_covariance = decov(regularization = 2),
#   prior_aux = cauchy(0, 1, autoscale = TRUE), 
#   chains = 4,
#   iter = 10000,
#   cores = 3,
#   # reproducible blogging
#   seed = 20211116
# )
# 
# # print model
# summary(b, digits = 4)
# 
# # Get a dataframe: One row per posterior sample
# df_posterior <- b %>% 
#   as.data.frame() %>% 
#   as_tibble()
# 
# ## Manipulate data into a usable form for plotting
# # For each sample, add the average intercept and average slope values to each
# # participant's deviation from that average. These yields the intercept and
# # slope parameters for each participant.
# df_effects <- df_posterior %>%
#   mutate(
#     # Find all the columns with the pattern "b[(Intercept". Add the column
#     # `(Intercept)` to each of those columns.
#     across(
#       .cols = matches("b\\[\\(Intercept"), 
#       .fns = ~ . + `(Intercept)`
#     ),
#     # Again for slope
#     across(
#       .cols = matches("b\\[year"), 
#       .fns = ~ . + year
#     )
#   )
# 
# # Convert to a long format
# df_long_effects <- df_effects %>%
#   select(matches("b\\[")) %>%
#   rowid_to_column("draw") %>%
#   tidyr::pivot_longer(
#     cols = c(-draw),
#     # when we make new columns with pivot_ functions, the
#     # they get quotes
#     names_to = "Parameter", 
#     values_to = "Value"
#   )
# 
# df_long_effects <- df_effects %>% 
#   dplyr::select(starts_with("b[")) %>% 
#   rownames_to_column("draw") %>%
#   tidyr::gather(Parameter, Value, -draw)
# 
# # Extract the effect type and subject number from each parameter name
# df_long_effects$Type <- df_long_effects$Parameter %>%
#   stringr::str_detect("Intercept") %>%
#   ifelse(., "Intercept", "Slope_year")
# 
# df_long_effects$hexID <- df_long_effects$Parameter %>%
#   stringr::str_extract(c("\\d+|other"))
# 
# df_long_effects <- df_long_effects %>% 
#   dplyr::select(draw, hexID, Effect = Type, Value)
# 
# # choose 50 posterior samples
# df_samples <- df_long_effects %>%
#   filter(draw %in% sample(1:4000, size = 50)) %>%
#   tidyr::spread(Effect, Value)
# df_samples
# 
# # # Extract the effect type and subject number from each parameter name
# # df_long_effects <- df_long_effects %>% 
# #   mutate(
# #     Effect = Parameter %>% 
# #       stringr::str_detect("Intercept") %>%
# #       ifelse(., "Intercept", "Slope_year"),
# #     hexID = Parameter %>%
# #       stringr::str_extract("\\d\\d\\d")
# #   ) %>% 
# #   select(draw, hexID, Effect, Value)
# 
# # # Finally!
# # df_long_effects
# # #> # A tibble: 160,000 × 4
# # #>     draw Subject Effect     Value
# # #>    <int> <chr>   <chr>      <dbl>
# # #>  1     1 308     Intercept 256.  
# # #>  2     1 308     Slope_Day  19.5 
# # #>  3     1 309     Intercept 208.  
# # #>  4     1 309     Slope_Day   2.49
# # #>  5     1 310     Intercept 197.  
# # #>  6     1 310     Slope_Day   9.00
# # #>  7     1 330     Intercept 281.  
# # #>  8     1 330     Slope_Day   5.21
# # #>  9     1 331     Intercept 307.  
# # #> 10     1 331     Slope_Day   1.16
# # #> # … with 159,990 more rows
# 
# # df_long_effects2 <- df_long_effects %>%
# #   tidyr::pivot_wider(names_from = Effect, values_from = Value)

# # For reproducibility
# set.seed(20220330)
# 
# ## Choose 50 posterior samples for plotting
# df_samples <- df_long_effects %>%
#   group_by(draw, hexID) %>%
#   filter(draw %in% sample(1:4000, size = 50)) %>%
#   mutate(row = row_number()) %>%
#   tidyr::pivot_wider(names_from = Effect, values_from = Value) %>%
#   select(-row)

# df_samples
# 
# # # plot up slope and intercept estimates
# # ggplot(df_long_effects %>% tidyr::spread(Effect, Value)) + 
# #   aes(x = Intercept, y = Slope_year) + 
# #   stat_density_2d(aes(fill = ..level..), geom = "polygon") +
# #   facet_wrap("hexID") + 
# #   xlab("Intercept estimate") + 
# #   ylab("Slope estimate") +
# #   theme(legend.position = "bottom") +
# #   guides(fill = "none")
# 
# # get rid of NAs
# df_samples <- sapply(df_samples, as.character)
# df_samples[is.na(df_samples)] <- " "
# df_samples <- as.data.frame(df_samples)
# 
# df_samples2 <- df_samples %>% 
#   group_by(draw, hexID) %>% 
#   summarise_all(funs(trimws(paste(., collapse = '')))) -> df
# 
# df_samples2 %>%
#   mutate(Intercept = as.numeric(Intercept),
#          Slope_year = as.numeric(Slope_year))
# 
# ## plot with posteriors for each site
# # ggplot(rich) +
# #   aes(x = year, y = richness) +
# #   geom_abline(
# #     aes(intercept = Intercept, slope = Slope_year), 
# #     data = df_samples2, 
# #     color = "#3366FF", 
# #     alpha = .1
# #   ) +
# #   geom_point() +
# #   facet_wrap("hexID") + 
# #   scale_x_continuous(breaks = 0:4 * 2) + 
# #   labs(x = xlab, y = ylab) 
# 
# # Figure 3 final
# # plot all stations with posterior draws
# richplot2 <- ggplot(rich) +
#   aes(x = year, y = richness) +
#   geom_abline(aes(intercept = Intercept, slope = Slope_year), 
#               data = df_samples, color = "dark gray", alpha = .2) +
#   # geom_abline(aes(intercept = b(Intercept), slope = b(Slope)), data = rich) + 
#   geom_point(alpha = 0.7, pch = 21, aes(fill = as.factor(hexID)), size = 2) +
#   scale_fill_brewer(palette = "BrBG") +
#   theme_classic() + 
#   facet_wrap("hexID", ncol = 5) +
#   xlab("Year") +
#   ylab("Fish species richness") +
#   # theme(axis.title.x = element_blank(), 
#   #       axis.title.y = element_blank(),
#   #       axis.text.x = element_blank(),
#   #       axis.text.y = element_blank()) +
#   #ylab(expression(paste("ln(Chl ", italic("a"),") (", mu,g," ", L^-1,")"))) +
#   guides(color = guide_legend(title = "Year")) + # to make three columns, ncol = 3)) + 
#   theme(legend.position = "none") # to move to bottom: c(.75,.1)) +
# print(richplot2)
# 
# ggsave(richplot2, filename = "figures/longfishrichness.png", dpi = 300, width = 10, height = 5)

### permanova with species over time
# NOT RUN {
library(PERMANOVA)
data(wine)
X = wine[,4:21]
X=IniTransform(X)
D = DistContinuous (X)
perwine=PERMANOVA(D, wine$Group)
perwine


C = matrix(c(1, 1, -1, 1, 1, -1, 1, 1, 1, -1, -1, 1), nrow=3, byrow=TRUE)
rownames(C)=c("C1", "C2", "C3")
colnames(C)=levels(wine$Group)

effects=factor(c(1,2,3))
levels(effects)=c("Origin", "Year", "Interaction")
perwine2=PERMANOVA(D, wine$Group, C=C, Effects=effects, CoordPrinc = TRUE)
summary(perwine2)


## species changes over time ----
unique(longfish$species)
View(presmat)

# [1] Catostomus clarkii       Gambusia affinis         Agosia chrysogaster     
# [4] Catostomus insignis      Cyprinella lutrensis     Rhinichthys cobitis     
# [7] Micropterus dolomieu     Micropterus salmoides    Ameiurus natalis        
# [10] Pimephales promelas      Pylodictis olivaris      Ameiurus melas          
# [13] Cyprinus carpio          Ictalurus punctatus      Lepomis cyanellus       
# [16] Gila robusta             Meda fulgida             Gila nigra              
# [19] Rhinichthys osculus      Oncorhynchus mykiss      Salmo trutta            
# [22] Oncorhynchus gilae       Culaea inconstans        Salvelinus fontinalis   
# [25] Oncorhynchus nerka       Catostomus platyrhynchus Prosopium williamsoni   
# [28] Salvelinus namaycush     Richardsonius balteatus  Cottus bairdii          
# [31] Couesius plumbeus        Oncorhynchus clarkii     Thymallus arcticus

# Catostomus clarkii
cacl <- longfish %>% filter(species == "Catostomus clarkii")
cacl <- presmat[,c(1:2,4)]
cacl$count <- cacl$`Catostomus clarkii`
cacl$presabs <- as.factor(ifelse(cacl$count >= 1, 1, 0))
caclplot <- ggplot(aes(x = year, y = hexID, fill = presabs), data = cacl) +
  geom_point(pch = 21, size = 3, alpha = 0.9) +
  scale_fill_manual(name = "Present?",
                    values = c("white", "darkblue")) +
  theme_bw(base_size = 14) +
  ggtitle("Catostomus clarkii")
print(caclplot2)



# Gambusia affinis
# gaaf <- longfish %>% filter(species == "Gambusia affinis")
gaaf <- presmat[,c(1:2,6)]
gaaf$count <- gaaf$`Gambusia affinis`
gaaf$presabs <- as.factor(ifelse(gaaf$count >= 1, 1, 0))
gaafplot <- ggplot(aes(x = year, y = hexID, fill = presabs), data = gaaf) +
  geom_point(pch = 21, size = 3, alpha = 0.9) +
  scale_fill_manual(name = "Present?",
                    values = c("white", "darkblue")) +
  theme_bw(base_size = 14) +
  xlim(1968, 2020) +
  ggtitle("Gambusia affinis")
print(gaafplot)

# Agosia chrysogaster
# agch <- longfish %>% filter(species == "Agosia chrysogaster")
agch <- presmat[,c(1:2,3)]
agch$count <- agch$`Agosia chrysogaster`
agch$presabs <- as.factor(ifelse(agch$count >= 1, 1, 0))
agchplot <- ggplot(aes(x = year, y = hexID, fill = presabs), data = agch) +
  geom_point(pch = 21, size = 3, alpha = 0.9) +
  scale_fill_manual(name = "Present?",
                    values = c("white", "darkblue")) +
  theme_bw(base_size = 14) +
  xlim(1968, 2020) +
  ggtitle("Agosia chrysogaster")
print(agchplot)

# Catostomus insignis
# cain <- longfish %>% filter(species == "Catostomus insignis")
cain <- presmat[,c(1:2,5)]
cain$count <- cain$`Catostomus insignis`
cain$presabs <- as.factor(ifelse(cain$count >= 1, 1, 0))

cainplot <- ggplot(aes(x = year, y = hexID, fill = presabs), data = cain) +
  geom_point(pch = 21, size = 3, alpha = 0.9) + 
  # scale_fill_viridis_d(begin = 0.3, end = 0.6) +
  scale_fill_manual(name = "Present?",
                    values = c("white", "darkblue")) +
  xlim(1968, 2020) +
  theme_bw(base_size = 14) +
  ggtitle("Catostomus insignis")
print(cainplot)
ggsave(cainplot, filename = "figures/CaInPresAbs.png", dpi = 300, width = 6, height = 3)

#  Cyprinella lutrensis
# cylu <- longfish %>% filter(species == "Cyprinella lutrensis")
cylu <- presmat[,c(1:2,13)]
cylu$count <- cylu$`Cyprinella lutrensis`
cylu$presabs <- as.factor(ifelse(cylu$count >= 1, 1, 0))

cyluplot <- ggplot(aes(x = year, y = hexID, fill = presabs), data = cylu) +
  geom_point(pch = 21, size = 3, alpha = 0.9) + 
  scale_fill_manual(name = "Present?",
                    values = c("white", "darkblue")) +
  theme_bw(base_size = 14) +
  xlim(1968, 2020) +
  ggtitle("Cyprinella lutrensis")
print(cyluplot)

#  Rhinichthys cobitis
rhco <- presmat[,c(1:2,9)]
rhco$count <- rhco$`Rhinichthys cobitis`
rhco$presabs <- as.factor(ifelse(rhco$count >= 1, 1, 0))
# rhco <- longfish %>% filter(species == "Rhinichthys cobitis")
rhcoplot <- ggplot(aes(x = year, y = hexID, fill = presabs), data = rhco) +
  geom_point(pch = 21, size = 3, alpha = 0.9) + 
  scale_fill_manual(name = "Present?",
                    values = c("white", "darkblue")) +
  theme_bw(base_size = 14) +
  xlim(1968, 2020) +
  ggtitle("Rhinichthys cobitis")
print(rhcoplot)

#  Micropterus dolomieu
# mido <- longfish %>% filter(species == "Micropterus dolomieu")
mido <- presmat[,c(1:2,8)]
mido$count <- mido$`Micropterus dolomieu`
mido$presabs <- as.factor(ifelse(mido$count >= 1, 1, 0))
midoplot <- ggplot(aes(x = year, y = hexID, fill = presabs), data = mido) +
  geom_point(pch = 21, size = 3, alpha = 0.9) +
  scale_fill_manual(name = "Present?",
                    values = c("white", "darkblue")) +
  xlim(1968, 2020) +
  theme_bw(base_size = 14) +
  ggtitle("Micropterus dolomieu")
print(midoplot)

# Micropterus salmoides
# misa <- longfish %>% filter(species == "Micropterus salmoides")
misa <- presmat[,c(1:2,14)]
misa$count <- misa$`Micropterus salmoides`
misa$presabs <- as.factor(ifelse(misa$count >= 1, 1, 0))
misaplot <- ggplot(aes(x = year, y = hexID, fill = presabs), data = misa) +
  geom_point(pch = 21, size = 3, alpha = 0.9) +
  scale_fill_manual(name = "Present?",
                    values = c("white", "darkblue")) +
  theme_bw(base_size = 14) +
  xlim(1968, 2020) +
  ggtitle("Micropterus salmoides")
print(misaplot)

# Ameiurus natalis
# amna <- longfish %>% filter(species == "Ameiurus natalis")
amna <- presmat[,c(1:2,16)]
amna$count <- amna$`Ameiurus natalis`
amna$presabs <- as.factor(ifelse(amna$count >= 1, 1, 0))
amnaplot <- ggplot(aes(x = year, y = hexID, fill = presabs), data = amna) +
  geom_point(pch = 21, size = 3, alpha = 0.9) +
  scale_fill_manual(name = "Present?",
                    values = c("white", "darkblue")) +
  theme_bw(base_size = 14) +
  xlim(1968, 2020) +
  ggtitle("Ameiurus natalis")
print(amnaplot)

# Pimephales promelas
# pipo <- longfish %>% filter(species == "Pimephales promelas")
pipo <- presmat[,c(1:2,19)]
pipo$count <- pipo$`Pimephales promelas`
pipo$presabs <- as.factor(ifelse(pipo$count >= 1, 1, 0))
pipoplot <- ggplot(aes(x = year, y = hexID, fill = presabs), data = pipo) +
  geom_point(pch = 21, size = 3, alpha = 0.9) +
  scale_fill_manual(name = "Present?",
                    values = c("white", "darkblue")) +
  theme_bw(base_size = 14) +
  xlim(1968, 2020) +
  ggtitle("Pimephales promelas")
print(pipoplot)

# Pylodictis olivaris
# pyol <- longfish %>% filter(species == "Pylodictis olivaris")
pyol <- presmat[,c(1:2,12)]
pyol$count <- pyol$`Pylodictis olivaris`
pyol$presabs <- as.factor(ifelse(pyol$count >= 1, 1, 0))
pyolplot <- ggplot(aes(x = year, y = hexID, fill = presabs), data = pyol) +
  geom_point(pch = 21, size = 3, alpha = 0.9) +
  scale_fill_manual(name = "Present?",
                    values = c("white", "darkblue")) +
  xlim(1968, 2020) +
  theme_bw(base_size = 14) +
  ggtitle("Pylodictis olivaris")
print(pyolplot)

# Ameiurus melas
# amme <- longfish %>% filter(species == "Ameiurus melas")
amme <- presmat[,c(1:2,15)]
amme$count <- amme$`Ameiurus melas`
amme$presabs <- as.factor(ifelse(amme$count >= 1, 1, 0))
ammeplot <- ggplot(aes(x = year, y = hexID, fill = presabs), data = amme) +
  geom_point(pch = 21, size = 3, alpha = 0.9) +
  scale_fill_manual(name = "Present?",
                    values = c("white", "darkblue")) +
  theme_bw(base_size = 14) +
  xlim(1968, 2020) +
  ggtitle("Ameiurus melas")
print(ammeplot)

# Cyprinus carpio
cyca <- longfish %>% filter(species == "Cyprinus carpio")
cyca <- presmat[,c(1:2,17)]
cyca$count <- cyca$`Cyprinus carpio`
cyca$presabs <- as.factor(ifelse(cyca$count >= 1, 1, 0))
cycaplot <- ggplot(aes(x = year, y = hexID, fill = presabs), data = cyca) +
  geom_point(pch = 21, size = 3, alpha = 0.9) +
  scale_fill_manual(name = "Present?",
                    values = c("white", "darkblue")) +
  theme_bw(base_size = 14) +
  xlim(1968, 2020) +
  ggtitle("Cyprinus carpio")
print(cycaplot)

# Ictalurus punctatus
icpu <- longfish %>% filter(species == "Ictalurus punctatus")
icpu <- presmat[,c(1:2,10)]
icpu$count <- icpu$`Ictalurus punctatus`
icpu$presabs <- as.factor(ifelse(icpu$count >= 1, 1, 0))
icpuplot <- ggplot(aes(x = year, y = hexID, fill = presabs), data = icpu) +
  geom_point(pch = 21, size = 3, alpha = 0.9) +
  scale_fill_manual(name = "Present?",
                    values = c("white", "darkblue")) +
  theme_bw(base_size = 14) +
  xlim(1968, 2020) +
  ggtitle("Ictalurus punctatus")
print(icpuplot)

# Lepomis cyanellus
lecy <- longfish %>% filter(species == "Lepomis cyanellus")
lecy <- presmat[,c(1:2,11)]
lecy$count <- lecy$`Lepomis cyanellus`
lecy$presabs <- as.factor(ifelse(lecy$count >= 1, 1, 0))
lecyplot <- ggplot(aes(x = year, y = hexID, fill = presabs), data = lecy) +
  geom_point(pch = 21, size = 3, alpha = 0.9) +
  scale_fill_manual(name = "Present?",
                    values = c("white", "darkblue")) +
  xlim(1968, 2020) +
  theme_bw(base_size = 14) +
  ggtitle("Lepomis cyanellus")
print(lecyplot)

# Gila robusta
# giro <- longfish %>% filter(species == "Gila robusta")
giro <- presmat[,c(1:2,18)]
giro$count <- giro$`Gila robusta`
giro$presabs <- as.factor(ifelse(giro$count >= 1, 1, 0))
giroplot <- ggplot(aes(x = year, y = hexID, fill = presabs), data = giro) +
  geom_point(pch = 21, size = 3, alpha = 0.9) +
  scale_fill_manual(name = "Present?",
                    values = c("white", "darkblue")) +
  xlim(1968, 2020) +
  theme_bw(base_size = 14) +
  ggtitle("Gila robusta")
print(giroplot)

# Meda fulgida
mefu <- longfish %>% filter(species == "Meda fulgida")
mefu <- presmat[,c(1:2,7)]
mefu$count <- mefu[,3]
mefu$presabs <- as.factor(ifelse(mefu$count >= 1, 1, 0))
mefuplot <- ggplot(aes(x = year, y = hexID, fill = presabs), data = mefu) +
  geom_point(pch = 21, size = 3, alpha = 0.9) +
  scale_fill_manual(name = "Present?",
                    values = c("white", "darkblue")) +
  theme_bw(base_size = 14) +
  xlim(1968, 2020) +
  ggtitle("Meda fulgida")
print(mefuplot)

# Gila nigra
gini <- longfish %>% filter(species == "Gila nigra")
gini <- presmat[,c(1:2,20)]
gini$count <- gini$`Gila nigra`
gini$presabs <- as.factor(ifelse(gini$count >= 1, 1, 0))
giniplot <- ggplot(aes(x = year, y = hexID, fill = presabs), data = gini) +
  geom_point(pch = 21, size = 3, alpha = 0.9) +
  scale_fill_manual(name = "Present?",
                    values = c("white", "darkblue")) +
  theme_bw(base_size = 14) +  
  xlim(1968, 2020) +
  ggtitle("Gila nigra")
print(giniplot)

# Rhinichthys osculus
rhos <- longfish %>% filter(species == "Rhinichthys osculus")
rhos <- presmat[,c(1:2,21)]
rhos$count <- rhos$`Rhinichthys osculus`
rhos$presabs <- as.factor(ifelse(rhos$count >= 1, 1, 0))
rhosplot <- ggplot(aes(x = year, y = hexID, fill = presabs), data = rhos) +
  geom_point(pch = 21, size = 3, alpha = 0.9) +
  scale_fill_manual(name = "Present?",
                    values = c("white", "darkblue")) +
  theme_bw(base_size = 14) +
  xlim(1968, 2020) +
  ggtitle("Rhinichthys osculus")
print(rhosplot)

# Oncorhynchus mykiss
onmy <- longfish %>% filter(species == "Oncorhynchus mykiss")
onmy <- presmat[,c(1:2,23)]
onmy$count <- onmy$`Oncorhynchus mykiss`
onmy$presabs <- as.factor(ifelse(onmy$count >= 1, 1, 0))
onmyplot <- ggplot(aes(x = year, y = hexID, fill = presabs), data = onmy) +
  geom_point(pch = 21, size = 3, alpha = 0.9) +
  scale_fill_manual(name = "Present?",
                    values = c("white", "darkblue")) +
  theme_bw(base_size = 14) +
  xlim(1968, 2020) +
  ggtitle("Oncorhynchus mykiss")
print(onmyplot)

# Salmo trutta
satr <- longfish %>% filter(species == "Salmo trutta")
satr <- presmat[,c(1:2,22)]
satr$count <- satr$`Salmo trutta`
satr$presabs <- as.factor(ifelse(satr$count >= 1, 1, 0))
satrplot <- ggplot(aes(x = year, y = hexID, fill = presabs), data = satr) +
  geom_point(pch = 21, size = 3, alpha = 0.9) +
  scale_fill_manual(name = "Present?",
                    values = c("white", "darkblue")) +
  xlim(1968, 2020) +
  theme_bw(base_size = 14) +
  ggtitle("Salmo trutta")
print(satrplot)

# Oncorhynchus gilae
ongi <- longfish %>% filter(species == "Oncorhynchus gilae")
ongi <- presmat[,c(1:2,24)]
ongi$count <- ongi$`Oncorhynchus gilae`
ongi$presabs <- as.factor(ifelse(ongi$count >= 1, 1, 0))
ongiplot <- ggplot(aes(x = year, y = hexID, fill = presabs), data = ongi) +
  geom_point(pch = 21, size = 3, alpha = 0.9) +
  scale_fill_manual(name = "Present?",
                    values = c("white", "darkblue")) +
  theme_bw(base_size = 14) +
  xlim(1968, 2020) +
  ggtitle("Oncorhynchus gilae")
print(ongiplot)

# Culaea inconstans
cuin <- longfish %>% filter(species == "Culaea inconstans")
cuin <- presmat[,c(1:2,25)]
cuin$count <- cuin$`Culaea inconstans`
cuin$presabs <- as.factor(ifelse(cuin$count >= 1, 1, 0))
cuinplot <- ggplot(aes(x = year, y = hexID, fill = presabs), data = cuin) +
  geom_point(pch = 21, size = 3, alpha = 0.9) +
  scale_fill_manual(name = "Present?",
                    values = c("white", "darkblue")) +
  theme_bw(base_size = 14) +
  xlim(1968, 2020) +
  ggtitle("Culaea inconstans")
print(cuinplot)

# Salvelinus fontinalis
safo <- longfish %>% filter(species == "Salvelinus fontinalis")
safo <- presmat[,c(1:2,26)]
safo$count <- safo$`Salvelinus fontinalis`
safo$presabs <- as.factor(ifelse(safo$count >= 1, 1, 0))
safoplot <- ggplot(aes(x = year, y = hexID, fill = presabs), data = safo) +
  geom_point(pch = 21, size = 3, alpha = 0.9) +
  scale_fill_manual(name = "Present?",
                    values = c("white", "darkblue")) +
  xlim(1968, 2020) +
  theme_bw(base_size = 14) +
  ggtitle("Salvelinus fontinalis")
print(safoplot)

# Oncorhynchus nerka
onne <- longfish %>% filter(species == "Oncorhynchus nerka")
onne <- presmat[,c(1:2,30)]
onne$count <- onne$`Oncorhynchus nerka`
onne$presabs <- as.factor(ifelse(onne$count >= 1, 1, 0))
onneplot <- ggplot(aes(x = year, y = hexID, fill = presabs), data = onne) +
  geom_point(pch = 21, size = 3, alpha = 0.9) +
  scale_fill_manual(name = "Present?",
                    values = c("white", "darkblue")) +
  theme_bw(base_size = 14) +
  xlim(1968, 2020) +
  ggtitle("Oncorhynchus nerka")
print(onneplot)

# Catostomus platyrhynchus
capl <- longfish %>% filter(species == "Catostomus platyrhynchus")
capl <- presmat[,c(1:2,31)]
capl$count <- capl$`Catostomus platyrhynchus`
capl$presabs <- as.factor(ifelse(capl$count >= 1, 1, 0))
caplplot <- ggplot(aes(x = year, y = hexID, fill = presabs), data = capl) +
  geom_point(pch = 21, size = 3, alpha = 0.9) +
  scale_fill_manual(name = "Present?",
                    values = c("white", "darkblue")) +
  theme_bw(base_size = 14) +
  xlim(1968, 2020) +
  ggtitle("Catostomus platyrhynchus")
print(caplplot)

# Prosopium williamsoni
prwi <- longfish %>% filter(species == "Prosopium williamsoni")
prwi <- presmat[,c(1:2,33)]
prwi$count <- prwi$`Prosopium williamsoni`
prwi$presabs <- as.factor(ifelse(prwi$count >= 1, 1, 0))
prwiplot <- ggplot(aes(x = year, y = hexID, fill = presabs), data = prwi) +
  geom_point(pch = 21, size = 3, alpha = 0.9) +
  scale_fill_manual(name = "Present?",
                    values = c("white", "darkblue")) +
  theme_bw(base_size = 14) +
  xlim(1968, 2020) +
  ggtitle("Prosopium williamsoni")
print(prwiplot)

# Salvelinus namaycush
sana <- longfish %>% filter(species == "Salvelinus namaycush")
sana <- presmat[,c(1:2,29)]
sana$count <- sana$`Salvelinus namaycush`
sana$presabs <- as.factor(ifelse(sana$count >= 1, 1, 0))
sanaplot <- ggplot(aes(x = year, y = hexID, fill = presabs), data = sana) +
  geom_point(pch = 21, size = 3, alpha = 0.9) +
  scale_fill_manual(name = "Present?",
                    values = c("white", "darkblue")) +
  theme_bw(base_size = 14) +
  xlim(1968, 2020) +
  ggtitle("Salvelinus namaycush")
print(sanaplot)

# Richardsonius balteatus
riba <- longfish %>% filter(species == "Richardsonius balteatus")
riba <- presmat[,c(1:2,29)]
riba$count <- riba$`Richardsonius balteatus`
riba$presabs <- as.factor(ifelse(riba$count >= 1, 1, 0))
ribaplot <- ggplot(aes(x = year, y = hexID, fill = year), data = riba) +
  geom_point(pch = 21, size = 3, alpha = 0.9) +
  scale_fill_manual(name = "Present?",
                    values = c("white", "darkblue")) +
  theme_bw(base_size = 14) +
  xlim(1968, 2020) +
  ggtitle("Richardsonius balteatus")
print(ribaplot)

# Cottus bairdii
coba <- longfish %>% filter(species == "Cottus bairdii")
coba <- presmat[,c(1:2,32)]
coba$count <- coba$`Cottus bairdii`
coba$presabs <- as.factor(ifelse(coba$count >= 1, 1, 0))
cobaplot <- ggplot(aes(x = year, y = hexID, fill = presabs), data = coba) +
  geom_point(pch = 21, size = 3, alpha = 0.9) +
  scale_fill_manual(name = "Present?",
                    values = c("white", "darkblue")) +
  xlim(1968, 2020) +
  theme_bw(base_size = 14) +
  ggtitle("Cottus bairdii")
print(cobaplot)

# Couesius plumbeus
copl <- longfish %>% filter(species == "Couesius plumbeus")
copl <- presmat[,c(1:2,35)]
copl$count <- copl$`Couesius plumbeus`
copl$presabs <- as.factor(ifelse(copl$count >= 1, 1, 0))
coplplot <- ggplot(aes(x = year, y = hexID, fill = presabs), data = copl) +
  geom_point(pch = 21, size = 3, alpha = 0.9) +
  scale_fill_manual(name = "Present?",
                    values = c("white", "darkblue")) +
  xlim(1968, 2020) +
  theme_bw(base_size = 14) +
  ggtitle("Couesius plumbeus")
print(coplplot)

# Oncorhynchus clarkii
oncl <- longfish %>% filter(species == "Oncorhynchus clarkii")
oncl <- presmat[,c(1:2,28)]
oncl$count <- oncl$`Oncorhynchus clarkii`
oncl$presabs <- as.factor(ifelse(oncl$count >= 1, 1, 0))
onclplot <- ggplot(aes(x = year, y = hexID, fill = presabs), data = oncl) +
  geom_point(pch = 21, size = 3, alpha = 0.9) +
  scale_fill_manual(name = "Present?",
                    values = c("white", "darkblue")) +
  xlim(1968, 2020) +
  theme_bw(base_size = 14) +
  ggtitle("Oncorhynchus clarkii")
print(onclplot)

# Thymallus arcticus
thar <- longfish %>% filter(species == "Thymallus arcticus")
thar <- presmat[,c(1:2,27)]
thar$count <- thar$`Thymallus arcticus`
thar$presabs <- as.factor(ifelse(thar$count >= 1, 1, 0))
tharplot <- ggplot(aes(x = year, y = hexID, fill = presabs), data = thar) +
  geom_point(pch = 21, size = 3, alpha = 0.9) +
  scale_fill_manual(name = "Present?",
                    values = c("white", "darkblue")) +
  xlim(1968, 2020) +
  theme_bw(base_size = 14) +
  ggtitle("Thymallus arcticus")
print(tharplot)

declines <- ggpubr::ggarrange(
          agchplot,
          cyluplot,
          rhcoplot,
          midoplot,
          misaplot,
          amnaplot,
          pipoplot,
          pyolplot,
          ncol = 2,
          nrow = 4,
          align = "hv",
          common.legend = TRUE,
          legend = "right")
print(declines)
ggsave(declines, filename = "figures/PresAbsent1.png", dpi = 300, height = 12, width = 12)

declines2 <- ggpubr::ggarrange(
  ammeplot,
  cycaplot,
  icpuplot,
  lecyplot,
  mefuplot,
  giniplot,
  onmyplot,
  satrplot,
  ncol = 2,
  nrow = 4,
  align = "hv",
  common.legend = TRUE,
  legend = "right")
print(declines2)
ggsave(declines2, filename = "figures/PresAbsent2.png", dpi = 300, height = 12, width = 12)


declines3 <- ggpubr::ggarrange(
  cuinplot,
  tharplot,
  prwiplot,
  sanaplot,
  onclplot,
  ncol = 2,
  nrow = 3,
  align = "hv",
  common.legend = TRUE,
  legend = "right")
print(declines3)
ggsave(declines3, filename = "figures/PresAbsent3.png", dpi = 300, height = 12*3/4, width = 12*3/4)





# moving window manual ----
# trellis of fish richness vs. TP by month
# non-pooled
xyplot(richness ~ year|as.factor(hexID), data = rich, panel=function(x,y,...){
  panel.xyplot(x, y, ...)
  panel.lmline(x,y,...)}, ylab="Richness", xlab="Year")
lm <- lmer(richness ~ year + (1|hexID), data = rich)
summary(lm)

rich1980 <- subset(rich, year >1979)
xyplot(richness ~ year|as.factor(hexID), data = rich1980, panel=function(x,y,...){
  panel.xyplot(x, y, ...)
  panel.lmline(x,y,...)}, ylab="Richness", xlab="Year")

rich1990 <- subset(rich, year >1989)
xyplot(richness ~ year|as.factor(hexID), data = rich1990, panel=function(x,y,...){
  panel.xyplot(x, y, ...)
  panel.lmline(x,y,...)}, ylab="Richness", xlab="Year")

rich2000 <- subset(rich, year >1999)
xyplot(richness ~ year|as.factor(hexID), data = rich2000, panel=function(x,y,...){
  panel.xyplot(x, y, ...)
  panel.lmline(x,y,...)}, ylab="Richness", xlab="Year")

rich2010 <- subset(rich, year >2009)
xyplot(richness ~ year|as.factor(hexID), data = rich2010, panel=function(x,y,...){
  panel.xyplot(x, y, ...)
  panel.lmline(x,y,...)}, ylab="Richness", xlab="Year")

library(nlme)
fit <-lme(richness ~ year, random = ~1 + year | hexID, data= rich)
summary(fit)

# make predictions dataframe
new_data <- data.frame(expand.grid(year = unique(rich$year), hexID = unique(rich$hexID), stringsAsFactors = FALSE))

# add predictions 
new_data$richness <- predict(fit, newdata = new_data, re.form=NA)

# # OPTIONAL EXTRA
# # get confidence and prediction intervals around mean prediction 
# # get model matrix
# mm <- model.matrix(terms(fit_lme), new_data)
# 
# ## or newdat$distance <- mm %*% fixef(fm1)
# # variance of fixed effect
# pvar <- diag(mm %*% tcrossprod(vcov(fit_lme), mm))
# # variance of random effects
# tvar <- pvar + VarCorr(fit_lme)$individual[1]  
# cmult <- 1.96
# 
# # add columns to dataframes
# # CI gives uncertainty around fixed effects only
# # PI gives uncertainty based on random effects as well!
# new_data <- mutate(new_data,
#                    PI_low = Hue - cmult*sqrt(tvar),
#                    PI_high = Hue + cmult*sqrt(tvar),
#                    CI_low = Hue - cmult*sqrt(pvar), 
#                    CI_high = Hue + cmult*sqrt(pvar))
# 
# # create dataframe for new_data for each individual ####
# new_data_ind <- select(d, time, prawn_col, individual)
# # predict each individuals relationship including the random effects
# new_data_ind$Hue <- predict(fit_lme, new_data_ind, re.form = NULL)

# option 2
# plots the mean relationship and each individual random relationship
ggplot(rich) +
  geom_point(aes(year, richness, fill = hexID), alpha = 0.5, pch = 21, size = 2, rich) +
  # geom_line(aes(year, richness, col = hexID, group = hexID), alpha = 0.25, new_data_ind) +
  geom_line(aes(year, richness, col = hexID), new_data) +
  ylab('Fish species richness') +
  xlab('Years') +
  # facet_wrap(~ prawn_col) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  theme_bw(base_size = 14)

## package rshift
library(rshift)

# how long for each regime shift?
# start with 5
# next try 8
# end with 10

## window = 8 ----
## hexID 216
# first attempt!
hex216 <- filter(rich, hexID == "216")
hex216red <- hex216[,-1]
# for answer
mod1 <- Rodionov(hex216red, col = "richness", time = "year", l = 8)
mod1
# for figure
mod1 <- Rodionov(hex216red, col = "richness", time = "year", l = 8, merge = TRUE)
mod1
RSI_graph(mod1, col = "richness", time = "year", rsi ="RSI")

## hexID 217
hex217 <- filter(rich, hexID == "217")
hex217red <- hex217[,-1]
# for answer
mod2 <- Rodionov(hex217red, col = "richness", time = "year", l = 8)
mod2
# for figure
mod2 <- Rodionov(hex217red, col = "richness", time = "year", l = 8, merge = TRUE)
RSI_graph(mod2, col = "richness", time = "year", rsi ="RSI")

## hexID 247
hex247 <- filter(rich, hexID == "247")
hex247red <- hex247[,-1]
# for answer
mod3 <- Rodionov(hex247red, col = "richness", time = "year", l = 8)
mod3
# for figure
mod3 <- Rodionov(hex247red, col = "richness", time = "year", l = 8, merge = TRUE)
RSI_graph(mod3, col = "richness", time = "year", rsi ="RSI")


## hexID 248
hex248 <- filter(rich, hexID == "248")
hex248red <- hex248[,-1]
# for answer
mod4 <- Rodionov(hex248red, col = "richness", time = "year", l = 8)
mod4


## hexID 262
hex262 <- filter(rich, hexID == "262")
hex262red <- hex262[,-1]
# for answer
mod5 <- Rodionov(hex262red, col = "richness", time = "year", l = 8)
mod5


## hexID 263
hex263 <- filter(rich, hexID == "263")
hex263red <- hex263[,-1]
# for answer
mod6 <- Rodionov(hex263red, col = "richness", time = "year", l = 8)
mod6


## hexID 279
hex279 <- filter(rich, hexID == "279")
hex279red <- hex279[,-1]
# for answer
mod7 <- Rodionov(hex279red, col = "richness", time = "year", l = 8)
mod7


## hexID 344
hex344 <- filter(rich, hexID == "344")
hex344red <- hex344[,-1]
# for answer
mod8 <- Rodionov(hex344red, col = "richness", time = "year", l = 8)
mod8


## hexID 1370
hex1370 <- filter(rich, hexID == "1370")
hex1370red <- hex1370[,-1]
# for answer
mod9 <- Rodionov(hex1370red, col = "richness", time = "year", l = 8)
mod9


## hexID 1372
hex1372 <- filter(rich, hexID == "1372")
hex1372red <- hex1372[,-1]
# for answer
mod10 <- Rodionov(hex1372red, col = "richness", time = "year", l = 8)
mod10




## window = 10 ----
## hexID 216
# first attempt!
hex216 <- filter(rich, hexID == "216")
hex216red <- hex216[,-1]
# for answer
mod1 <- Rodionov(hex216red, col = "richness", time = "year", l = 10)
mod1
# for figure
mod1 <- Rodionov(hex216red, col = "richness", time = "year", l = 10, merge = TRUE)
mod1
RSI_graph(mod1, col = "richness", time = "year", rsi ="RSI")

## hexID 217
hex217 <- filter(rich, hexID == "217")
hex217red <- hex217[,-1]
# for answer
mod2 <- Rodionov(hex217red, col = "richness", time = "year", l = 10)
mod2
# for figure
mod2 <- Rodionov(hex217red, col = "richness", time = "year", l = 10, merge = TRUE)
RSI_graph(mod2, col = "richness", time = "year", rsi ="RSI")

## hexID 247
hex247 <- filter(rich, hexID == "247")
hex247red <- hex247[,-1]
# for answer
mod3 <- Rodionov(hex247red, col = "richness", time = "year", l = 10)
mod3
# for figure
mod3 <- Rodionov(hex247red, col = "richness", time = "year", l = 10, merge = TRUE)
RSI_graph(mod3, col = "richness", time = "year", rsi ="RSI")


## hexID 248
hex248 <- filter(rich, hexID == "248")
hex248red <- hex248[,-1]
# for answer
mod4 <- Rodionov(hex248red, col = "richness", time = "year", l = 10)
mod4


## hexID 262
hex262 <- filter(rich, hexID == "262")
hex262red <- hex262[,-1]
# for answer
mod5 <- Rodionov(hex262red, col = "richness", time = "year", l = 10)
mod5


## hexID 263
hex263 <- filter(rich, hexID == "263")
hex263red <- hex263[,-1]
# for answer
mod6 <- Rodionov(hex263red, col = "richness", time = "year", l = 10)
mod6


## hexID 279
hex279 <- filter(rich, hexID == "279")
hex279red <- hex279[,-1]
# for answer
mod7 <- Rodionov(hex279red, col = "richness", time = "year", l = 10)
mod7


## hexID 344
hex344 <- filter(rich, hexID == "344")
hex344red <- hex344[,-1]
# for answer
mod8 <- Rodionov(hex344red, col = "richness", time = "year", l = 10)
mod8


## hexID 1370
hex1370 <- filter(rich, hexID == "1370")
hex1370red <- hex1370[,-1]
# for answer
mod9 <- Rodionov(hex1370red, col = "richness", time = "year", l = 10)
mod9


## hexID 1372
hex1372 <- filter(rich, hexID == "1372")
hex1372red <- hex1372[,-1]
# for answer
mod10 <- Rodionov(hex1372red, col = "richness", time = "year", l = 10)
mod10


# plot as unpooled
library(ggplot2)

rsidat <- read.csv("data/RSI.csv", header = TRUE, fileEncoding="UTF-8-BOM")
rsidat$hexID <- as.factor(rsidat$hexID)
# filter only window = 8
rsidat8 <- filter(rsidat, window == 8)

richmerg <- left_join(rich,rsidat8, by = "hexID")



#create regression lines for all three groups
longfish2 <- ggplot(richmerg, aes(x = year.x, y = richness)) +
  geom_point(pch = 21, size = 2.5, aes(fill = hexID), alpha = 0.7) +
  geom_point(pch = 25, size = 2, aes(x = year.y, y = 2), fill = "black") +
  geom_smooth(method = "lm", fill = NA, color = "black") +
  theme_classic(base_size = 14) +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "white", colour = "grey50"),
        ) +
  scale_fill_viridis_d() +
  facet_wrap(~hexID, ncol = 4) +
  ylab("Fish species richness") +
  xlab("Year")
  
print(longfish2)
ggsave(longfish2, filename = "figures/longtermtrends&breaks.png", dpi = 300, width = 10, height = 6)
