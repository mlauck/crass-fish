# Phylomaker

# devtools::install_github("GabrielNakamura/FishPhyloMaker", ref = "main")
library(FishPhyloMaker)
library(rfishbase)
library(curl)

# # example
# data(neotropical_comm)
# data_comm <- neotropical_comm[, -c(1, 2)] # removing latitude and longitude

# load traits data
allarid <- read.csv("Data/fishstatus.csv", header = TRUE)

# make a danger fish vector
allarid$danger <- NA
allarid$danger <-
  ifelse(allarid$IUCNstatus == "LC" |
           allarid$IUCNstatus == "NE"|
           allarid$IUCNstatus == "DD", 0, 1)
head(allarid)

mod <- lm(danger ~ endemic, data = allarid)

# # make a vector of fish names
fish <- allarid[,1]

# # # replace space with underscore
# fish2 <- gsub(" ", "_", fish)
# allarid$s <- gsub(" ", "_", allarid$GenusSpecies)
# 
# # # what sort of data are they?
# # str(fish)
# # fish2 <- as.factor(fish)

# # keep only unique
fish3 <- unique(fish)

# finds family and order of species in Fishbase
# example
# taxon_data <- FishTaxaMaker(data_comm, allow.manual.insert = TRUE)

# our data - use in another 
taxon_data2 <- FishTaxaMaker(fish3, allow.manual.insert = TRUE)
# taxon_data2 <- FishTaxaMaker(fish3, allow.manual.insert = FALSE)

# write data
taxondata <- as.data.frame(taxon_data2$Taxon_data_FishPhyloMaker)

taxondata2 <- left_join(allarid, taxondata, by = "s")

orderdanger <- taxondata2 |>
  group_by(o) |>
  summarise(prop.dang = mean(danger), na.omit = TRUE)

  
write.csv(taxondata2, "Data/taxondata.csv")
write.csv(orderdanger, "Data/orderdanger.csv")

## Families and orders
# > taxon_data2 <- FishTaxaMaker(fish2, allow.manual.insert = TRUE)
# Joining with `by = join_by(Subfamily, GenCode, FamCode)`
# Joining with `by = join_by(FamCode)`
# Joining with `by = join_by(Order, Ordnum, Class, ClassNum)`
# Joining with `by = join_by(Class, ClassNum)`
# Joining with `by = join_by(Subfamily, GenCode, FamCode)`
# Joining with `by = join_by(FamCode)`
# Joining with `by = join_by(Order, Ordnum, Class, ClassNum)`
# Joining with `by = join_by(Class, ClassNum)`

# tell the Family of  Phoxinus grumi belimiauensis
# Leuciscidae
# tell the Order of  Phoxinus grumi belimiauensis
# Cypriniformes
# tell the Family of  Phoxinus czekanowskiifresh
# Leuciscidae
# tell the Order of  Phoxinus czekanowskiifresh
# Cypriniformes
# tell the Family of  Gila alutacea
# Cyprinidae
# tell the Order of  Gila alutacea
# Cypriniformes
# tell the Family of  Pantosteus plebeius
# Catostomidae
# tell the Order of  Pantosteus plebeius
# Cypriniformes
# tell the Family of  Cyprinodon macularis
# Cyprinodontidae
# tell the Order of  Cyprinodon macularis
# Cyprinodontiformes
# tell the Family of  Cyprinodon pisterti
# Cyprinodontidae
# tell the Order of  Cyprinodon pisterti
# Cyprinodontiformes
# tell the Family of  NA
# 
# tell the Order of  NA
# 
# tell the Family of  Siphateles bicolor mohavensis
# Leuciscidae
# tell the Order of  Siphateles bicolor mohavensis
# Cypriniformes
# 
# tell the Family of  Gila coriacea
# Cyprinidae
# tell the Order of  Gila coriacea
# Cypriniformes
#
# tell the Family of  Pantosteus plebeius
# Catostomidae
# tell the Order of  Pantosteus plebeius
# Cypriniformes



# Cyprinidon macularis
## 
# res_phylo <- FishPhyloMaker(data = taxon_data$Taxon_data_FishPhyloMaker,
#                             insert.base.node = TRUE, 
#                             return.insertions = TRUE, 
#                             progress.bar = TRUE)

res_phylo2 <- FishPhyloMaker(data = taxon_data2$Taxon_data_FishPhyloMaker,
                            insert.base.node = TRUE, 
                            return.insertions = TRUE, 
                            progress.bar = TRUE)



# The output has two objects, a phylogenetic tree that can be directly plot with the following code:
plot(res_phylo2$Phylogeny, cex = 0.4, type = "fan")

plot(res_phylo2$Phylogeny, cex = 0.5, type = "fan")



# And a data frame indicating at which level the species was inserted (one of the six categories detailed above).
res_phylo2$Insertions_data

## library
library(ggtree)
library(phytools)

# phylogeny to use
tree.arid <- res_phylo2$Phylogeny

tree.arid <- ape::makeNodeLabel(tree.arid)
phylo <- tree.arid

# trait data
danger<-read.csv("Data/fishstatus2.csv",header=TRUE)
nrow(danger)
nrow(dplyr::distinct(danger))
danger2 <- dplyr::distinct(danger, X, .keep_all = TRUE)
row.names(danger2) <- danger2[,1]
danger3 <- danger2[,-1]

# set trait example
# svl<-read.csv("svl.csv",header=TRUE,row.names=1)
# svl<-setNames(svl[,1],rownames(svl))
# obj<-contMap(anole.tree,svl,plot=FALSE)
# obj<-setMap(obj,invert=TRUE)
# plot(obj,fsize=c(0.4,1),outline=FALSE,lwd=c(3,7),leg.txt="log(SVL)")

IUCN<-setNames(danger3[,2],rownames(danger3))
obj<-contMap(phylo,IUCN,plot=FALSE)
obj<-setMap(obj,invert=TRUE)
plot(obj,fsize=c(0.4,1),outline=FALSE,lwd=c(3,7),leg.txt="log(SVL)")

# different lengths, remove the taxa not in tree
remove_taxa <- setdiff(phylo$tip.label, danger[,1])
# > remove_taxa 
# [1] "Hubbsina_turneri"         "Squalomugil_nasutus"      "Awaous_banana"            "Rhonciscus_crocro"       
# [5] "Pomadasys_argenteus"      "Phoxinus_grumi"           "Rhinichthys_cobitis"      "Catostomus_discobolus"   
# [9] "Catostomus_clarkii"       "Catostomus_santaanae"     "Catostomus_platyrhynchus" "Neoarius_graeffei"       
# [13] "Bagre_panamensis" 

# pruned tree
pruned_tree <- drop.tip(phylo, remove_taxa)
pruned_tree$tip.label


# eel.tree<-read.tree("elopomorph.tre")
# eel.data<-read.csv("elopomorph.csv",row.names=1)
# fmode<-as.factor(setNames(eel.data[,1],rownames(eel.data)))
# dotTree(eel.tree,fmode,colors=setNames(c("blue","red"),
# #                                        c("suction","bite")),
# ftype="i",fsize=0.7)

# how to plot with thiaminase presence/absence ----
# fish.tree<-read.tree("data/fishorder_skeletal.tre")
# fish.data<-read.csv("data/OrderPresAbsNA.csv",row.names=1)
# fmode<-as.factor(setNames(fish.data[,1],rownames(fish.data)))
# dotTree(drop.tip(fish.tree, no_data),
#         fmode,
#         colors=setNames(c("white","red"),
#                         c("absent","present")),
#         legend = FALSE,
#         edge.width = 1,
#         ftype="i",
#         fsize=0.55,
#         mar = c(5.1, 4.1, 1.1, 1.1), cex = 0.55)
# axis(1)

fish.data<-read.csv("Data/fishstatus2.csv", row.names = 1)

mode <- as.factor(setNames(fish.data[,2], rownames(fish.data)))
dotTree(pruned_tree, x = mode, colors = setNames(c("blue", "red"),
                                         c("none", "danger")), 
                                       ftype = "i", fsize = 0.1)

# eel.trees<-make.simmap(eel.tree,fmode,nsim=100)
danger.trees <- make.simmap(pruned_tree, mode, nsim = 100)

# stochastic mapping ----
smap.trees <- make.simmap(pruned_tree, mode, 
                          model = "ER", nsim = 500)
summary(smap.trees)

obj <-
  densityMap(smap.trees,
             states = c("none", "danger"),
             plot = FALSE)
plot(
  obj,
  type = "fan",
  lwd = 1,
  outline = TRUE,
  fsize = c(0.5, 0.9),
  legend = 50,
  invert = TRUE
)
pdf('figures/PhylogenyDanger.pdf', height = 11, width = 11)
cols <- setNames(c("red", "blue"), c("danger", "none"))
plot(
  obj,
  type = "fan",
  lwd = 1,
  outline = TRUE,
  fsize = c(0.5, 0.9),
  legend = 50,
  invert = TRUE
)
legend("bottomright", c("danger", "none"),
       pch = 21, pt.bg = cols, pt.cex = 2)
dev.off()

x <- new("IUCN", 
         trees = phylo)
plot.base <-
  ggtree(smap.trees, layout = "circular") + geom_tiplab2(size = 2, aes(colors = mode))
print(plot.base)

library(ggplot2)
library(ggtree)

info <- read.csv("Data/fishstatus.csv")

allarid$danger <-
  ifelse(allarid$IUCNstatus2 == "LC" |
           allarid$IUCNstatus2 == "NE", 0, 1)

p <- ggtree(phylo, layout = "circular") %<+% allarid + xlim(-.1, 5)
p2 <- p + geom_tiplab(offset = 0.2) +
  geom_tiplab(aes(color = danger)) + 
  # scale_colorbrewer() +
  theme(legend.position = "right")
print(p2)



## another attempt
## Load needed packages for this blogpost
library(phytools)
library(ggtree)
library(tidyverse)
library(scico)
library(viridisLite)

## Load anole tree
anole.tree <- read.tree("http://www.phytools.org/eqg2015/data/anole.tre")

## Load anole trait data, extract snout-vent-length (svl) as named vector
# svl <- read_csv("http://www.phytools.org/eqg2015/data/svl.csv") %>%
#   mutate(svl = set_names(svl, species)) %>%
#   pull(svl)

library(magrittr)

stat <- allarid %>%
  mutate(IUCN = set_names(IUCNstatus, X)) %>%
  pull(IUCN)

library(ggimage)
library(ggtree)
library(TDbook)

# load `tree_boots`, `df_tip_data`, and `df_inode_data` from 'TDbook'
p <- ggtree(phylo, layout = "circular") %<+% allarid + xlim(-.1, 4)
p2 <- p + geom_tiplab(offset = .6, hjust = .5) +
  geom_tippoint(aes(color = endemic)) + 
  theme(legend.position = "right") + 
  geom_tiplab2(size = 2)

p <- ggtree(phylo, layout = "circular") + geom_tiplab2(size = 3)
p

p2 %<+% df_inode_data + 
  geom_label(aes(label = vernacularName.y, fill = posterior)) + 
  scale_fill_gradientn(colors = RColorBrewer::brewer.pal(3, "YlGnBu"))

p %<+% danger2 +
  geom_tiplab2()

# Plot with default color scheme
# contmap_obj <- contMap(anole.tree, svl, plot = FALSE)
contmap_obj <- contMap(phylo, stat, plot = FALSE)

plot(
  contmap_obj, 
  type="fan", 
  legend = 0.7*max(nodeHeights(anole.tree)),
  fsize = c(0.5, 0.7))

## merge data
# get danger fish
head(allarid)

library(phyloseq)
library(ggjoy)
library(dplyr)
library(ggtree)

mergedGP <- merge_samples(phylo, allarid)
mergedGP <- rarefy_even_depth(mergedGP,rngseed=394582)
mergedGP <- tax_glom(mergedGP,"Order") 


PR.PG <- plot1 + geom_hilight(data = df.phylo, aes(node = node.number, fill = Fam.names), 
                              alpha = .6) +
  scale_fill_viridis(discrete = T, name = "Family names")











## plot tree
library(ape)
library(phytools)

# read data from http://fishtreeoflife.org/taxonomy
my.tree <- read.tree('Data/fishorder_skeletal.tre')

# 
# pairwise taxa-taxa distance matrix
d <- cophenetic(my.tree)
d


# compute total tree height
h <- max(nodeHeights(my.tree))


# plot tree
plot(my.tree, edge.width = 1)
plot(my.tree, font = 1, cex = 0.45)

# generate sampling points
interval <- 50 # every 30 million years
dd <- seq(h,0, by =-interval)

# plot with millions of years
plotTree(my.tree, mar = c(5.1, 4.1, 1.1, 1.1), cex = 0.35)
axis(1)
xlab("Millions of years")
abline(v = dd, lty = "dotted", col = "grey")

# trim a phylogeny
## Orders
# drop.tip
no_data <- c("Acanthuriformes", "Alepocephaliformes",
             "Argentiniformes", "Ateleopodiformes",
             "Beryciformes", "Ephippiformes",
             "Galaxiiformes", "Hiodontiformes",
             "Istiophoriformes", "Lampridiformes",
             "Lepidogalaxiiformes", "Myctophiformes",
             "Notacanthiformes", "Pempheriformes",
             "Percopsiformes", "Pholidichthyiformes",
             "Polymixiiformes", "Spariformes",
             "Stomiatiformes", "Stylephoriformes",
             "Uranoscopiformes", "Zeiformes")
plot(drop.tip(fish.tree, no_data))

# pruned tree
my.tree$tip.label

# taxa to keep
keep_taxa <- c("Polypteriformes", "Acipenseriformes",
               "Lepisosteiformes", "Amiiformes",
               "Elopiformes", "Anguilliformes",
               "Albuliformes", "Osteoglossiformes",
               "Clupeiformes", "Gonorynchiformes",
               "Cypriniformes", "Gymnotiformes",
               "Siluriformes", "Characiformes",
               "Salmoniformes", "Esociformes",
               "Osmeriformes", "Aulopiformes",
               "Gadiformes", "Holocentriformes",
               "Ophidiiformes", "Batrachoidiformes",
               "Labriformes", "Tetraodontiformes",
               "Lophiiformes", "Incertae_sedis_in_Eupercaria",
               "Perciformes", "Centrarchiformes",
               "Gobiiformes", "Kurtiformes",
               "Syngnathiformes", "Pleuronectiformes",
               "Incertae_sedis_in_Carangaria", 
               "Carangiformes", "Mugiliformes",
               "Incertae_sedis_in_Ovalentaria",
               "Cichliformes", "Scombriformes",
               "Atheriniformes", "Cyprinodontiformes",
               "Beloniformes", "Synbranchiformes")

keep_taxa2 <- taxon_data2$Taxon_data_FishPhyloMaker$o
keep_taxa3 <- na.omit(keep_taxa2)
keep_taxa3 <- unique(keep_taxa3)

# check to see which taxa are being removed
setdiff(my.tree$tip.label, keep_taxa3)      

# remove
remove_taxa <- setdiff(my.tree$tip.label, keep_taxa3)

# pruned tree
pruned_tree <- drop.tip(my.tree, remove_taxa)
pruned_tree$tip.label

plot(pruned_tree)

as.data.frame(pruned_tree$tip.lab)

# get tree statistics ----
summary(pruned_tree)

# how to plot with thiaminase presence/absence ----
fish.tree<-read.tree("data/fishorder_skeletal.tre")
fish.data<-read.csv("data/OrderPresAbsNA.csv",row.names=1)
fish.data <- read.csv("Data/orderdanger.csv")

fmode<-as.factor(setNames(fish.data[,1],rownames(fish.data)))
fmode<-setNames(orderdanger$prop.dang,nm = as.factor(orderdanger$o))
dotTree(drop.tip(fish.tree, no_data),
        fmode,
        colors=setNames(c("white","red"),
                        c("absent","present")),
        legend = FALSE,
        edge.width = 1,
        ftype="i",
        fsize=0.55,
        mar = c(5.1, 4.1, 1.1, 1.1), cex = 0.55)
axis(1)

## anova ----
# won't work because response is yes/no
phylANOVA(tree = pruned_tree, x= pruned_tree$tip.label, y = fmode, nsim = 200, posthoc = TRUE, p.adj = "holm")

# make simulation of probability of thiaminase ----
# equal probability across the whole tree? LOL
# thia.trees<-make.simmap(drop.tip(fish.tree, no_data),
#                         fmode,
#                         nsim=200)
# obj<-densityMap(thia.trees,states=c("present","absent"),plot=FALSE)
# plot(obj,lwd=4,outline=TRUE,fsize=c(0.7,0.9),legend=50)

# stochastic mapping ----
smap.trees <- make.simmap(pruned_tree, fmode, 
                          model = "ER", nsim = 500)
summary(smap.trees)

pdf('figures/order_phylogeny.pdf', height = 8, width = 6)
cols <- setNames(c("black", "white"), c("threatened", "not a concern"))
plot(summary(smap.trees), colors = cols)
legend("topleft", c("present", "absent"),
       pch = 21, pt.bg = cols, pt.cex = 2)
dev.off()