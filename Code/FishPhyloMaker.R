# Phylomaker

# devtools::install_github("GabrielNakamura/FishPhyloMaker", ref = "main")
library(FishPhyloMaker)
library(rfishbase)
library(curl)

# example
data(neotropical_comm)
data_comm <- neotropical_comm[, -c(1, 2)] # removing latitude and longitude

# load traits data
allarid <- read.csv("Data/fishstatus.csv", header = TRUE)

# make a danger fish vector
allarid$danger <-
  ifelse(allarid$IUCNstatus2 == "LC" |
           allarid$IUCNstatus2 == "NE", 0, 1)

# # make a vector of fish names
fish <- allarid[,1]

# # replace space with underscore
fish2 <- gsub(" ", "_", fish)
allarid$s <- gsub(" ", "_", allarid$GenusSpecies)

# # what sort of data are they?
# str(fish)
# fish2 <- as.factor(fish)

# # keep only unique
fish3 <- unique(fish)

# finds family and order of species in Fishbase
# example
taxon_data <- FishTaxaMaker(data_comm, allow.manual.insert = TRUE)

# our data - use in another 
taxon_data2 <- FishTaxaMaker(fish3, allow.manual.insert = TRUE)
taxon_data2 <- FishTaxaMaker(fish3, allow.manual.insert = FALSE)

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
# tell the Family of  Oncorhynchus clarkii henshawi
# Salmonidae
# tell the Order of  Oncorhynchus clarkii henshawi
# Salmoniformes
# tell the Family of  Oncorhynchus clarkii utah
# Salmonidae
# tell the Order of  Oncorhynchus clarkii utah
# Salmoniformes
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
# tell the Family of  NA
# 
# tell the Order of  NA
# 
# tell the Family of  NA
# 
# tell the Order of  NA
# 
# tell the Family of  NA
# 
# tell the Order of  NA
# 
# tell the Family of  NA
# 
# tell the Order of  NA
# 
# tell the Family of  NA
# 
# tell the Order of  NA
# 
# tell the Family of  Gila coriacea
# Cyprinidae
# tell the Order of  Gila coriacea
# Cypriniformes
# tell the Family of  NA
# 
# tell the Order of  NA
# 
# tell the Family of  NA
# 
# tell the Order of  NA
# 
# tell the Family of  NA
# 
# tell the Order of  NA
# 
# tell the Family of  NA
# 
# tell the Order of  NA
# 
# tell the Family of  Pantosteus plebeius
# Catostomidae
# tell the Order of  Pantosteus plebeius
# Cypriniformes
# tell the Family of  NA
# 
# tell the Order of  NA
# 
# tell the Family of  NA
# 
# tell the Order of  NA
# 
# tell the Family of  NA
# 
# tell the Order of  NA
# 
# tell the Family of  NA
# 
# tell the Order of  NA


# Cyprinidon macularis
## 
res_phylo <- FishPhyloMaker(data = taxon_data$Taxon_data_FishPhyloMaker,
                            insert.base.node = TRUE, 
                            return.insertions = TRUE, 
                            progress.bar = TRUE)

res_phylo2 <- FishPhyloMaker(data = taxon_data2$Taxon_data_FishPhyloMaker,
                            insert.base.node = TRUE, 
                            return.insertions = TRUE, 
                            progress.bar = TRUE)



# The output has two objects, a phylogenetic tree that can be directly plot with the following code:
plot(res_phylo2$Phylogeny, cex = 0.7)

# And a data frame indicating at which level the species was inserted (one of the six categories detailed above).
res_phylo2$Insertions_data

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
